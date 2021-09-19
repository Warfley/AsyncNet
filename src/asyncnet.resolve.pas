{$MODE OBJFPC}
{$H+}
Unit asyncnet.resolve;

{ --------------------------------------------------------------------
  Unit for internet domain calls.
  Copyright (C) 2003  Michael Van Canneyt

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 1, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  ------------------------------------------------------------------- }

interface

uses
  sockets,Classes,UriParser;

Type
  THostAddr = in_addr;		
  PHostAddr = ^THostAddr;
  TNetAddr = in_addr;
  PNetAddr = ^TNetAddr;

Type

{ ---------------------------------------------------------------------
    TResolver
  ---------------------------------------------------------------------}

  TResolver = Class (TComponent)
  Private
    FName : String;
    FAliases : TStringList;
    FRaiseOnError : Boolean;
    FLastError: Integer;
    Function GetAlias(Index : Integer) : STring;
    Function GetAliasCount : Integer;
    Function GetAliasSorted : Boolean;
    Procedure SetAliasSorted (Value : Boolean);
  Protected
    Procedure CheckOperation(Msg : String);
    Function NameLookup(Const S : String) : Boolean; virtual;
    Procedure SaveAliases(P : PPChar);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure ClearData; virtual;
    Property ResolvedName : String Read FName;
    Property Aliases [Index : integer ] : string Read GetAlias;
    Property AliasCount : Integer read GetAliasCount;
    Property SortAliases : Boolean Read GetAliasSorted Write SetAliasSorted;
    Property RaiseOnError : Boolean Read FRaiseOnError Write FRAiseOnError;
    Property LastError : Integer Read FlastError;
  end;

{ ---------------------------------------------------------------------
    THostResolver
  ---------------------------------------------------------------------}

  THostResolver = Class(TResolver)
  Private
    FHostAddress : THostAddr;
    FAddressCount : Integer;
    FAddresses : PHostAddr;
    Function GetAddress (Index : Integer) : THostAddr;
    Function GetNetAddress (Index : Integer) : THostAddr;
    Function GetNetHostAddress : THostAddr;
    Function GetAsString : String;
    Procedure SaveHostEntry (Entry : Pointer);
  Public
    Procedure ClearData; Override;
    Function NameLookup(Const S : String) : Boolean; override;
    Function AddressLookup(Const S : String) : Boolean; virtual;
    Function AddressLookup(Const Address : THostAddr) : Boolean; virtual;
    Property HostAddress : THostAddr Read FHostAddress;
    Property NetHostAddress : THostAddr Read GetNetHostAddress;
    Property AddressAsString : String Read GetAsString;
    Property AddressCount : Integer Read FAddressCount ;
    Property Addresses [Index : Integer] : ThostAddr Read GetAddress;
    Property NetAddresses [Index : Integer] : ThostAddr Read GetNetAddress;
  end;

{ ---------------------------------------------------------------------
    TNetResolver
  ---------------------------------------------------------------------}

  TNetResolver = Class(TResolver)
  Private
    FNetAddress : TNetAddr;
    FAddrType : Integer;
    Function  GetAsString : String;
    Procedure SaveNetEntry(Entry : Pointer);
    Function GetNetAddress : TNetAddr;
  Public
    Procedure ClearData; override;
    Function NameLookup(Const S : String) : boolean; override;
    Function AddressLookup(Const S : String) : Boolean; virtual;
    Function AddressLookup(Const Address : TNetAddr) : Boolean; virtual;
    Property NetAddress : TNetAddr Read FNetAddress;
    Property NetNetAddress : TNetAddr Read GetNetAddress;
    Property AddressAsString : String Read GetAsString;
    Property AddressType : Integer Read FAddrType;
  end;

{ ---------------------------------------------------------------------
    TServiceResolver
  ---------------------------------------------------------------------}

  TServiceResolver = Class(TResolver)
  private
    FProtocol : String;
    FPort : Integer;
    Procedure SaveServiceEntry(Entry : Pointer);
    Function GetNetPort : Integer ;
  public
    Procedure ClearData; override;
    Function NameLookup (Const S : String) : boolean; override;
    Function NameLookup (Const S,Proto : String) : Boolean;
    Function PortLookup (APort : Longint; Proto: string) : Boolean;
    Property Protocol : String Read FProtocol;
    Property Port : Integer Read FPort;
    Property NetPort : Integer Read GetNetPort;
  end;

  TURIParser = Class(TComponent)
  Private
    FActive : Boolean;
    FProtocol: String;
    FUsername: String;
    FPassword: String;
    FHost: String;
    FPort: Word;
    FPath: String;
    FDocument: String;
    FParams: String;
    FBookmark: String;
    FURI : String;
  Protected
    Procedure SetElement (Index : Integer; Value : String);Virtual;
    Function GetElement(Index : Integer) : String;
    Procedure SetPort(Value : Word);
    Procedure SetURI(Value : String);
  Public
    Procedure Clear;
    Procedure ParseUri(AURI : String);
    Function ComposeURI : String;
  Published
    Property Port: Word  Read FPort Write SetPort;
    Property Protocol: String Index 0 Read GetElement Write SetElement;
    Property Username: String Index 1 Read GetElement Write SetElement;
    Property Password: String Index 2 Read GetElement Write SetElement;
    Property Host: String     Index 3 Read GetElement Write SetElement;
    Property Path: String     index 4 Read GetElement Write SetElement;
    Property Document: String index 5 read GetElement Write SetElement;
    Property Params: String   Index 6 read GetElement Write SetElement;
    Property Bookmark: String Index 7 Read GetElement Write SetElement;
    Property URI : String Read FURI write SetURI;
    Property Active : Boolean Read FActive Write FActive;
  end;


Resourcestring
  SErrHostByName = 'Host by name';
  SErrHostByAddr = 'Host by address';
  SErrNetByName  = 'Net by name';
  SErrServByName = 'Service by name';
  SErrServByPort = 'Service by port';

Implementation

{ ---------------------------------------------------------------------
    Include system dependent stuff.
  ---------------------------------------------------------------------}

uses asyncnet.netdb;

{ ---------------------------------------------------------------------
  TResolver
  ---------------------------------------------------------------------}

Constructor TResolver.Create(AOwner : TComponent);

begin
  Inherited;
  FAliases:=TstringList.Create;
end;

Destructor TResolver.Destroy;

begin
  ClearData;
  FAliases.Free;
end;

Procedure TResolver.ClearData;

begin
  FName:='';
  FAliases.Clear;
end;

Function TResolver.GetAlias(Index : Integer) : STring;

begin
  Result:=FAliases[Index];
end;

Function TResolver.GetAliasCount : Integer;

begin
  Result:=FAliases.Count;
end;

Function TResolver.GetAliasSorted : Boolean;

begin
  Result:=FAliases.Sorted;
end;

Procedure TResolver.SetAliasSorted (Value : Boolean);

begin
  FAliases.Sorted:=Value;
end;

Procedure TResolver.CheckOperation(Msg : String);

begin
end;

Function TResolver.NameLookup(Const S : String) : Boolean;

begin
  ClearData;
  FName:=S;
  Result:=True;
end;

Procedure TResolver.SaveAliases(P : PPChar);

Var
  I : Integer;

begin
  If (P<>Nil) then
    begin
    I:=0;
    While P[I]<>Nil do
      begin
      FAliases.Add(StrPas(P[I]));
      Inc(I);
      end;
    end;
end;


{ ---------------------------------------------------------------------
  THostResolver
  ---------------------------------------------------------------------}

Function THostResolver.GetAddress (Index : Integer) : THostAddr;

begin
  If (Index>=0) and (Index<FAddressCount) then
    Result:=FAddresses[Index];
end;

Function THostResolver.GetAsString : String;

begin
  Result:=HostAddrToStr(FHostAddress);
end;

Procedure THostResolver.ClearData;

begin
  Inherited;
  FHostAddress:=NoAddress;
  If FAddressCount<>0 Then
    FreeMem(FAddresses);
  FAddressCount:=0;
  FAddresses:=Nil;
end;

Function THostResolver.AddressLookup(Const S : String) : Boolean;

begin
  Result:=AddressLookup(StrToHostAddr(S));
end;

Function THostResolver.NameLookup (Const S : String) : Boolean;

Var
  H : THostEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    //Result:=GetHostByName(S,H);
    if not Result then
      //Result:=ResolveHostByName(S,H)
    else
      H.Addr:=H.Addr;
    If Result then
      SaveHostEntry(@H);
    end;
end;

Function THostResolver.AddressLookup (Const Address: THostAddr) : Boolean;

Var
  H : THostEntry;

begin
  ClearData;
  //Result:=ResolveHostByAddr(Address,H);
  If Result then
    SaveHostEntry(@H);
end;

Procedure THostResolver.SaveHostEntry(Entry : Pointer);

Var
  PH : ^THostEntry;
  I : Integer;

begin
  PH:=ENtry;
  FName:=PH^.Name;
  //FHostAddress:=NetToHost(PH^.Addr);
  FAddressCount:=1;
  GetMem(FAddresses,SizeOf(THostAddr));
  //FAddresses[0]:=NetToHost(PH^.Addr);
  //FAliases.CommaText:=PH^.Aliases;
end;

Function THostResolver.GetNetAddress (Index : Integer) : THostAddr;

begin
  Result:=HostToNet(Addresses[Index]);
end;

Function THostResolver.GetNetHostAddress : THostAddr;

begin
  Result:=HostToNet(FHostAddress);
end;


{ ---------------------------------------------------------------------
    TNetResolver
  ---------------------------------------------------------------------}

Function TNetResolver.AddressLookup (Const Address: TNetAddr) : boolean;

Var
  N : TNetworkEntry;

begin
  ClearData;
  //Result:=GetNetworkByAddr(Address,N);
  If Result then
    SaveNetEntry(@N);
end;

Function TNetResolver.NameLookup (Const S : String) : Boolean;

Var
  N : TNetworkEntry;

begin
  Result:=Inherited NameLookup(S);
  If Result then
    begin
    //Result:=GetNetworkByName(S,N);
    If Result then
      SaveNetEntry(@N);
    end;
end;

Procedure TNetResolver.SaveNetEntry(Entry : Pointer);

Var
  PN : ^TNetworkEntry;

begin
  PN:=ENtry;
  FName:=PN^.Name;
  //FNetAddress:=NetToHost(PN^.Addr);
  //FAliases.CommaText:=PN^.Aliases;
end;

Function TNetResolver.AddressLookup(Const S : String) : Boolean;

begin
  Result:=AddressLookup(StrToNetAddr(S));
end;


Function TNetResolver.GetAsString : String;

begin
  Result:=HostAddrToStr(FNetAddress);
end;

Function TNetResolver.GetNetAddress : TNetAddr;

begin
  Result:=HostToNet(FNetAddress);
end;


Procedure TNetResolver.ClearData;

begin
  Inherited;
  FNetAddress:=NoAddress;
  FAddrType:=0;
end;

{ ---------------------------------------------------------------------
    TServiceResolver
  ---------------------------------------------------------------------}

Function TServiceResolver.NameLookup (Const S : String) : Boolean;

begin
  Result:=NameLookup(S,'');
end;

Function TServiceResolver.NameLookup (Const S,Proto : String) : Boolean;

Var
  E : TServiceEntry;

begin
  ClearData;
  //Result:=GetServiceByName(S,Proto,E);
  If Result then
    SaveServiceEntry(@E);
end;

Function TServiceResolver.PortLookup (APort: Longint; Proto : String) : Boolean;

Var
  E : TServiceEntry;

begin
  ClearData;
  //Result:=GetServiceByPort(APort,Proto,E);
  If Result then
    SaveServiceEntry(@E);
end;

Procedure TServiceResolver.SaveServiceEntry(Entry : Pointer);

Var
  PE : ^TServiceEntry;

begin
  PE:=Entry;
  FName:=PE^.Name;
  FPort:=PE^.Port;
  FProtocol:=PE^.Protocol;
  //FAliases.CommaText:=PE^.Aliases;
end;

Procedure TServiceResolver.ClearData;

begin
  Inherited;
  FProtocol:='';
  FPort:=0;
end;

Function TServiceResolver.GetNetPort : Integer;

begin
  //Result:=ShortHostToNet(FPort);
end;

{ ---------------------------------------------------------------------
    TURIParser
  ---------------------------------------------------------------------}


Procedure TURIParser.SetElement (Index : Integer; Value : String);

begin
 Case index of
   0  : FProtocol := Value;
   1  : FUsername := Value;
   2  : FPassword := Value;
   3  : FHost     := Value;
   4  : FPath     := Value;
   5  : FDocument := Value;
   6  : FParams   := Value;
   7  : FBookmark := Value;
  else
  end;
  If FActive and not (csLoading in ComponentState) then
    FURI:=ComposeURI;
end;

Function  TURIParser.GetElement(Index : Integer) : String;

begin
  Case Index of
  0  : Result := FProtocol;
  1  : Result := FUsername;
  2  : Result := FPassword;
  3  : Result := FHost    ;
  4  : Result := FPath    ;
  5  : Result := FDocument;
  6  : Result := FParams  ;
  7  : Result := FBookmark;
  else
    Result:='';
  end;
end;

Procedure TURIParser.SetPort(Value : Word);

begin
  FPort:=Value;
  If FActive and not (csLoading in ComponentState) then
    FURI:=ComposeURI;
end;

Procedure TURIParser.SetURI(Value : String);

begin
  If Active and not (csLoading in ComponentState) then
    begin
    Clear;
    ParseUri(Value);
    end;
  FURI:=Value;
end;

Procedure TURIParser.Clear;

begin
   FProtocol :='';
   FUsername :='';
   FPassword :='';
   FHost     :='';
   FPort     :=0;
   FPath     :='';
   FDocument :='';
   FParams   :='';
   FBookmark :='';
   FURI      :='';
end;

Procedure TURIParser.ParseUri(AURI : String);

Var
  U : TURI;

begin
  U:=UriParser.ParseURI(AUri);
  FProtocol := u.Protocol;
  FUsername := u.Username;
  FPassword := u.Password;
  FHost     := u.Host    ;
  FPort     := u.Port    ;
  FPath     := u.Path    ;
  FDocument := u.Document;
  FParams   := u.Params  ;
  FBookmark := u.Bookmark;
end;


Function  TURIParser.ComposeURI : String;

var
  U : TURI;

begin
  U.Protocol := FProtocol;
  U.Username := FUsername;
  U.Password := FPassword;
  U.Host     := FHost    ;
  U.Port     := FPort    ;
  U.Path     := FPath    ;
  U.Document := FDocument;
  U.Params   := FParams  ;
  U.Bookmark := FBookmark;
  Result:=EncodeUri(U);
end;

end.
