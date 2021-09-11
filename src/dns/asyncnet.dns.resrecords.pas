unit asyncnet.dns.resrecords;
// DNS implementation according to RFC 1035
// See: https://datatracker.ietf.org/doc/html/rfc1035

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sockets;

type
  EMalformedResRecord = class(Exception);
  EOutOfBounds = class(Exception);
  EInvalidDomainName = class(Exception);
  
  // from http://www.iana.org/assignments/dns-parameters:
  (* JS code for extracting
   * let tb = tb = document.getElementById("table-dns-parameters-4").children[1];
   * let str = "";
   * for (let ele of tb.children) {
   *   if (ele.innerText.includes("1035") || ele.innerText.includes("3569")) {
   *     str += "rrt"+ele.children[0].innerText + " = " + ele.children[1].innerText + ",\n";
   *   }
   * }
   * console.log(str);
   *)
  // Only RFC 1035 and 3596 (for IPv6 support)
  // See: https://datatracker.ietf.org/doc/html/rfc1035#section-3.2.2
  TRRType = (rrtA = 1,
             rrtNS = 2,
             rrtMD = 3,
             rrtMF = 4,
             rrtCNAME = 5,
             rrtSOA = 6,
             rrtMB = 7,
             rrtMG = 8,
             rrtMR = 9,
             rrtNULL = 10,
             rrtWKS = 11,
             rrtPTR = 12,
             rrtHINFO = 13,
             rrtMINFO = 14,
             rrtMX = 15,
             rrtTXT = 16,
             rrtAAAA = 28,
             // QType only valid in requests
             rrtAXFR = 252,
             rrtMAILB = 253,
             rrtMAILA = 254,
             rrtAny = 255);


  TRRClass = (rrcIN = 1,
              rrcCS = 2,
              rrcCH = 3,
              rrcHS = 4,
              // QClass only valid in requests
              rrcAny = 255);

    // see: https://datatracker.ietf.org/doc/html/rfc1035#section-3.2
    PResourceRecordHeader = ^TResourceRecordHeader;
    TResourceRecordHeader = packed record
      TypeField: Word;
      ClassField: Word;
      TTLField: Integer;
      LengthField: Word;
    end;

    TResourceRecordInfo = record
      // Raw memory
      // Start and end of the whole message this RR comes from (required for compression)
      MessageStartPtr: Pointer;
      MessageEndPtr: Pointer;
      // Start and end of this RRs memory region
      RRStartPtr: Pointer;
      RREndPtr: Pointer;
      // Pointer to the Data section
      DataPtr: Pointer;
      // Parsed data
      Name: String;
      RRType: TRRType;
      RRClass: TRRClass;
      TTL: Integer;
      DataLength: SizeInt;
    end;

    // Resource Record Data types
    // See: https://datatracker.ietf.org/doc/html/rfc1035#section-3.3
    // All of these records own their data through managed types
    // This makes them, once parsed, independent of the original message memory
    TCNAMEData = String;

    THINFOData = record
      CPU: String;
      OS: String;
    end;

    TMBData = string;
    TMDData = string;
    TMFData = string;
    TMGData = string;

    TMINFOData = record
      RMailBX: string;
      EMailBX: string;
    end;

    TMRData = string;

    TMXData = record
      Preference: Word;
      Exchange: String;
    end;

    TNULLData = TBytes;

    TNSData = string;

    TPTRData = string;

    TSOAData = record
      MName: String;
      RName: String;
      Serial: Cardinal;
      Refresh: Integer;
      Retry: Integer;
      Expire: Integer;
      Minimum: Cardinal;
    end;

    TTXTData = string;

    TAData = TInAddr;

    TWKSData = record
      Address: TInAddr;
      Protocol: Byte;
      Bitmap: TBytes;
    end;

    // IPv6: https://www.rfc-editor.org/rfc/rfc3596.html#section-2.1
    TAAAAData = TIn6Addr;

    generic TResourceRecord<TData> = record
      Name: String;
      RRType: TRRType;
      RRClass: TRRClass;
      TTL: Integer;
      Data: TData;
    end;

    TCNAMERecord = specialize TResourceRecord<TCNAMEData>;
    THINFORecord = specialize TResourceRecord<THINFOData>;
    TMBRecord = specialize TResourceRecord<TMBData>;
    TMDRecord = specialize TResourceRecord<TMDData>;
    TMFRecord = specialize TResourceRecord<TMFData>;
    TMGRecord = specialize TResourceRecord<TMGData>;
    TMINFORecord = specialize TResourceRecord<TMINFOData>;
    TMRRecord = specialize TResourceRecord<TMRData>;
    TMXRecord = specialize TResourceRecord<TMXData>;
    TNULLRecord = specialize TResourceRecord<TNULLData>;
    TNSRecord = specialize TResourceRecord<TNSData>;
    TPTRRecord = specialize TResourceRecord<TPTRData>;
    TSOARecord = specialize TResourceRecord<TSOAData>;
    TTXTRecord = specialize TResourceRecord<TTXTData>;
    TARecord = specialize TResourceRecord<TAData>;
    TWKSRecord = specialize TResourceRecord<TWKSData>;
    TAAAARecord = specialize TResourceRecord<TAAAAData>;

const
  // Limitations according to https://datatracker.ietf.org/doc/html/rfc1035#section-2.3.4
  MaxLabelSize = 63;
  MaxNameSize = 255;

  // Some limitations for the implementation of recursive functions
  MaxRecursionDepth = 10;

function IsValidDomainName(const AName: String): Boolean;

// Reads the neccesary data from a resource record
// MessageStart: pointer to the first byte of the message (for compression)
// MessageEnd: pointer to the last byte of the message (for sanity checks)
// Offset: The offset of the RR within this message
function ReadResourceRecord(RecordStart, MessageStart, MessageEnd: Pointer): TResourceRecordInfo;

function WriteDomainName(const AName: String; Buffer: PChar; BufferEnd: Pointer): Pointer;
function ReadDomainName(NameData: PByte; MessageStart, MessageEnd: Pointer;
                        EndOfName: PPointer = nil; RecursionDepth: Integer = MaxRecursionDepth): String;

// Resource Record Parsing
function ParseCNAMERecord(constref ARecord: TResourceRecordInfo): TCNAMERecord;
function ParseHINFORecord(constref ARecord: TResourceRecordInfo): THINFORecord;
function ParseMBRecord(constref ARecord: TResourceRecordInfo): TMBRecord;
function ParseMDRecord(constref ARecord: TResourceRecordInfo): TMDRecord;
function ParseMFRecord(constref ARecord: TResourceRecordInfo): TMFRecord;
function ParseMGRecord(constref ARecord: TResourceRecordInfo): TMGRecord;
function ParseMINFORecord(constref ARecord: TResourceRecordInfo): TMINFORecord;
function ParseMRRecord(constref ARecord: TResourceRecordInfo): TMRRecord;
function ParseMXRecord(constref ARecord: TResourceRecordInfo): TMXRecord;
function ParseNULLRecord(constref ARecord: TResourceRecordInfo): TNULLRecord;
function ParseNSRecord(constref ARecord: TResourceRecordInfo): TNSRecord;
function ParsePTRRecord(constref ARecord: TResourceRecordInfo): TPTRRecord;
function ParseSOARecord(constref ARecord: TResourceRecordInfo): TSOARecord;
function ParseTXTRecord(constref ARecord: TResourceRecordInfo): TTXTRecord;
function ParseARecord(constref ARecord: TResourceRecordInfo): TARecord;
function ParseWKSRecord(constref ARecord: TResourceRecordInfo): TWKSRecord;
function ParseAAAARecord(constref ARecord: TResourceRecordInfo): TAAAARecord;
implementation

// Check if domain name structure accords to https://datatracker.ietf.org/doc/html/rfc1035#section-2.3.1
function IsValidDomainName(const AName: String): Boolean;
var
  LabelStart, i: SizeInt;
begin
  Result := False;
  // Size constraints
  if AName.IsEmpty or (AName.Length > MaxNameSize) then
    Exit;
  LabelStart := 1;
  // Check each character
  for i := 1 to AName.Length do
    case AName[i] of
    '.': // A dot seperates two labels
    begin
      if (i = LabelStart) or (i > LabelStart + MaxLabelSize) then
        Exit; // Previous label empty or to long
      if AName[i - 1] = '-' then
        Exit; // Labels must end with digit or letter
      LabelStart := i + 1; // start of a new label
    end;
    '0'..'9', '-':
      if i = LabelStart then
        Exit; // Labels must start with a letter
    'A'..'Z', 'a'..'z':; // Are always allowed
    otherwise Exit;
    end;
  // Lastly check that we don't end on a dot or a -
  Result := not (AName[AName.Length] in ['.', '-']);
end;


function WriteDomainName(const AName: String; Buffer: PChar; BufferEnd: Pointer
  ): Pointer;
var
  labels: array of String;
  i: SizeInt;
begin
  if not IsValidDomainName(AName) then
    raise EInvalidDomainName.Create('"' + AName +'" is not a valid domain name');
  if Buffer + AName.Length + 2 > BufferEnd then
    raise EOutOfBounds.Create('Domain does not fit buffer');
  labels := AName.Split('.');
  // write each label
  for i:=0 to Length(labels) - 1 do
  begin
    // Write the length
    Buffer^ := chr(labels[i].Length);
    // Copy data right behind length byte
    Move(PChar(labels[i])^, Buffer[1], ord(Buffer^));
    // Move pointer
    Inc(Buffer, ord(Buffer^) +  1);
  end;
  // Zero terminated
  Buffer^ := #0;
  // Return a pointer to the first byte after the written domain name
  Result := Buffer + 1;
end;

// according to https://datatracker.ietf.org/doc/html/rfc1035#section-3.1
// NameData: pointer to the start of the name data
// MessageStart: pointer to the first byte of the message (for compression)
// MessageEnd: pointer to the last byte of the message (for sanity checks)
// EndOfName: Outputs a pointer to the first byte after the name
// RecursionDepth: Parameter to control how often we go recursively for preventing stack overflows
function ReadDomainName(NameData: PByte; MessageStart, MessageEnd: Pointer;
                        EndOfName: PPointer = nil; RecursionDepth: Integer = MaxRecursionDepth): String;
var
  WritePos: SizeInt;
  LabelSize: Byte;
  Offset: SizeInt;
  WriteBuff: PChar;
begin
  Result := '';
  if RecursionDepth = 0 then
    raise EMalformedResRecord.Create('Recursion loop detected');
  SetLength(Result, MaxNameSize + 1); // + 1 for final '.'
  WritePos := 0;
  // use pchar for 0 based access as well as circumvent lazy copy mechanisms
  WriteBuff := @Result[1];
  Offset := -1;
  // Assume that there is at least one byte to read
  // Read until final 0 byte or a pointer but never out of bounds
  while NameData^ > 0 do
  begin
    LabelSize := NameData^;
    // Compression offset found
    // See: https://datatracker.ietf.org/doc/html/rfc1035#section-4.1.4
    if (LabelSize and Byte($C0)) = Byte($C0) then
    begin
      // ensure we don't read out of bounds
      if NameData + 1 > MessageEnd then
        raise EMalformedResRecord.Create('Offset out of bounds');
      // Point to the second half of the offset
      // this is required for correctly setting EndOfName afterwards
      Inc(NameData);
      // Offset are the two next bytes without the leading 2 bits
      Offset := SizeInt(LabelSize and not Byte($C0)) shl 8 + NameData^;
      Break;
    end;
    // Sanity checks:
    if (NameData + LabelSize + 1 > MessageEnd) or // No out of bound reading
       (WritePos + LabelSize > MaxNameSize) or // enforce max domain length
       (LabelSize > MaxLabelSize) then // 01 and 10 start bits are reserved
       raise EMalformedResRecord.Create('Label exceeds size constraints');
    // Copy data to string
    Move(NameData[1], WriteBuff[WritePos], LabelSize);
    // Move read and write head
    Inc(NameData, LabelSize + 1); // +1 for the size byte
    Inc(WritePos, LabelSize + 1); // +1 for the appended '.'
    // append '.' for the next label
    WriteBuff[WritePos - 1] := '.';
  end;
  // Shrink to fit
  SetLength(Result, WritePos);
  // Set EndOfName as the first byte after the current position of NameData
  // as NameData will point to the last byte of the name
  if Assigned(EndOfName) then
    EndOfName^ := NameData + 1;
  if Offset >= 0 then
  begin // on offset concatenate with referenced
    Result += ReadDomainName(MessageStart + Offset, MessageStart, MessageEnd, nil, RecursionDepth  - 1);
    // Sanity check: enforce size limit
    if Result.Length > MaxNameSize then
      raise Exception.Create('Name exceeds maximum size');
  end
  else // ended on 0 byte
    SetLength(Result, Result.Length - 1) // remove last '.'
end;

// Reads the neccesary data from a resource record
// RecordStart: pointer to the first byte of the record
// MessageStart: pointer to the first byte of the message (for compression)
// MessageEnd: pointer to the last byte of the message (for sanity checks)
function ReadResourceRecord(RecordStart, MessageStart, MessageEnd: Pointer
  ): TResourceRecordInfo;
var
  HeaderPtr: PResourceRecordHeader;
begin
  Result := Default(TResourceRecordInfo);
  // Set the parameter
  Result.MessageStartPtr := MessageStart;
  Result.MessageEndPtr := MessageEnd;
  // Get the pointer to the start of the RR
  Result.RRStartPtr := RecordStart;
  // The name field is at most as large as the total record minus the header size
  // Also outputs the first byte after the name which is where the header starts
  Result.Name := ReadDomainName(Result.RRStartPtr, MessageStart,
                             MessageEnd - SizeOf(TResourceRecordHeader),
                             @HeaderPtr);
  if Pointer(HeaderPtr) + SizeOf(TResourceRecordHeader) > MessageEnd then
    raise EMalformedResRecord.Create('Resource record does not fit message');
  // Read the header fields
  Result.RRType := TRRType(NToHs(HeaderPtr^.TypeField));
  Result.RRClass := TRRClass(NToHs(HeaderPtr^.ClassField));
  Result.TTL := NToHl(HeaderPtr^.TTLField);
  Result.DataLength := NToHs(HeaderPtr^.LengthField);
  // Data starts right after the header
  Result.DataPtr := PByte(HeaderPtr) + SizeOf(TResourceRecordHeader);
  // Compute the end of the resource record
  // The end is the last byte of the RR memory
  Result.RREndPtr := Result.DataPtr + Result.DataLength - 1;
  // At last a sanity check if the data section is in bound
  if Result.RREndPtr > MessageEnd then
    raise EMalformedResRecord.Create('Data size does not fit message');
end;

function ReadCharacterString(AData: PByte; MaxLength: SizeInt): String;
var
  DataLen: Byte;
begin
  Result := '';
  DataLen := AData^;
  if DataLen > MaxLength then
    raise EMalformedResRecord.Create('Character string exceeds data size');
  SetLength(Result, DataLen);
  Move(AData[1], PChar(Result)^, DataLen);
end;

generic procedure FillCommonFileds<TRecordType>(constref RecInfo: TResourceRecordInfo;
                                                out ARecord: TRecordType);
begin
  ARecord := Default(TRecordType);
  ARecord.Name := RecInfo.Name;
  ARecord.RRType := RecInfo.RRType;
  ARecord.RRClass := RecInfo.RRClass;
  ARecord.TTL := RecInfo.TTL;
end;

function ParseCNAMERecord(constref ARecord: TResourceRecordInfo): TCNAMERecord;
begin
  specialize FillCommonFileds<TCNAMERecord>(ARecord, Result);
  if ARecord.RRType <> rrtCNAME then
    raise EMalformedResRecord.Create('Not a CNAME record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseHINFORecord(constref ARecord: TResourceRecordInfo): THINFORecord;
begin
  specialize FillCommonFileds<THINFORecord>(ARecord, Result);
  if ARecord.RRType <> rrtHINFO then
    raise EMalformedResRecord.Create('Not a HINFO record');
  if ARecord.DataLength < 2 then
    raise EMalformedResRecord.Create('A HINFO record must be at least 2 bytes large');
  // can be at most n-1 bytes long so OS is 1 byte at least
  Result.Data.CPU := ReadCharacterString(ARecord.DataPtr, ARecord.DataLength - 1);
  Result.Data.OS := ReadCharacterString(ARecord.DataPtr + Result.Data.CPU.Length + 1,
                                   ARecord.DataLength - Result.Data.CPU.Length + 1);
end;

function ParseMBRecord(constref ARecord: TResourceRecordInfo): TMBRecord;
begin 
  specialize FillCommonFileds<TMBRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMB then
    raise EMalformedResRecord.Create('Not a MB record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMDRecord(constref ARecord: TResourceRecordInfo): TMDRecord;
begin
  specialize FillCommonFileds<TMDRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMD then
    raise EMalformedResRecord.Create('Not a MD record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMFRecord(constref ARecord: TResourceRecordInfo): TMFRecord;
begin 
  specialize FillCommonFileds<TMFRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMF then
    raise EMalformedResRecord.Create('Not a MF record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMGRecord(constref ARecord: TResourceRecordInfo): TMGRecord;
begin
  specialize FillCommonFileds<TMGRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMG then
    raise EMalformedResRecord.Create('Not a MG record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMINFORecord(constref ARecord: TResourceRecordInfo): TMINFORecord;
var
  DataPtr: Pointer;
begin
  specialize FillCommonFileds<TMINFORecord>(ARecord, Result);
  if ARecord.RRType <> rrtMINFO then
    raise EMalformedResRecord.Create('Not a MINFO record');
  if ARecord.DataLength < 2 then
    raise EMalformedResRecord.Create('A MINFO record must be at least 2 bytes large');
  DataPtr := ARecord.DataPtr;
  // Can at most go to the RR size -1 so 1 byte is for EMailBX
  Result.Data.RMailBX := ReadDomainName(DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr - 1, @DataPtr);
  Result.Data.EMailBX := ReadDomainName(DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMRRecord(constref ARecord: TResourceRecordInfo): TMRRecord;
begin 
  specialize FillCommonFileds<TMRRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMR then
    raise EMalformedResRecord.Create('Not a MR record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseMXRecord(constref ARecord: TResourceRecordInfo): TMXRecord;
begin
  specialize FillCommonFileds<TMXRecord>(ARecord, Result);
  if ARecord.RRType <> rrtMX then
    raise EMalformedResRecord.Create('Not a MX record');
  if ARecord.DataLength < 3 then
    raise EMalformedResRecord.Create('A MX record must be at least 3 bytes large');
  Result.Data.Preference := NToHs(PWord(ARecord.DataPtr)^);
  Result.Data.Exchange := ReadDomainName(ARecord.DataPtr + SizeOf(Word), ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseNULLRecord(constref ARecord: TResourceRecordInfo): TNULLRecord;
begin 
  specialize FillCommonFileds<TNULLRecord>(ARecord, Result);
  if ARecord.RRType <> rrtNULL then
    raise EMalformedResRecord.Create('Not a NULL record');
  SetLength(Result.Data, ARecord.DataLength);
  Move(PByte(ARecord.DataPtr)^, PByte(Result.Data)^, ARecord.DataLength);
end;

function ParseNSRecord(constref ARecord: TResourceRecordInfo): TNSRecord;
begin  
  specialize FillCommonFileds<TNSRecord>(ARecord, Result);
  if ARecord.RRType <> rrtNS then
    raise EMalformedResRecord.Create('Not a NS record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParsePTRRecord(constref ARecord: TResourceRecordInfo): TPTRRecord;
begin 
  specialize FillCommonFileds<TPTRRecord>(ARecord, Result);
  if ARecord.RRType <> rrtPTR then
    raise EMalformedResRecord.Create('Not a PTR record');
  if ARecord.DataLength = 0 then
    raise EMalformedResRecord.Create('A <domain-name> must be at least 1 byte');
  Result.Data := ReadDomainName(ARecord.DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr);
end;

function ParseSOARecord(constref ARecord: TResourceRecordInfo): TSOARecord;
var
  DataPtr: Pointer;
begin 
  specialize FillCommonFileds<TSOARecord>(ARecord, Result);
  if ARecord.RRType <> rrtSOA then
    raise EMalformedResRecord.Create('Not a SOA record');
  if ARecord.DataLength < 22 then
    raise EMalformedResRecord.Create('A SOA must be at least 22 bytes large');
  DataPtr := ARecord.DataPtr;
  // leave enough room for 20 bytes of integer data and 1 byte for RName
  Result.Data.MName := ReadDomainName(DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr - 21, @DataPtr);
  // leave enough room for 20 bytes of integer data
  Result.Data.RName := ReadDomainName(DataPtr, ARecord.MessageStartPtr, ARecord.RREndPtr - 20, @DataPtr);
  // Read integer data, convert byte order
  Result.Data.Serial := NToHl(PCardinal(DataPtr)[0]);
  Result.Data.Refresh := NToHl(PCardinal(DataPtr)[1]);
  Result.Data.Retry := NToHl(PCardinal(DataPtr)[2]);
  Result.Data.Expire := NToHl(PCardinal(DataPtr)[3]);
  Result.Data.Minimum := NToHl(PCardinal(DataPtr)[4]);
end;

function ParseTXTRecord(constref ARecord: TResourceRecordInfo): TTXTRecord;
var
  DataPtr: Pointer;
begin 
  specialize FillCommonFileds<TTXTRecord>(ARecord, Result);
  Result.Data := '';
  if ARecord.RRType <> rrtTXT then
    raise EMalformedResRecord.Create('Not a TXT record');
  if ARecord.DataLength < 1 then
    raise EMalformedResRecord.Create('A SOA must be at least 1 byte');
  DataPtr := ARecord.DataPtr;
  while DataPtr < ARecord.RREndPtr do
    Result.Data += ReadCharacterString(DataPtr, ARecord.RREndPtr - DataPtr);
end;

function ParseARecord(constref ARecord: TResourceRecordInfo): TARecord;
begin 
  specialize FillCommonFileds<TARecord>(ARecord, Result);
  if ARecord.RRType <> rrtA then
    raise EMalformedResRecord.Create('Not an A record');
  if ARecord.RRClass <> rrcIN then
    raise EMalformedResRecord.Create('A records need to be of class IN');
  if ARecord.DataLength <> 4 then
    raise EMalformedResRecord.Create('A A must be exactly 4 bytes');
  Result.Data.s_addr := htonl(PCardinal(ARecord.DataPtr)^);
end;

function ParseWKSRecord(constref ARecord: TResourceRecordInfo): TWKSRecord;
begin
  specialize FillCommonFileds<TWKSRecord>(ARecord, Result);
  if ARecord.RRType <> rrtWKS then
    raise EMalformedResRecord.Create('Not an WKS record');
  if ARecord.RRClass <> rrcIN then
    raise EMalformedResRecord.Create('WKS records need to be of class IN');
  if ARecord.DataLength < 5 then
    raise EMalformedResRecord.Create('A WKS must be at least 5 bytes long');
  Result.Data.Address.s_addr := htonl(PCardinal(ARecord.DataPtr)^);
  Result.Data.Protocol := PByte(ARecord.DataPtr)[4];
  SetLength(Result.Data.Bitmap, ARecord.DataLength - 5);
  Move(PByte(ARecord.DataPtr)[5], PByte(Result.Data.Bitmap), ARecord.DataLength - 5);
end;

function ParseAAAARecord(constref ARecord: TResourceRecordInfo): TAAAARecord;
begin
  specialize FillCommonFileds<TAAAARecord>(ARecord, Result);
  if ARecord.RRType <> rrtAAAA then
    raise EMalformedResRecord.Create('Not an AAAA record');
  if ARecord.RRClass <> rrcIN then
    raise EMalformedResRecord.Create('AAAA records need to be of class IN');
  if ARecord.DataLength <> 16 then
    raise EMalformedResRecord.Create('A AAAAA must be exactly 16 bytes');
  Move(PByte(ARecord.DataPtr)^, Result.Data, SizeOf(Result.Data));
end;

end.

