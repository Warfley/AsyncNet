# AsyncNet
This package contains asynchronous networking functionality for STAX.
It is based on the fcl-net package, but with large reworks and restructuring.

The goal is to provide a complete networking library which also makes proting existing applications using the fcl-net package easy.
It works on Linux (possibly more Unix systems) and Windows.

## Units
* asyncnet.sockets: This unit provides functionality for asynchronous TCP and UDP communications over both IPv4 and IPv6.
* asyncnet.netdb: This unit allows for accessing the hosts, services, networks and protocols database of the system (located in the etc directory).
* asyncnet.dns & asyncnet.dns.resrecords: These units provide a general DNS client implementation for sending request queries and parsing the responses
* asyncnet.resolve: This unit makes use of asyncnet.dns and asyncnet.netdb to provide DNS resolution using the system settings (DNS servers, retries, etc)
