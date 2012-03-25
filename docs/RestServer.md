# Representational State Transfer

## Session State

- each HTTP request should carry enough information by itself for its recipient
  to process it 
- whole idea of session is to make stateful applications using stateless
  protocol (HTTP) and dumb client (web browser), by maintaining the state on
server's side 
- Truly RESTful application would maintain state on client's side, sending all
  the necessary variables over by HTTP, preferably in the URI

### Concept

- clients initiate requests and servers process them to return appropriate
  responses
- requests and responses are build around transfer of representations (e.g,
  JSON, XML) of resources
- representation captures current or intended state of resource

- client sends request when ready to transition to new state and is considered
  in transition while one or more requests are outstanding

### Constraints

#### Client Server

- separated by uniform interface (separation of concerns)

#### Stateless

- no client context is stored on server between requests
- each client request contains all information necessary to service it and any
  session state is held by the client
- server can be stateful, server-side state is addressable by URL as resource

#### Cacheable

- clients can cache responses, so responses need to define themselves as
  cacheable or not (e.g, to prevent clients from reusing stale data) 
- caching can eliminate some client-server interactions and improve scalability
  and performance

#### Layered system

- clients can be connected to server directly or to an intermediary
- intermidiary servers may improve scalability (load-balancing, shared caches)
  and enforce security policies

#### Code on demand

- servers may extend/customize client functionality by transferring executable
  code (e.g., JavaScript)

### Guiding Principles of Uniform Interface

#### Resources

- individual resources are identified in requests e.g., using URIs
- resources are conceptually separate from returned representations (JSON)
- representation of resource held by client has enough information to modify or
  delete resource on server if so permitted

#### Messages

- message includes information to describe how it is to be processed 
- Internet media type (previously MIME type) specifies which parser to invoke 

### Central principle

- resources (sources of specific information) referenced via global identifier (URI)
- user agents and servers communicate via interface (HTTP) in order to
  manipulate these resources by exchanging their representations
- connectors (clients, servers, caches, tunnels) mediate requests (layering)
- application interacts with resources by knowing its identifier and the action
  to perform on it (no need to know about caches, proxies, gateways etc.)
- application understands format of representation (JSON, HTML, XML)


## Hypertext Transfer Protocol

- request-response protocol for client-server
- client submits HTTP request to server
- server provides resources (HTML files) or performs actions on behalf of
  client and sends response
- response contains request status information and may contain requested
  content in message body

- HTTP presume a reliable Transport Layer protocol for host-to-host data
  transfer (TCP)

- HTTP resources are identified and located on network via Uniform Resource
  Identifiers (URIs) or Uniform Resource Locators (URLs) using http or https
[URI schemes](http://en.wikipedia.org/wiki/URI_scheme)

### HTTP Session

- sequence of network request-response transactions
- initiated by client establishing a TCP connection to particular port on
  server (typically port 80)
- server listening on that port waits for client's request message and sends
  back a status line ("HTTP/1.1 200 OK") followed by server message
- body of server message is typically the requested resource, but can be error
  message or other information

### Request methods
 
- HTTP defines nine methods (verbs) indicating desired action to be performed
  on identified resource
- often resource is file or output of an executable residing on server

#### HEAD

- same as GET but without response body, useful to retrieve meta data
  information in response headers

#### GET

- requests representation of specified resource
- should only retrieve data (no other effect) 

#### POST

- submits data to be processed (e.g., from HTML form) to identified resource
- results in creation and/or update of resource

#### PUT

- uploads representation of specified resource

#### DELETE

- deletes specified resource

#### TRACE

- echoes back received request in order to trace changes/additions made by
  intermediate servers

#### OPTIONS

- returns server supported HTTP methods for specified URL
- can be used to check functionality of web server by requesting "*" instead of
  specified resource

#### CONNECT

- converts request connection to transparent TCP/IP tunnel to facilitate
  [SSL](http://en.wikipedia.org/wiki/Transport_Layer_Security) encrypted
communication [HTTPS](http://en.wikipedia.org/wiki/HTTPS) through unencrypted
[HTTP proxy ](http://en.wikipedia.org/wiki/HTTP_proxy)

#### PATCH

- used for partial resource modifications

#### Safe methods

- HEAD, GET, OPTIONS, TRACE
- only intended for information retrieval (don't change server state - only
  harmless side effects like logging, caching, webcounter increment)

#### Idempotent methods and Web Applications
- multiple identical requests shoud have the same effect as single request
  (e.g., all safe methods)
- sometimes POST should also be idempotent
- enforced by protocol or webserver

#### Status Codes

- first (status) line of HTTP response includes status code and message (e.g.,
  404 "Not Found")

#### HTTP Session State

- server is not required to retain information or status about each user over
  duration of multiple requests
- state can be kept on client side via [HTTP
  cookies](http://en.wikipedia.org/wiki/HTTP_cookie)
- other options include server side sessions, hidden variables (on form) and
  URL rewriting using URI-encode parameters (e.g., /index.html?session_id=guid)

#### Request Message

- request line e.g., GET /images/logo.png HTTP/1.1<CR><LF>
- [headers](http://en.wikipedia.org/wiki/List_of_HTTP_headers) e.g.,
  Accept-Language: en<CR><LF>
- empty line e.g., <CR><LF>
- optional message body

#### Response Message

- status line e.g., HTTP/1.1 200 OK<CR><LF>
- [headers](http://en.wikipedia.org/wiki/List_of_HTTP_headers) e.g.,
  Content-Type: text/html<CR><LF>
- empty line e.g., <CR><LF>
- optional message body

#### Example Session

Client Request:

    GET /index.html HTTP/1.1<CR><LF>
    Host: www.example.com<CR><LF>
    <CR><LF>

- Host header distinguishes between various
  [DNS](http://en.wikipedia.org/wiki/Domain_Name_System) names sharing as
single [IP address](http://en.wikipedia.org/wiki/IP_address) allowing
name-based [virtual hosting](http://en.wikipedia.org/wiki/Virtual_hosting)
(mandatory in HTTP/1.1)

Server Response:

    HTTP/1.1 200 OK
    Date: Mon, 23 May 2005 22:38:34 GMT
    Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
    Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
    Etag: "3f80f-1b6-3e1cb03b"
    Accept-Ranges: bytes
    Content-Length: 438
    Connection: close
    Content-Type: text/html; charset=UTF-8

- ETag (entity tag) header determines if cached version of requested resource
  is identical to current version on the server
- Accept-Ranges: bytes is usefull if client only needs certain portions of
  resource sent by the server, called [byte
serving](http://en.wikipedia.org/wiki/Byte_serving)
- Connection: close means web server closes TCP connection immediately after
  responding
- Content-Length: optional, if missing it is determined

## URI Scheme

- generic syntax:

        <scheme name> : <hierarchical part> [ ? <query> ] [ # <fragment> ]

- query optional (key value pairs separated by semicolon or ampersand)

        key1=value1;key2=value2;key3=value3
        key1=value1&key2=value2&key3=value3

- fragment optional, provides direction to secondary resource, e.g., section
  heading in article

Example:
    
    foo://username:password@example.com:8042/over/there/index.dtb?type=animal&name=narwhal#nose and urn:example:animal:ferret:nose


## Resources

[StackOverflow REST Question](http://stackoverflow.com/questions/544474/can-you-help-me-understand-this-common-rest-mistakes-sessions-are-irrelevant)

[Common REST Mistakes](http://prescod.net/rest/mistakes/)

[Wiki REST](http://en.wikipedia.org/wiki/Representational_State_Transfer)

[Wiki HTTP](http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

[Wiki URI schemes](http://en.wikipedia.org/wiki/URI_scheme)

[Wiki List of TCP and UDP port numbers](http://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers)
