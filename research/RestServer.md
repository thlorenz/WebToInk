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

## Resources

[StackOverflow REST Question](http://stackoverflow.com/questions/544474/can-you-help-me-understand-this-common-rest-mistakes-sessions-are-irrelevant)

[Common REST Mistakes](http://prescod.net/rest/mistakes/)

[Wiki REST](http://en.wikipedia.org/wiki/Representational_State_Transfer)

[Wiki HTTP](http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

[URI schemes](http://en.wikipedia.org/wiki/URI_scheme)

[List of TCP and UDP port numbers](http://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers)
