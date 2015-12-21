##The Overall Structure of Hermes

### Layered Model
Each layer of Hermes has its own function. Lower layers maintain their funtion except that
the higher layer extends and controls them. This kind of inter-layer relationship is the
main principle of Hermes.

|Layered Levels |Protocols  |Programmes   |Descriptions                           |
|---------------|-----------|-------------|---------------------------------------|
|Transport      |HTP        |hTransport   |packet and stream relay                |
|Infrastructure |HSCP       |hSession     |authentication and session controlling |
|Routing        |HRP        |hRoute       |authorization and routing              |
|Resource       |HRAP       |hResource    |resource accessing                     |
|Appliction     |varies     |varies       |applictions based on those layers      |


#### Transport Layer

This layer deals with basic packet and stream relay by providing virtual transport and
controling real transport.

##### Transport Modes

There are two modes of communcation:

  * connectionless(CL):
    * assuming other part of the communication is reachable
    * assuming not to be noitfied when other part of the communication changes its state
  * connection-oriented(CO):
    * assuming other part of the communication is not reachable
    * assuming to be noitfied when other part of the communication changes its state

##### Virtual Transport
Two node of Hermes can connect directly or by relay of other nodes.

Virtual transport provides interfaces that the upper layer can use without knowning the
details of how real lower level transport is working.

Vritual transport is provided by protocol: HTP (Hermes Transport Protocol).
