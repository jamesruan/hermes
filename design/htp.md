# Hermes Transport Protocol

## Introduction

### Purpose
The Hermes Transport Protocol provides virtual transport that hides away
detail information of lower level implementation that may differ in various
networks.

While the key idea of Hermes is interconnected nodes manage trust level of
each other, the HTP is the lowest level that deals only with transport.

HTP is designed as an level 3 protocol but may also serves as an overlay
network protocol that runs over any connection packet-orientied protocol.
Lower layer protocol adaptation is also defined in this document.

There are similar idea about providing identity "identifier" independent from
it's "locator" while now IP address serves as both. See [RFC 4423](https://tools.ietf.org/html/draft-ietf-hip-rfc4423-bis-12)
Hermes provides an extra layer of namespace comparing to HIP.

### Requirements
The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", 
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be 
interpreted as described in [RFC 2119](http://www.ietf.org/rfc/rfc2119.txt).

### Terminology

node or Hermes node
    A transport provider that can be communicate using HTP.

nodeID
    Global unique identifier that used to distingush different nodes. nodeID is
    generated partially based on lower level network address.

peer
    The node's owner. peer may have many nodes communicate on different network
    interfaces.

peerID
    Global unique identifier that used to distingush different peers. peerID is
    generated randomly and associates with public key of the peer.

### Parameters

Parameters are properties that can be changed by users or Controllers. They are
the relatively fixed part of protocol state.

MTU or Maximum Transmission Unit
    How many bytes that can be transmitted by underlying layer of network. e.g.
    1500 for Ethernet and 1492 for PPPoE.

TM or Transmission Mode
    Is either CL for "connectinoless" and CO for "connection-oriented".

PO or Packet Order
    Is either In-Order of Out-of-Order.

KA or Keepalive
    Is destination availability continously checked.

MP or Multipath
    Is packet relayed with multiple path or with a single path.

OBC or Obfuscator Controller
    Obfuscator Controller is a set of parameters and algorithm that controls
    the behavior of Obfuscator

MPC or Multipath Controller
    When MP is set, the Multipath Controller is a set of prarmeters and
    algorithm that controls multiple relay paths.

RC or Retry Controller
    In CO Transmission Mode, the Retry Controller is a set of parameters and
    algorithm that controls when and how to retransmit.

KC or Keepalive Controller
    When KA is set, the Keepalive Controller is a set of parameters and
    algorithm that controls when and how to retransmit.

CC or Congestion Controller
    A set of parameters and algorithm that avoid congestion and throttle the
    traffic.

## Protocols

HTP is a bundle of cooperating sublayer protocols that provides various
feature.

### Sublayers

Sublayers controls all operations of HTP in an order.

1. Obfuscator: Obfuscate and deobfuscate.
2. Security Encapsulator: managing security association and encapsulating
   upper layer data;
3. [Obfuscator Controller][note1]: Obfuscate and deobfuscate negotiation.
4. Channel Controller: throttle and (de)multiplexing;
5. Congestion Controller: congestion detection and avoidance;
6. Keepalive Controller: heart beating for CO;
7. Retry Controller: NACK/ACK acceptance and retry emmition;
8. Packet Sorter: order maintaining for CO;
9. Multipath Controller: Multipath mapping.

[note1]: Obfucate and deobfuscate is done in lowerest level, but the
controlling and protocol level communication is happend at level 2.

## Lower Layer Protocols

### Introduction
Hermes Transport Protocol runs over a lower layer protocol that has end-to-end
packet based delivering feature. Lower layer protocol MUST support packet size
as large as 1280 bytes which is the default assumed MTU by IPv6.

### HTP over IPv6

#### IP protocol number
HTP over IPv6 uses IP protocol number 114 (0x72).

