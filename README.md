# hermes
Hermes Encrypted Relay of Message, an Erlang-based System

## Features
* End-to-end encrytion for private messaging
* DHT-based network of peers (no single point of registration)
* Best-effort relay of message (no guarantee of delivery)
* Reliable delivery (all or none, managed timeout and retransmition)
* Virtual circuit streaming

## Possible client appliction: 
* Packet relay based messaging (hermes-wave)
* Virtual circuit based instant messaging (hermes-im)
* Dedicated message storage (hermes-mail)
* Active public content delivering (hermes-cd)
