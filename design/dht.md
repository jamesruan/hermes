## Basic DHTs

### DHT of Identities (DHTI)

The DHT of Identites records enough information to reach an identity online.

For certain identity, DHTI records:
* ID of the identity (key of the record)
* Version: last modified time
* Identity's main public key
* Key of profile in the DHT of Profiles
* Addresses that the identity used and using

See [definition](https://github.com/jamesruan/hermes/blob/master/asn1/Dhti.asn1)

### DHT of Profiles (DHTP)

The DHT of Profiles records information of identity's profile.

For certain Profile, DHTP records:
* ID of the owner's identity (key of the record)
* Version: last modified time
* Name of the owner
* Other Profile information in HTML

### DHT of Relationships (DHTR)

The DHT of Relationships records the one-direction credit of one identity to another.

* ID of the relationship (key of the record)
* Version: last modified time
* Credit: -16 to 15.

### DHT of Sessions (DHTS)

The DHT of Sessions records recent sessions of an identity.
* ID of the identity's session (key of the record)
* Version: last modified time
* Session's public key
* Session's public key encrypted token
