Packet Inspector
===========

# Payloads

All incoming Packet Inspector data streams are JSON structures with a standardized meta block that always 
includes a request name and a data block that contains the request specific payload.

For example:

```
{"pi": {
    "meta": {
        "request": "data_packet",
        "timestamp": 1381334122828699,
        "account": {
            "token": "c72a7802-190f-4984-8fcc-58a149ba8b63",
            "name": "Martin Hald",
            "avatar": "http:\/\/d1n6bomzxlt9xn.cloudfront.net\/v1\/c72a7802-190f-4984-8fcc-58a149ba8b63\/2.jpg"
            }
        },
        "data": {
            "direction": "received",
            "encoding": "xmpp",
            "on-behalf-of": true,
            "sender": "Mobile Phone",
            "recipient": "XMPP Server",
            "data": "<iq xmlns='jabber:is_typing' type='result' id='0FD94515-AFA6-4B16-8030-353BB2117474'\/>"
        }
    }
}
```

# Request Types

There are several request types, with various payloads:

* Data Packets (General)
* Http Request
* Http Response
* Generic

## General

XMPP packets contain basic payload data with an account block on who is making the request and 

* Direction: sent or received
* Encoding: xmpp

