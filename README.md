# Packet Inspector

Packet tracing web UI.  Pulls packets from SSE stream (which must originate from the same website - see etc/nginx.conf for a nginx proxy example).

# Security

The packet tracer is a development/QA tool to make network traffic related to a user or topic visible in order to debug or validate server and client behavior.

# Setup

TODO: to install...

# Usage

The packet tracing tool listens on a channel.  For applications that are user-focused this naturally corresponds to the users account identifier.

HTTP request to trace a session identified with a username martin

```
http://packets.tigertext.me:1338/trace/martin
```


# Packet Types

The trace interface can render XMPP, XML or HTTP requests.  The XMPP rendering is almost identical to XML but since the packet encoding is known the trace will automatically calculate the request time in milliseconds between the async request and response.  HTTP requests are rendered with headers and parameters but large data payloads in the body are cropped.

The trace works with an associated SSE (server-sent-event) stream that can also be viewed in the browser as raw-data.  To see the SSE data payloads use:

```
http://packets.tigertext.me:1338/stream/martin
```

## XMPP


```
data: {"server":"send", "timestamp":"2013-05-22T14:44:32.475083Z", "data":"<iq xmlns='tigertext:iq:client_advisory' type='set' id='08d60280-e3fb-498a-baf6-a07e0ffd3717'><query xmlns='tigertext:iq:client_advisory'><advisory name='replay_start' replay_message_count='0'\/><\/query><\/iq>"}

data: {"server":"receive", "timestamp":"2013-05-22T14:44:32.485366Z","data":"<iq to=\"tigertext.me\" type=\"get\" id=\"AFC4F6EF-5902-47A4-936F-084A9396AD9C\"><query xmlns=\"jaber:iq:friends\"\/><\/iq>"}
```

## XML

## HTTP 

```
data: {"server":"receive", "timestamp":"2013-05-22T14:44:32.485366Z", "headers":[{}], parameters:[]}
```

# Application Hooks

TODO: a link to Erlang.md where the erlang SSE streams and calls are documented
