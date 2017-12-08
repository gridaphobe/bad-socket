# Making `network` hang on macOS

Blocking writes to a socket seem to hang (on macOS) when another thread is trying to read from the same socket.

To demonstrate this we have a simple tcp proxy server that connects two instances of `yes` talking via `nc`.

## Good

``` bash
# shell 1
$ yes | nc -l localhost 6789
# shell 2
$ stack exec bad-socket
# shell 3
$ yes | nc localhost 9876
```

Output from proxy server:

```
...
("send[c->s]: wrote   ",1024)
("send[s->c]: writing ",1024)
("writev[s->c]: ",1024)
("send[s->c]: wrote   ",1024)
("send[c->s]: writing ",1024)
("writev[c->s]: ",1024)
("send[c->s]: wrote   ",1024)
("send[s->c]: writing ",1024)
("writev[s->c]: ",1024)
("send[s->c]: wrote   ",1024)
("send[c->s]: writing ",1024)
("writev[c->s]: ",1024)
("send[c->s]: wrote   ",1024)
...
```

## Bad

``` bash
# shell 1
$ nc -l localhost 6789
# shell 2
$ stack exec bad-socket
# shell 3
$ yes | nc localhost 9876
```

Output from proxy server:

``` 
...
("send[c->s]: writing ",4096)
("writev[c->s]: ",4096)
("send[c->s]: wrote   ",4096)
("send[c->s]: writing ",4096)
("writev[c->s]: ",4096)
("send[c->s]: wrote   ",4096)
("send[c->s]: writing ",4096)
("writev[c->s]: ",3448)
("send[c->s]: wrote   ",3448)
("send[c->s]: writing ",648)
("writev[c->s]: ",-1)
("errno[c->s]: ",35,writev: resource exhausted (Resource temporarily unavailable))
```

Eventually the outgoing buffer is filled up and the `writev` call blocks and never recovers.

I've tested this on macOS 10.13.2 with GHC 8.0.2 and 8.2.2. The hang does not occur on Linux as far as I can tell.
