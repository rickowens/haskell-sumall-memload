# sumall-memload

This package provides some utilities 

## Motivation

The motivation behind this library is that we want some of our services,
specifically those that operate cooperatively in a homogeneous cluster, to
start rejecting new requests or otherwise gracefully handle a service backoff
whenever that instance is overloaded. Measuring what it means for a service to
be overloaded is tricky, and depends a lot on the performance profile of your
program. 

One common profile, however, is memory bound programs. This package
provides utilities for detecting the case where your program is overloaded with
regard to memory.

## Development

This package requires GHC, because it makes use of the GHC garbage collection
utilities.

This is a prototype version to really get a sense if this is even useful.
Therefore it doesn't have many bells and whistles. One possible direction of
future development is to have a variety of load management profiles (Network IO,
CPU, etc.), have them all conform to some common typeclass, and provide
functions used to compose load manager instances in some (hopefully)
interesting ways.


