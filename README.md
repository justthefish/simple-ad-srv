simple-ad-srv
=============

Simple ad server, written with Haskell Warp framework

So what does that thing:
--
* parses the key from http query uri
* looks up for key in memcached
* if such keys exists in memcached, get the string
* tries to parse this string as specific JSON array
* get random array element, pushes a notification object to RabbitMQ
* responses with the "code" part of random element
* else responses with empty string


changes
--

 * i've updated to ghc 7.6.3, moved to cabal-dev build system since ghc doesnt accept multiple paths in 
    GHC_PACKAGE_PATH anymore. And plain cabal just forbids its use.

how to build:
--

    cabal-dev configure
    cabal-dev install
    cabal-dev build

Simple, eh?

TODO: 
--
* persistent amqp and memcached connections
* rewrite amqp driver to handle custom message properties (app_id, type)
* amqp and memcached exception handling
* basic logging
* double and triple check for laziness/concurrency
* text or yml config for ports, queue names, exchanges, hosts and much more
* static linking (to run standalone)
* build and install instructions

