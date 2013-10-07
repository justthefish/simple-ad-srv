simple-ad-srv
=============

Simple ad server, written with Haskell Warp framework
Intended to serve banner codes, while tracking views.

So what does that thing:
--
* parses the key from http query uri
* looks up for key in memcached
* if such keys exists in memcached, get the string
* tries to parse this string as array of JSON objects
* gets random array element, pushes a notification to RabbitMQ
* responses with the "code" part of random element
* else responses with empty string


changes
--

 * moved to ghc 7.6.3, cabal 1.18 (sandbox ftw)

how to build:
--

    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build

Simple, eh?

TODO: 
--
* rewrite amqp driver to handle custom message properties (app_id, type)
* amqp and memcached exception handling
* basic logging
* double and triple check for laziness/concurrency

