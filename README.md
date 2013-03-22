simple-ad-srv
=============

Simple ad server, written with Haskell Warp framework

Dev environment was created with cabal-dev only, so i dont know if it will run on haskell-platform

So what does that thing:
--
    * parses a uri from http query
    * looks up specific key concatenated with uri in memcached
    * if such keys exists in memcached, get the string
    * tries to parse this string as specific JSON array
    * get random array element, pushes a notification object to RabbitMQ
    * responses with the "code" part of random element
    * else responses with empty string

TODO: 
--
* double and triple check for laziness/concurrency
* text or yml config for ports, queue names, exchanges, hosts and much more
* static linking (to run standalone)
* basic logging
* build and install instructions

