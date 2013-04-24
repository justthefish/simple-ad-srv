sudo apt-get install ghc cabal-install
cabal update
cabal install cabal-dev
sudo ln -s ~/.cabal/bin/cabal-dev /usr/local/bin/cabal-dev
cabal-dev update
cabal-dev install random utf8-string aeson warp memcached amqp base64-string base64-bytestring

