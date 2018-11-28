module UnitTests where

main =
  assert (skipDoctype "<?xml version="1.0" encoding="UTF-8"?>Hello" == "Hello")
    $ return ()


