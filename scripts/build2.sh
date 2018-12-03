#!/bin/bash

stack build --copy-bins &&
stack exec xml-typelift-cli test/customersOrders.xsd >customersOrders.hs
stack ghc -- -ilib customersOrders.hs
