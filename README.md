LMonad
======

LMonad is an Information Flow Control (IFC) framework for Haskell applications. 
It can be used to enforce security properties like confidentiality and integrity. 
It is derived from [LIO](http://hackage.haskell.org/package/lio), but is more general in that it tracks information flow for any monad. 

Usage
-----

Most code should import the `LMonad` module. 
Trusted code can import the `LMonad.TCB` module to perform trusted operations. 
The `LMonadT` monad transformer is used to enable IFC for any monad. 
