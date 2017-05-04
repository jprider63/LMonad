LMonad
======

LMonad is an Information Flow Control (IFC) framework for Haskell applications. 
It can be used to enforce security properties like confidentiality and integrity. 
It is derived from [LIO](http://hackage.haskell.org/package/lio), but is more general in that it tracks information flow for any monad by using monad transformers. 

Usage
-----

Most code should import the `LMonad` module. 
Trusted code can import the `LMonad.TCB` module to perform trusted operations. 
The `LMonadT` monad transformer is used to enable IFC for any monad. 

Use the [LMonad-Yesod](https://github.com/jprider63/LMonad-yesod) library to integrate LMonad into your Yesod application. 
