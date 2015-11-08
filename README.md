`Simple` is "framework-less" web framework for Haskell web applications
using the WAI server interface (e.g. for use with the warp server). Unlike
other frameworks, `Simple` does not enforce a particular structure or
paradigm for web applications. Rather, `Simple` makes it easier for you, the
developer, to use whichever paradigm or structure you like. This package
includes:

* Web application building blocks under Web.Simple

* A [Sintra](http://www.sinatrarb.com) inspired DSL - Web.Frank

* A Monad for building RESTful controllers - Web.REST

To get started using the warp web server:

    $ cabal install simple warp


`helloworld.hs`:

    import Web.Simple
    import Network.Wai.Handler.Warp
 
    main :: IO ()
    main = runSettings defaultSettings $ mkRouter $
            okHtml "Hello World"

Then:
  
    $ runghc -XOverloadedStrings helloworld.hs@

See `Web.Simple` for a more detailed introduction.
