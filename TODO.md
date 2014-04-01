# TODO

## simple-templates

  * add some native operations: boolean operators, basic arithmetic. These can
    be implemented as functions in the FFI, but would be better to have
    syntactic support. For example `if(foo || bar)` instead of
    `if(or([foo,bar]))`

  * optionally ignore whitespace before/after template clause, same as ERB's
    `<%-`

## simple

  * The default instance for `HasTemplates` should include functions for
    sanatizing user generated content. Perhaps introduce a type wrapper with
    two constructors, `NeedsEscape str | Raw str`.

