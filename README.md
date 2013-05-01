
# Sausage, an extensible compiler 

You can design embedded languages and write compiler transformations
to a lower-level language. These separate passes can be composed to
build a compiler. It's similar in spirit to [Racket's #lang
feature](http://docs.racket-lang.org/guide/languages.html). 

For example, let's say you want to embed a Prolog-like domain specific
language into Scheme. You would write a transformation that defines a
logic language and a set of rules to turn it into optimized
Scheme. You can add Haskell-ish pattern matching syntax, parallel
programming features, more powerful iteration constructs,
etc. Languages can be layered: pattern matching -> typed scheme ->
scheme. 

Unfortunately, I wrote this a *looong* time ago using an old version of
[Scheme48](http://s48.org). I'll need to port this to another Scheme
implementation before it's generally useful. 

