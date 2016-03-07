PJS-JSON
========

A small library containing both a parser and a STREAMING generator
for JSON.

In working on [my web application library][1], I had the problem of
converting the resultset of an SQL query into JSON for transfer to
the client. The natural structure of this JSON would be an array
of arrays (where each sub array represents a DB row, and each
element of the sub array represents a field from the DB row).

[1]: https://github.com/pjstirling/pjs-webapp

However, the existing libraries I looked at for generating JSON
would not work incrementally, they would take the whole object
to be converted, or bust! For my use case this was unacceptable,
consing up a big (the JSON for some of my queries reaches 42k
without extra whitespace) vector-of-vectors for my result-set
simply to be gc'd as soon as the JSON generator returned is gross!

My initial implementation involved manually writing the
appropriate JSON tokens to the output stream, as I pulled each row
from the database, which both eliminated the need for the big
vectors, and also reduced latency for the client.

Once that was working I decided to reify the code into this
library, which lets you do the same thing, but in a structured way
(ensuring that you don't forget closing tokens, commas, etc)

Generator Usage
---------------

All symbols are exorted from `PJS-JSON`

All output must be made within the lexical context of either
`WITH-JSON-STREAM` or `WITH-JSON-OUTPUT-TO-STRING` (this is so that
aggressive merging of stuff that is known at compile time can
take place)

`JSON-ARR` perhaps unsurprisingly is responsible for outputting
arrays.

`JSON-OBJ` creates hashes.

`JSON-VAL` is for outputting values, it handles:

* Numbers,
* Strings,
* `t` (output as `true`),
* `nil` (output as `null`),
* Vectors,
* Hashtables
* Quoted symbols, and keywords (converted to strings by Parenscript's SYMBOL-TO-JS-STRING)

Example Usage
-------------

```common-lisp
(with-json-output-to-string
  (json-obj
    (json-k-v 'column-headings
              (json-arr
	        (dolist (el '("some" "abritrary" "stuff"))
		  (json-val el))))
    (json-k-v "string key"
              #(1 2 3))))
```

Parser
------

There is also a parser `JSON-PARSE` it returns 4 `VALUES`

1. The parsed form
2. Whether the whole input was consumed
3. The last index reached in the input
4. The length of the input string

