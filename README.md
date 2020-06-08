# WIP: connective

## Motivation

Connective provides an API for each accessing persisted
entities and their relationships. The API is designed to be
consistent and uniform and provide "good defaults" across multiple
backend storage mechanisms. More concretely, this means that
the library does not provide a way of accessing the
entities themselves. That responsibility is mostly left to the
adaptors/implementors and therefore the burden of performance
(and consistency semantics) is on the implemention.

Accordingly, the means of accessing the entities is designed to fit in
homeogenously with the current system's method of fetching data,
regardless of where the API lies in the application's layer
(e.g. jdbc, cassandra, walkable, honeysql).

In short, this library is designed to provide a uniform way to 
access, store, and relate entities.

Inspiration and similar projects:

* https://github.com/jkk/cantata
* https://github.com/walkable-server/walkable
* https://github.com/rails/rails/tree/master/activerecord
  (in the relational mapping, not object-relational mapping)
  
Use this library, when your solution is presenting simple and direct
relationships of entities. Do not use this if you have complex
queries (and want to harness the full capabilities of your query
language) and performance is a concern.

## Documentation

*  [API Docs](https://codestiff.github.io/connective/)


## Usage

WIP

## License

Copyright © 2020 Codestiff

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
