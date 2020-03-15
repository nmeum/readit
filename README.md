# readit

Tooling for managing structured reading notes for scientific publications.

## Motivation

When reading scientific papers and books I usually take notes.
Currently, I primarily use markdown for note taking. Markdown has the
disadvantages that it does not enforce any structure regarding these
notes and it doesn't allow me to search them efficiently, e.g. find all
publications on a specific topic. In an attempt to improve my reading
notes, I came up with this repository.

## Status

Work in progress, I am (more or less) happy with the input format. The
tooling is still in very early stages of development. Tooling for
finding entries is available as a proof of concept. Additionally, I
would like to add tooling for generating reference graphs, interacting
with fields (e.g. list all field names), and tooling for generating
HTML output.

## Features

* Simple plain-text input format
* [CHICKEN Scheme][chicken website] module for parsing the format
* Work in progress tooling for interacting with files of this format

## Installation

If a correctly configured chicken toolchain is available run:

	$ chicken-install -test

This will compile `readit` and add the binary to your `$PATH`.
Additionally, it will perform some basic functionality tests.

### Building without installing

If installation is not desired, build `readit` as follows:

	$ export CHICKEN_REPOSITORY_PATH="${CHICKEN_REPOSITORY_PATH}:$(pwd)"
	$ chicken-install -n -test

This will create a `readit` binary in the current working directory.

## Syntax

The input file is based on reading list entries with associated
information. Each entry consists of a current state, either read `x` or
unread `-`, a unique identifier, and a description (e.g. publication
title). Additionally, key-value pairs and notes can be optionally
associated with an entry. The syntax of the input format is specified
using parser combinators in the file `parser.scm`.

The optional key-value pairs are entirely user-chosen. Two special types
are provided for field values: References and sets. References are
enclosed in square brackets and contain (one or more) reference to the
unique identifiers of other entries. Sets are collections of multiple
values, e.g. multiple author names, sets are specified using curly
brackets.

The optional notes are separated from key-value pairs using an empty
line (even when fields are omitted). The reference and set literals
cannot currently be used in the notes section. At the moment, notes are
just uninterpreted plain text.

## Example

Consider the following input file:

	x [rfc7252]: The Constrained Application Protocol (CoAP)
		* Authors: {Zach Shelby, Klaus Hartke, Carsten Bormann}

	- [rfc7228]: Terminology for Constrained-Node Networks
		* Authors: {Carsten Bormann, Mehmet Ersue, Ari Keranen}
		* DOI: 10.17487/RFC7228
		* Topics: {Internet of Things, Constrained Devices}
		* References: [rfc7252]

		* Introduces different Internet of Things related terms
		* Differentiates different classes of constrained devices

This file contains reading list entries for [RFC 7252][rfc7252] and
[RFC 7228][rfc7228]. Only the latter contains notes, both make use of
optional fields. Tooling is provided for finding entries by fields. For
example, to find a publication by a specific field value, e.g. a
publication with a specific `DOI`, the `readit` tool must be invoked as
follows:

	$ readit -f test.readit -v "10.17487/RFC7228" DOI
	- [rfc7228]: Terminology for Constrained-Node Networks

As the value of the `DOI` field is neither a reference nor a set, this
query only matches entries providing a `DOI` field with this exact
value. For set values the tool only checks if any element in the set
matches the value. As an example, this can be used to find all
publications co-authored by `Carsten Bormann`:

	$ readit -f test.readit -v "Carsten Bormann" "Authors"
	x [rfc7252]: The Constrained Application Protocol (CoAP)
	- [rfc7228]: Terminology for Constrained-Node Networks

To find all publications written by both `Carsten Bormann` and `Mehmet
Ersue`, the `-v` flag can be passed more than once. For example:

	$ readit -f test.readit -v "Carsten Bormann" -v "Mehmet Ersue" "Authors"
	x [rfc7228]: Terminology for Constrained-Node Networks

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.

[chicken website]: https://call-cc.org/
[rfc7252]: https://tools.ietf.org/html/rfc7252
[rfc7228]: https://tools.ietf.org/html/rfc7228
