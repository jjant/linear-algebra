# Linear Algebra for Elm

A library for vector and matrix math. See the full docs [here][docs].

[docs]: https://package.elm-lang.org/packages/jjant/linear-algebra/latest/

This library is _not_ compatible with WebGL, and will require conversions from one's types to the other. This library is however more featureful (includes `Mat2`, `Mat3`) and quite a few useful operations not found in the official package.

The implementation of all these types is using plain elm records, which _may_ be faster than the official one, which uses typed arrays.

[webgl]: https://github.com/elm-explorations/webgl
