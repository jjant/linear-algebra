# Linear Algebra for Elm

A library for vector and matrix math.

This library is _not_ compatible with WebGL, and will require conversions from one's types to the other. This library is however more featureful, including `Mat2`, `Mat3`, and quite a few useful operations not found in the official package.

The implementation of all these types is using plain elm records, which _may_ be faster than the official one, which uses JS typed arrays.

You can convert the `Vec2, Vec3, Vec4, Mat4` types to [elm-explorations/linear-algebra][linear-algebra] using its `fromRecord` functions.

[webgl]: https://github.com/elm-explorations/webgl
[linear-algebra]: https://github.com/elm-explorations/linear-algebra
[docs]: https://package.elm-lang.org/packages/jjant/linear-algebra/latest/
