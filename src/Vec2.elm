module Vec2 exposing
    ( Vec2
    , vec2
    , setX, setY
    , zero, up, down, left, right
    , add, sub, negate, scale, dot, normalize, direction, midpoint
    , lerp, angle
    , scaleX, scaleY, rotate
    , length, lengthSquared, distance, distanceSquared
    , cross
    , toString
    , point, vector, fromHomogeneous
    )

{-|

@docs Vec2


# Create

@docs vec2


# Setters

The set functions create a new copy of the vector, updating a single field.

@docs setX, setY


# Constants

@docs zero, up, down, left, right


# Operations

@docs add, sub, negate, scale, dot, normalize, direction, midpoint
@docs lerp, angle
@docs scaleX, scaleY, rotate
@docs length, lengthSquared, distance, distanceSquared
@docs cross
@docs toString


# Homogeneous coordinates

@docs point, vector, fromHomogeneous

-}

import Vec3 exposing (Vec3)


{-| -}
type alias Vec2 =
    { x : Float
    , y : Float
    }



---- Setters ----


{-| -}
setX : Float -> Vec2 -> Vec2
setX x { y } =
    { x = x
    , y = y
    }


{-| -}
setY : Float -> Vec2 -> Vec2
setY y { x } =
    { x = x
    , y = y
    }


{-| Create a vector out of its x and y components.
-}
vec2 : Float -> Float -> Vec2
vec2 =
    Vec2


{-| -}
add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


{-| `sub v1 v2` computes v1 - v2.

Can be easily confused in pipelines like

    v2
        |> sub v1 -- this is actually v1 - v2

I think both alternatives are confusing, so I chose this one rather arbitrarily.

-}
sub : Vec2 -> Vec2 -> Vec2
sub v1 v2 =
    { x = v1.x - v2.x
    , y = v1.y - v2.y
    }


{-| -}
scale : Float -> Vec2 -> Vec2
scale factor vec =
    { x = vec.x * factor
    , y = vec.y * factor
    }


{-| -}
distance : Vec2 -> Vec2 -> Float
distance v1 v2 =
    length (sub v1 v2)


{-| -}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared v1 v2 =
    lengthSquared (sub v1 v2)


{-| Computes `|v|` for a vector `v`.

If you need the square of this value, use Vec2.lengthSquared for efficiency.

-}
length : Vec2 -> Float
length vec =
    Basics.sqrt (lengthSquared vec)


{-| Computes `|v|^2` for a vector `v`.

Faster than `length` because it saves a square root operation.

-}
lengthSquared : Vec2 -> Float
lengthSquared { x, y } =
    x ^ 2 + y ^ 2


{-| -}
dot : Vec2 -> Vec2 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


{-| Normalizes a vector v, returning v/|v|.
If the vector is 0, it returns 0.
-}
normalize : Vec2 -> Vec2
normalize vec =
    let
        len =
            length vec
    in
    if len > 0 then
        scale (1 / len) vec

    else
        vec


{-| Given `v`, computes `-v`.
-}
negate : Vec2 -> Vec2
negate { x, y } =
    { x = -x
    , y = -y
    }


{-| Rotates a given vector by an angle in radians, in counterclockwise fashion.
-}
rotate : Float -> Vec2 -> Vec2
rotate angleRadians { x, y } =
    let
        co =
            Basics.cos angleRadians

        si =
            Basics.sin angleRadians
    in
    vec2 (co * x - si * y) (si * x + co * y)


{-| Returns the z-coordinate of the cross product when interpreting the 2D vectors as
3D vectors of the form `vec3 v.x v.y 0`:

    cross v1 v2
        == (Vec3.cross
                (vec3 v1.x v1.y 0)
                (vec3 v2.x v2.y 0)
           ).z

This operation is useful for calculating the torque created by a force in 2D, for example.

-}
cross : Vec2 -> Vec2 -> Float
cross v1 v2 =
    v1.x * v2.y - v1.y * v2.x


{-| Convenient for debugging purposes.
-}
toString : Vec2 -> String
toString { x, y } =
    "{ "
        ++ "x = "
        ++ String.fromFloat x
        ++ ", y = "
        ++ String.fromFloat y
        ++ " }"


{-| -}
scaleX : Float -> Vec2 -> Vec2
scaleX xScale { x, y } =
    { x = xScale * x
    , y = y
    }


{-| -}
scaleY : Float -> Vec2 -> Vec2
scaleY yScale { x, y } =
    { x = x
    , y = yScale * y
    }


{-| Returns a normalized vector pointing from `from` to `to`.
-}
direction : { from : Vec2, to : Vec2 } -> Vec2
direction { from, to } =
    sub to from
        |> normalize


{-| Returns the midpoint between two points. This operation is commutative.
-}
midpoint : Vec2 -> Vec2 -> Vec2
midpoint v1 v2 =
    scale (1 / 2) (add v1 v2)


{-| Linearly interpolate `from` to `to` with a [0, 1] parameter.

    lerp { from = vec2 0 1, to = vec2 1 1 } 0.75 == vec2 0.75 1

-}
lerp : { from : Vec2, to : Vec2 } -> Float -> Vec2
lerp { from, to } t =
    -- ((1-t)*from) + (t*to)
    add (scale (1 - t) from) (scale t to)


{-| -}
angle : Vec2 -> Float
angle { x, y } =
    Basics.atan2 y x



---- Commonly used vectors ----


{-| -}
zero : Vec2
zero =
    vec2 0 0


{-| -}
up : Vec2
up =
    vec2 0 1


{-| -}
down : Vec2
down =
    vec2 0 -1


{-| -}
right : Vec2
right =
    vec2 1 0


{-| -}
left : Vec2
left =
    vec2 -1 0


{-| -}
fromHomogeneous : Vec3 -> Vec2
fromHomogeneous v3 =
    {- TODO: Not sure if this is correct/good/etc. PROBABLY not. -}
    if v3.z /= 0 then
        vec2 (v3.x / v3.z) (v3.y / v3.z)

    else
        vec2 v3.x v3.y


{-| -}
point : Vec2 -> Vec3
point { x, y } =
    { x = x, y = y, z = 1 }


{-| -}
vector : Vec2 -> Vec3
vector { x, y } =
    { x = x, y = y, z = 0 }
