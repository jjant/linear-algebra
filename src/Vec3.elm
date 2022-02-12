module Vec3 exposing
    ( Vec3
    , vec3
    , setX, setY, setZ
    , zero, i, j, k
    , add, sub, negate, scale, dot, cross, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , point, vector, fromHomogeneous
    )

{-|

@docs Vec3


# Create

@docs vec3


# Setters

The set functions create a new copy of the vector, updating a single field.

@docs setX, setY, setZ


# Constants

@docs zero, i, j, k


# Operations

@docs add, sub, negate, scale, dot, cross, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Homogeneous coordinates

@docs point, vector, fromHomogeneous

-}

import Vec4 exposing (Vec4)


{-| -}
type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


{-| Create a vector out of its x, y, and z components.
-}
vec3 : Float -> Float -> Float -> Vec3
vec3 =
    Vec3



---- Constants ----


{-| -}
zero : Vec3
zero =
    vec3 0 0 0


{-| -}
i : Vec3
i =
    vec3 1 0 0


{-| -}
j : Vec3
j =
    vec3 0 1 0


{-| -}
k : Vec3
k =
    vec3 0 0 1



---- Setters ----


{-| -}
setX : Float -> Vec3 -> Vec3
setX x vec =
    { vec | x = x }


{-| -}
setY : Float -> Vec3 -> Vec3
setY y vec =
    { vec | y = y }


{-| -}
setZ : Float -> Vec3 -> Vec3
setZ z vec =
    { vec | z = z }



---- Operations ----


{-| -}
add : Vec3 -> Vec3 -> Vec3
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    , z = v1.z + v2.z
    }


{-| -}
sub : Vec3 -> Vec3 -> Vec3
sub v1 v2 =
    add v1 (scale -1 v2)


{-| -}
negate : Vec3 -> Vec3
negate v =
    scale -1 v


{-| -}
scale : Float -> Vec3 -> Vec3
scale factor v =
    { x = factor * v.x
    , y = factor * v.y
    , z = factor * v.z
    }


{-| -}
dot : Vec3 -> Vec3 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y + v1.z * v2.z


{-| Normalizes a vector.

        normalize (vec3 0 3 4) = vec3 0 0.6 0.8
        normalize (vec3 0 0 0) = vec3 0 0 0

-}
normalize : Vec3 -> Vec3
normalize v =
    let
        len2 =
            lengthSquared v
    in
    if len2 /= 0 then
        scale (1 / Basics.sqrt len2) v

    else
        v


{-| -}
cross : Vec3 -> Vec3 -> Vec3
cross a b =
    vec3 (a.y * b.z - a.z * b.y)
        (a.z * b.x - a.x * b.z)
        (a.x * b.y - a.y * b.x)


{-| -}
length : Vec3 -> Float
length v =
    Basics.sqrt (lengthSquared v)


{-| -}
lengthSquared : Vec3 -> Float
lengthSquared { x, y, z } =
    x ^ 2 + y ^ 2 + z ^ 2


{-| -}
distance : Vec3 -> Vec3 -> Float
distance v1 v2 =
    sub v1 v2
        |> length


{-| -}
distanceSquared : Vec3 -> Vec3 -> Float
distanceSquared v1 v2 =
    sub v1 v2
        |> lengthSquared


{-| -}
direction : { from : Vec3, to : Vec3 } -> Vec3
direction { from, to } =
    sub to from
        |> normalize


{-| -}
fromHomogeneous : Vec4 -> Vec3
fromHomogeneous v4 =
    {- TODO: Not sure if this is correct/good/etc. PROBABLY not. -}
    if v4.w /= 0 then
        vec3 (v4.x / v4.w) (v4.y / v4.w) (v4.z / v4.w)

    else
        vec3 v4.x v4.y v4.z


{-| -}
point : Vec3 -> Vec4
point { x, y, z } =
    { x = x, y = y, z = z, w = 1 }


{-| -}
vector : Vec3 -> Vec4
vector { x, y, z } =
    { x = x, y = y, z = z, w = 0 }
