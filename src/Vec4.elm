module Vec4 exposing
    ( Vec4, vec4, zero
    , setX, setY, setZ, setW
    , add, sub, negate, scale, dot, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toString
    )

{-|


# Create

@docs Vec4, vec4, zero


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs setX, setY, setZ, setW


# Operations

@docs add, sub, negate, scale, dot, normalize, direction
@docs length, lengthSquared, distance, distanceSquared
@docs toString

-}


{-| Vec4
-}
type alias Vec4 =
    { x : Float
    , y : Float
    , z : Float
    , w : Float
    }


{-| -}
vec4 : Float -> Float -> Float -> Float -> Vec4
vec4 =
    Vec4


{-| -}
zero : Vec4
zero =
    vec4 0 0 0 0



---- Setters ----


{-| -}
setX : Float -> Vec4 -> Vec4
setX x vec =
    { vec | x = x }


{-| -}
setY : Float -> Vec4 -> Vec4
setY y vec =
    { vec | y = y }


{-| -}
setZ : Float -> Vec4 -> Vec4
setZ z vec =
    { vec | z = z }


{-| -}
setW : Float -> Vec4 -> Vec4
setW w vec =
    { vec | w = w }


{-| -}
add : Vec4 -> Vec4 -> Vec4
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    , z = v1.z + v2.z
    , w = v1.w + v2.w
    }


{-| -}
sub : Vec4 -> Vec4 -> Vec4
sub v1 v2 =
    add v1 (scale -1 v2)


{-| -}
negate : Vec4 -> Vec4
negate v =
    scale -1 v


{-| -}
scale : Float -> Vec4 -> Vec4
scale factor v =
    { x = factor * v.x
    , y = factor * v.y
    , z = factor * v.z
    , w = factor * v.w
    }


{-| -}
length : Vec4 -> Float
length v =
    Basics.sqrt (lengthSquared v)


{-| -}
lengthSquared : Vec4 -> Float
lengthSquared { x, y, z, w } =
    x ^ 2 + y ^ 2 + z ^ 2 + w ^ 2


{-| -}
normalize : Vec4 -> Vec4
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
dot : Vec4 -> Vec4 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w


{-| -}
distance : Vec4 -> Vec4 -> Float
distance v1 v2 =
    sub v1 v2
        |> length


{-| -}
distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared v1 v2 =
    sub v1 v2
        |> lengthSquared


{-| -}
direction : { from : Vec4, to : Vec4 } -> Vec4
direction { from, to } =
    sub to from
        |> normalize


{-| -}
toString : Vec4 -> String
toString { x, y, z, w } =
    "{ "
        ++ "x = "
        ++ String.fromFloat x
        ++ ", y = "
        ++ String.fromFloat y
        ++ ", z = "
        ++ String.fromFloat z
        ++ ", w = "
        ++ String.fromFloat w
        ++ " }"
