module Vec4 exposing
    ( Vec4
    , vec4
    , toString
    , setW, setX, setY, setZ
    )

{-| Vec4

@docs Vec4


# Create

@docs vec4

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
