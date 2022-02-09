module Vec4 exposing
    ( Vec4
    , vec4
    )

{-| Vec4

@docs Vec4


# Create

@docs vec4

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
