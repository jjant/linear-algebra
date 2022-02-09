module Mat3 exposing
    ( Mat3
    , identity, rotate, scale, translate
    , transform, transformVector, transformPoint
    )

{-| Mat3

@docs Mat3


# Create

@docs identity, rotate, scale, translate


# Operations

{mul, det,invert}


# Transformations

@docs transform, transformVector, transformPoint

-}

import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (Vec3, vec3)


{-| -}
type alias Mat3 =
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    }


{-| -}
identity : Mat3
identity =
    { m11 = 1
    , m12 = 0
    , m13 = 0
    , m21 = 0
    , m22 = 1
    , m23 = 0
    , m31 = 0
    , m32 = 0
    , m33 = 1
    }



---- 2D Operations ----


{-| -}
translate : Vec2 -> Mat3
translate { x, y } =
    { m11 = 1
    , m12 = 0
    , m13 = x
    , m21 = 0
    , m22 = 1
    , m23 = y
    , m31 = 0
    , m32 = 0
    , m33 = 1
    }


{-| -}
rotate : Float -> Mat3
rotate angleRadians =
    { m11 = cos angleRadians
    , m12 = -(sin angleRadians)
    , m13 = 0
    , m21 = sin angleRadians
    , m22 = cos angleRadians
    , m23 = 0
    , m31 = 0
    , m32 = 0
    , m33 = 1
    }


{-| Creates a 2D scaling matrix:

    m =  Mat3.scale (vec2 2 3)

    Mat3.transformPoint m (vec2 5 8) == vec2 10 24
    Mat3.transformVector m (vec2 5 8) == vec2 10 24

-}
scale : Vec2 -> Mat3
scale vec =
    { m11 = vec.x
    , m12 = 0
    , m13 = 0
    , m21 = 0
    , m22 = vec.y
    , m23 = 0
    , m31 = 0
    , m32 = 0
    , m33 = 1
    }


{-| Transforms a vector, applying scaling and rotation, but not translation.
-}
transformVector : Mat3 -> Vec2 -> Vec2
transformVector mat3 v2 =
    transform mat3 (vec3 v2.x v2.y 0)
        |> drop3rdCoordinate


{-| Transforms a point, applying scaling, rotation, and translation.
-}
transformPoint : Mat3 -> Vec2 -> Vec2
transformPoint mat3 v2 =
    transform mat3 (vec3 v2.x v2.y 1)
        |> Vec2.fromHomogeneous


{-| Transforms a 3D vector.
-}
transform : Mat3 -> Vec3 -> Vec3
transform { m11, m12, m13, m21, m22, m23, m31, m32, m33 } { x, y, z } =
    vec3
        (m11 * x + m12 * y + m13 * z)
        (m21 * x + m22 * y + m23 * z)
        (m31 * x + m32 * y + m33 * z)



---- MISC ----


drop3rdCoordinate : Vec3 -> Vec2
drop3rdCoordinate v3 =
    vec2 v3.x v3.y
