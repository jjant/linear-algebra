module Mat3 exposing
    ( Mat3
    , identity, fromRows, rotate, scale, translate
    , transpose, mul
    , transform, transformVector, transformPoint
    , orthographic
    )

{-| Mat3

@docs Mat3


# Create

@docs identity, fromRows, rotate, scale, translate


# Operations

@docs transpose, mul

{mul, det,invert}


# Transformations

@docs transform, transformVector, transformPoint


# Projections

@docs orthographic

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


{-| -}
fromRows : Vec3 -> Vec3 -> Vec3 -> Mat3
fromRows row1 row2 row3 =
    { m11 = row1.x
    , m12 = row1.y
    , m13 = row1.z
    , m21 = row2.x
    , m22 = row2.y
    , m23 = row2.z
    , m31 = row3.x
    , m32 = row3.y
    , m33 = row3.z
    }


{-| -}
mul : Mat3 -> Mat3 -> Mat3
mul a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31
    , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32
    , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32
    , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33
    , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33
    , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33
    }


{-| -}
transpose : Mat3 -> Mat3
transpose { m11, m12, m13, m21, m22, m23, m31, m32, m33 } =
    { m11 = m11
    , m12 = m21
    , m13 = m31
    , m21 = m12
    , m22 = m22
    , m23 = m32
    , m31 = m13
    , m32 = m23
    , m33 = m33
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


{-| -}
orthographic : { width : Float, height : Float } -> Mat3
orthographic { width, height } =
    fromRows
        (vec3 (2 / width) 0 0)
        (vec3 0 (2 / height) 0)
        (vec3 0 0 1)



---- MISC ----


drop3rdCoordinate : Vec3 -> Vec2
drop3rdCoordinate v3 =
    vec2 v3.x v3.y
