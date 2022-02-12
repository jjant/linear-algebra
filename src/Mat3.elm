module Mat3 exposing
    ( Mat3
    , identity, fromRows
    , mul, invert, transpose
    , rotate, scale, translate
    , lookAt, orthographic, viewport
    , transform, transformVector, transformPoint
    )

{-|

@docs Mat3


# Create

@docs identity, fromRows


# Operations

@docs mul, invert, transpose


# 2D Transformations

@docs rotate, scale, translate


# Projections

@docs lookAt, orthographic, viewport


# Apply matrices

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


{-| Create a matrix out of vectors representing its rows.
-}
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
invert : Mat3 -> Maybe Mat3
invert { m11, m12, m13, m21, m22, m23, m31, m32, m33 } =
    let
        det =
            m11 * m22 * m33 + m12 * m23 * m31 + m13 * m21 * m32 - m11 * m23 * m32 - m12 * m21 * m33 - m13 * m22 * m31

        idet =
            1 / det
    in
    if det == 0 then
        Nothing

    else
        Just
            { m11 = (m22 * m33 - m23 * m32) * idet
            , m12 = (m13 * m32 - m12 * m33) * idet
            , m13 = (m12 * m23 - m13 * m22) * idet
            , m21 = (m23 * m31 - m21 * m33) * idet
            , m22 = (m11 * m33 - m13 * m31) * idet
            , m23 = (m13 * m21 - m11 * m23) * idet
            , m31 = (m21 * m32 - m22 * m31) * idet
            , m32 = (m12 * m31 - m11 * m32) * idet
            , m33 = (m11 * m22 - m12 * m21) * idet
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
transformVector mat3 v =
    transform mat3 (Vec2.vector v)
        |> Vec2.fromHomogeneous


{-| Transforms a point, applying scaling, rotation, and translation.
-}
transformPoint : Mat3 -> Vec2 -> Vec2
transformPoint mat3 p =
    transform mat3 (Vec2.point p)
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
lookAt : { centerOfAttention : Vec2, upDirection : Vec2 } -> Mat3
lookAt { centerOfAttention, upDirection } =
    let
        angle =
            Vec2.angle upDirection - Vec2.angle Vec2.up
    in
    -- Center of attention becomes 0,0
    translate (Vec2.negate centerOfAttention)
        -- Rotate stuff so that up direction is up
        |> mul (rotate -angle)


{-| -}
orthographic : { width : Float, height : Float } -> Mat3
orthographic { width, height } =
    fromRows
        (vec3 (2 / width) 0 0)
        (vec3 0 (2 / height) 0)
        (vec3 0 0 1)


{-| Transforms clip space into viewport space.

    --      Clip space
    --
    --                ^ +1
    --                |
    --                |
    --                |
    --   -1         y |           +1
    --   <--------- (0,0) --------->
    --                |  x
    --                |
    --                |
    --                |
    --                v -1
    --
    --
    --
    --
    --
    --      Viewport space
    --                       width
    --        (0,0)------------>
    --          |
    --          |
    --          |
    --          |
    --          v
    --        height
    --
    --



-}
viewport : { width : Float, height : Float } -> Mat3
viewport { width, height } =
    -- Start in clip space: [-1, 1] x [-1, 1]
    -- [0, 2] x [0, 2]
    translate (vec2 1 1)
        -- [0, 1] x [0, -1]
        |> mul (scale (vec2 0.5 -0.5))
        -- [0, width] x [0, -height]
        |> mul (scale (vec2 width height))
        -- [0, width] x [height, 0]
        |> mul (translate (vec2 0 height))
