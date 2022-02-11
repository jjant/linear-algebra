module Mat4 exposing
    ( Mat4, identity
    , invert, mul, transpose, transform
    , lookAt
    , rotate, scale, translate
    )

{-| This library uses the convention that the prefix `make` is creating a new
array,as without the prefix, you are applying some transform to an
existing matrix.


# Create

@docs Mat4, identity


# Operations

@docs invert, mul, transpose, transform

-- @docs makeBasis


# Projections

@docs lookAt

-- @docs makeFrustum, makePerspective, makeOrtho, makeOrtho2D,


# Create Transformations

@docs rotate, scale, translate

-}

import Vec3 exposing (Vec3)
import Vec4 exposing (Vec4, vec4)


{-| Mat4
-}
type alias Mat4 =
    { m11 : Float
    , m12 : Float
    , m13 : Float
    , m14 : Float
    , m21 : Float
    , m22 : Float
    , m23 : Float
    , m24 : Float
    , m31 : Float
    , m32 : Float
    , m33 : Float
    , m34 : Float
    , m41 : Float
    , m42 : Float
    , m43 : Float
    , m44 : Float
    }


fromRows : Vec4 -> Vec4 -> Vec4 -> Vec4 -> Mat4
fromRows v1 v2 v3 v4 =
    Mat4 v1.x v1.y v1.z v1.w v2.x v2.y v2.z v2.w v3.x v3.y v3.z v3.w v4.x v4.y v4.z v4.w


{-| -}
identity : Mat4
identity =
    fromRows
        (vec4 1 0 0 0)
        (vec4 0 1 0 0)
        (vec4 0 0 1 0)
        (vec4 0 0 0 1)


{-| -}
transpose : Mat4 -> Mat4
transpose { m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44 } =
    fromRows
        (vec4 m11 m21 m31 m41)
        (vec4 m12 m22 m32 m42)
        (vec4 m13 m23 m33 m43)
        (vec4 m14 m24 m34 m44)


{-| -}
invert : Mat4 -> Maybe Mat4
invert m =
    let
        r11 =
            m.m22 * m.m33 * m.m44 - m.m22 * m.m43 * m.m34 - m.m23 * m.m32 * m.m44 + m.m23 * m.m42 * m.m34 + m.m24 * m.m32 * m.m43 - m.m24 * m.m42 * m.m33

        r12 =
            -m.m12 * m.m33 * m.m44 + m.m12 * m.m43 * m.m34 + m.m13 * m.m32 * m.m44 - m.m13 * m.m42 * m.m34 - m.m14 * m.m32 * m.m43 + m.m14 * m.m42 * m.m33

        r13 =
            m.m12 * m.m23 * m.m44 - m.m12 * m.m43 * m.m24 - m.m13 * m.m22 * m.m44 + m.m13 * m.m42 * m.m24 + m.m14 * m.m22 * m.m43 - m.m14 * m.m42 * m.m23

        r14 =
            -m.m12 * m.m23 * m.m34 + m.m12 * m.m33 * m.m24 + m.m13 * m.m22 * m.m34 - m.m13 * m.m32 * m.m24 - m.m14 * m.m22 * m.m33 + m.m14 * m.m32 * m.m23

        r21 =
            -m.m21 * m.m33 * m.m44 + m.m21 * m.m43 * m.m34 + m.m23 * m.m31 * m.m44 - m.m23 * m.m41 * m.m34 - m.m24 * m.m31 * m.m43 + m.m24 * m.m41 * m.m33

        r22 =
            m.m11 * m.m33 * m.m44 - m.m11 * m.m43 * m.m34 - m.m13 * m.m31 * m.m44 + m.m13 * m.m41 * m.m34 + m.m14 * m.m31 * m.m43 - m.m14 * m.m41 * m.m33

        r23 =
            -m.m11 * m.m23 * m.m44 + m.m11 * m.m43 * m.m24 + m.m13 * m.m21 * m.m44 - m.m13 * m.m41 * m.m24 - m.m14 * m.m21 * m.m43 + m.m14 * m.m41 * m.m23

        r24 =
            m.m11 * m.m23 * m.m34 - m.m11 * m.m33 * m.m24 - m.m13 * m.m21 * m.m34 + m.m13 * m.m31 * m.m24 + m.m14 * m.m21 * m.m33 - m.m14 * m.m31 * m.m23

        r31 =
            m.m21 * m.m32 * m.m44 - m.m21 * m.m42 * m.m34 - m.m22 * m.m31 * m.m44 + m.m22 * m.m41 * m.m34 + m.m24 * m.m31 * m.m42 - m.m24 * m.m41 * m.m32

        r32 =
            -m.m11 * m.m32 * m.m44 + m.m11 * m.m42 * m.m34 + m.m12 * m.m31 * m.m44 - m.m12 * m.m41 * m.m34 - m.m14 * m.m31 * m.m42 + m.m14 * m.m41 * m.m32

        r33 =
            m.m11 * m.m22 * m.m44 - m.m11 * m.m42 * m.m24 - m.m12 * m.m21 * m.m44 + m.m12 * m.m41 * m.m24 + m.m14 * m.m21 * m.m42 - m.m14 * m.m41 * m.m22

        r34 =
            -m.m11 * m.m22 * m.m34 + m.m11 * m.m32 * m.m24 + m.m12 * m.m21 * m.m34 - m.m12 * m.m31 * m.m24 - m.m14 * m.m21 * m.m32 + m.m14 * m.m31 * m.m22

        r41 =
            -m.m21 * m.m32 * m.m43 + m.m21 * m.m42 * m.m33 + m.m22 * m.m31 * m.m43 - m.m22 * m.m41 * m.m33 - m.m23 * m.m31 * m.m42 + m.m23 * m.m41 * m.m32

        r42 =
            m.m11 * m.m32 * m.m43 - m.m11 * m.m42 * m.m33 - m.m12 * m.m31 * m.m43 + m.m12 * m.m41 * m.m33 + m.m13 * m.m31 * m.m42 - m.m13 * m.m41 * m.m32

        r43 =
            -m.m11 * m.m22 * m.m43 + m.m11 * m.m42 * m.m23 + m.m12 * m.m21 * m.m43 - m.m12 * m.m41 * m.m23 - m.m13 * m.m21 * m.m42 + m.m13 * m.m41 * m.m22

        r44 =
            m.m11 * m.m22 * m.m33 - m.m11 * m.m32 * m.m23 - m.m12 * m.m21 * m.m33 + m.m12 * m.m31 * m.m23 + m.m13 * m.m21 * m.m32 - m.m13 * m.m31 * m.m22

        det =
            m.m11 * r11 + m.m21 * r12 + m.m31 * r13 + m.m41 * r14

        idet =
            1 / det
    in
    if det == 0 then
        Nothing

    else
        Just
            { m11 = r11 * idet
            , m21 = r21 * idet
            , m31 = r31 * idet
            , m41 = r41 * idet
            , m12 = r12 * idet
            , m22 = r22 * idet
            , m32 = r32 * idet
            , m42 = r42 * idet
            , m13 = r13 * idet
            , m23 = r23 * idet
            , m33 = r33 * idet
            , m43 = r43 * idet
            , m14 = r14 * idet
            , m24 = r24 * idet
            , m34 = r34 * idet
            , m44 = r44 * idet
            }


{-| -}
mul : Mat4 -> Mat4 -> Mat4
mul a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31 + a.m14 * b.m41
    , m21 = a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31 + a.m24 * b.m41
    , m31 = a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31 + a.m34 * b.m41
    , m41 = a.m41 * b.m11 + a.m42 * b.m21 + a.m43 * b.m31 + a.m44 * b.m41
    , m12 = a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32 + a.m14 * b.m42
    , m22 = a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32 + a.m24 * b.m42
    , m32 = a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32 + a.m34 * b.m42
    , m42 = a.m41 * b.m12 + a.m42 * b.m22 + a.m43 * b.m32 + a.m44 * b.m42
    , m13 = a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33 + a.m14 * b.m43
    , m23 = a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33 + a.m24 * b.m43
    , m33 = a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33 + a.m34 * b.m43
    , m43 = a.m41 * b.m13 + a.m42 * b.m23 + a.m43 * b.m33 + a.m44 * b.m43
    , m14 = a.m11 * b.m14 + a.m12 * b.m24 + a.m13 * b.m34 + a.m14 * b.m44
    , m24 = a.m21 * b.m14 + a.m22 * b.m24 + a.m23 * b.m34 + a.m24 * b.m44
    , m34 = a.m31 * b.m14 + a.m32 * b.m24 + a.m33 * b.m34 + a.m34 * b.m44
    , m44 = a.m41 * b.m14 + a.m42 * b.m24 + a.m43 * b.m34 + a.m44 * b.m44
    }


{-| -}
translate : Vec3 -> Mat4
translate { x, y, z } =
    fromRows
        (vec4 1 0 0 x)
        (vec4 0 1 0 y)
        (vec4 0 0 1 z)
        (vec4 0 0 0 1)


{-| -}
scale : Vec3 -> Mat4
scale { x, y, z } =
    fromRows
        (vec4 x 0 0 0)
        (vec4 0 y 0 0)
        (vec4 0 0 z 0)
        (vec4 0 0 0 1)


{-| -}
rotate : Float -> Vec3 -> Mat4
rotate =
    Debug.todo "Mat4.rotate"



---- Projections ----


{-| -}
lookAt : { eye : Vec3, centerOfAttention : Vec3, up : Vec3 } -> Maybe Mat4
lookAt { eye, centerOfAttention, up } =
    let
        z =
            Vec3.direction { from = centerOfAttention, to = eye }

        x =
            Vec3.normalize (Vec3.cross up z)

        y =
            Vec3.normalize (Vec3.cross z x)
    in
    if x /= Vec3.zero && y /= Vec3.zero && z /= Vec3.zero then
        Just <|
            mul
                { m11 = x.x
                , m21 = y.x
                , m31 = z.x
                , m41 = 0
                , m12 = x.y
                , m22 = y.y
                , m32 = z.y
                , m42 = 0
                , m13 = x.z
                , m23 = y.z
                , m33 = z.z
                , m43 = 0
                , m14 = 0
                , m24 = 0
                , m34 = 0
                , m44 = 1
                }
                { m11 = 1
                , m21 = 0
                , m31 = 0
                , m41 = 0
                , m12 = 0
                , m22 = 1
                , m32 = 0
                , m42 = 0
                , m13 = 0
                , m23 = 0
                , m33 = 1
                , m43 = 0
                , m14 = -eye.x
                , m24 = -eye.y
                , m34 = -eye.z
                , m44 = 1
                }

    else
        Nothing


{-| -}
transform : Mat4 -> Vec4 -> Vec4
transform { m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44 } { x, y, z, w } =
    vec4
        (m11 * x + m12 * y + m13 * z + m14 * w)
        (m21 * x + m22 * y + m23 * z + m24 * w)
        (m31 * x + m32 * y + m33 * z + m34 * w)
        (m41 * x + m42 * y + m43 * z + m44 * w)



-- unwrap : b -> Maybe a -> a
-- unwrap b m =
--     case m of
--         Just a ->
--             a
--         Nothing ->
--             -- unwrap (Just (unwrap Nothing))
--             Debug.todo <| Debug.toString b
