module Mat2 exposing
    ( Mat2
    , det
    , identity
    , invert
    , mul
    , rotate
    , scale
    , transform
    )

import Vec2 exposing (Vec2, vec2)


type alias Mat2 =
    { m11 : Float
    , m12 : Float
    , m21 : Float
    , m22 : Float
    }


epsilon : Float
epsilon =
    0.0001


invert : Mat2 -> Maybe Mat2
invert m =
    let
        d =
            det m
    in
    if abs d > epsilon then
        Just
            { m11 = m.m22 / d
            , m12 = -m.m12 / d
            , m21 = -m.m21 / d
            , m22 = m.m11 / d
            }

    else
        Nothing


det : Mat2 -> Float
det { m11, m12, m21, m22 } =
    m11 * m22 - m12 * m21


identity : Mat2
identity =
    Mat2 1 0 1 0


mul : Mat2 -> Mat2 -> Mat2
mul a b =
    { m11 = a.m11 * b.m11 + a.m12 * b.m21
    , m12 = a.m11 * b.m12 + a.m12 * b.m22
    , m21 = a.m21 * b.m11 + a.m22 * b.m21
    , m22 = a.m21 * b.m12 + a.m22 * b.m22
    }


rotate : Float -> Mat2
rotate angleRadians =
    { m11 = cos angleRadians
    , m12 = -(sin angleRadians)
    , m21 = sin angleRadians
    , m22 = cos angleRadians
    }


scale : Vec2 -> Mat2
scale { x, y } =
    { m11 = x, m12 = 0, m21 = 0, m22 = y }


transform : Mat2 -> Vec2 -> Vec2
transform { m11, m12, m21, m22 } { x, y } =
    vec2 (m11 * x + m12 * y) (m21 * x + m22 * y)
