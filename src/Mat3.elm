module Mat3 exposing
    ( Mat3
    , identity
    , rotate
    , scale
    , transformPoint
    , transformVector
    , translate
    )

import Vec2 exposing (Vec2, vec2)
import Vec3 exposing (Vec3, vec3)


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


transformPoint : Vec2 -> Mat3 -> Vec2
transformPoint v2 mat3 =
    mulVector (vec3 v2.x v2.y 1) mat3
        |> Vec2.fromHomogeneous


transformVector : Vec2 -> Mat3 -> Vec2
transformVector v2 mat3 =
    mulVector (vec3 v2.x v2.y 0) mat3
        |> drop3rdCoordinate


mulVector : Vec3 -> Mat3 -> Vec3
mulVector { x, y, z } { m11, m12, m13, m21, m22, m23, m31, m32, m33 } =
    vec3
        (m11 * x + m12 * y + m13 * z)
        (m21 * x + m22 * y + m23 * z)
        (m31 * x + m32 * y + m33 * z)



---- MISC ----


drop3rdCoordinate : Vec3 -> Vec2
drop3rdCoordinate v3 =
    vec2 v3.x v3.y
