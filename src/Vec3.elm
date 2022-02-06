module Vec3 exposing
    ( Vec3
    , add
    , dot
    , length
    , lengthSquared
    , negate
    , normalize
    , setX
    , setY
    , setZ
    , vec3
    , zero
    )


type alias Vec3 =
    { x : Float
    , y : Float
    , z : Float
    }


vec3 : Float -> Float -> Float -> Vec3
vec3 =
    Vec3



---- Constants ----


zero : Vec3
zero =
    vec3 0 0 0



---- Setters ----


setX : Float -> Vec3 -> Vec3
setX x vec =
    { vec | x = x }


setY : Float -> Vec3 -> Vec3
setY y vec =
    { vec | y = y }


setZ : Float -> Vec3 -> Vec3
setZ z vec =
    { vec | z = z }



---- Operations ----


add : Vec3 -> Vec3 -> Vec3
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    , z = v1.z + v2.z
    }


negate : Vec3 -> Vec3
negate v =
    scale -1 v


scale : Float -> Vec3 -> Vec3
scale factor v =
    { x = factor * v.x
    , y = factor * v.y
    , z = factor * v.z
    }


dot : Vec3 -> Vec3 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y + v1.z * v2.z


{-| -}
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


length : Vec3 -> Float
length v =
    Basics.sqrt (lengthSquared v)


lengthSquared : Vec3 -> Float
lengthSquared v =
    v.x ^ 2 + v.y ^ 2 + v.z ^ 3
