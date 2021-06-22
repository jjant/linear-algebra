module Vec2 exposing
    ( Vec2
    , add
    , angle
    , cross
    , direction
    , distance
    , distanceSquared
    , dot
    , down
    , left
    , lengthSquared
    , lerp
    , midpoint
    , negate
    , normalize
    , right
    , rotateCCW
    , scale
    , scaleX
    , scaleY
    , setX
    , setY
    , sub
    , toString
    , up
    , vec2
    , zero
    )


type alias Vec2 =
    { x : Float
    , y : Float
    }


setX : Float -> Vec2 -> Vec2
setX x { y } =
    { x = x
    , y = y
    }


setY : Float -> Vec2 -> Vec2
setY y { x } =
    { x = x
    , y = y
    }


vec2 : Float -> Float -> Vec2
vec2 =
    Vec2


add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


{-| sub v1 v2 computes v1 - v2.
Can be easily confused in pipelines like

    v2
        |> sub v1

I think both alternatives are confusing, so I chose this one rather arbitrarily.

-}
sub : Vec2 -> Vec2 -> Vec2
sub v1 v2 =
    { x = v1.x - v2.x
    , y = v1.y - v2.y
    }


scale : Float -> Vec2 -> Vec2
scale factor vec =
    { x = vec.x * factor
    , y = vec.y * factor
    }


distance : Vec2 -> Vec2 -> Float
distance v1 v2 =
    length (sub v1 v2)


distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared v1 v2 =
    lengthSquared (sub v1 v2)


{-| Computes |v| for a vector v.
If you need the square of this value, use Vec2.lengthSquared for efficiency.
-}
length : Vec2 -> Float
length vec =
    Basics.sqrt (lengthSquared vec)


{-| Computes |v|^2 for a vector v.
Faster than `length` because it saves a square root operation.
-}
lengthSquared : Vec2 -> Float
lengthSquared { x, y } =
    x ^ 2 + y ^ 2


dot : Vec2 -> Vec2 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


{-| Normalizes a vector v, returning v/|v|.
If the vector is 0, it returns 0.
-}
normalize : Vec2 -> Vec2
normalize vec =
    let
        len =
            length vec
    in
    if len > 0 then
        scale (1 / len) vec

    else
        vec


{-| Computes -v given a vector `v`.
-}
negate : Vec2 -> Vec2
negate { x, y } =
    { x = -x
    , y = -y
    }


{-| Rotates a given vector by an angle in radians, in counterclockwise fashion.
-}
rotateCCW : Float -> Vec2 -> Vec2
rotateCCW angleRadians { x, y } =
    let
        co =
            Basics.cos angleRadians

        si =
            Basics.sin angleRadians
    in
    vec2 (co * x - si * y) (si * x + co * y)


{-| Returns the z-coordinate of the cross product when interpreting the 2d vectors as
3d vectors of the shape (v.x, v.y, 0).

    cross v1 v2 == (( v1.x, v1.y, 0 ) x ( v2.x, v2.y, 0 )).z

This operation is usually used to calculate the torque created by a force in 2D.

-}
cross : Vec2 -> Vec2 -> Float
cross v1 v2 =
    v1.x * v2.y - v1.y * v2.x


{-| Mostly for debugging purposes.
-}
toString : Vec2 -> String
toString { x, y } =
    "{ "
        ++ "x = "
        ++ String.fromFloat x
        ++ ", y = "
        ++ String.fromFloat y
        ++ " }"


scaleX : Float -> Vec2 -> Vec2
scaleX xScale { x, y } =
    { x = xScale * x
    , y = y
    }


scaleY : Float -> Vec2 -> Vec2
scaleY yScale { x, y } =
    { x = x
    , y = yScale * y
    }


{-| Returns a normalized vector pointing from `from` to `to`.
-}
direction : { from : Vec2, to : Vec2 } -> Vec2
direction { from, to } =
    sub to from
        |> normalize


{-| Returns the midpoint between two points. This operation is commutative.
-}
midpoint : Vec2 -> Vec2 -> Vec2
midpoint v1 v2 =
    scale (1 / 2) (add v1 v2)


{-| Linearly interpolate `from` to `to` with a [0, 1] parameter.
-}
lerp : { from : Vec2, to : Vec2 } -> Float -> Vec2
lerp { from, to } t =
    -- ((1-t)*from) + (t*to)
    add (scale (1 - t) from) (scale t to)


angle : Vec2 -> Float
angle { x, y } =
    Basics.atan2 y x



---- Commonly use vectors ----


zero : Vec2
zero =
    vec2 0 0


up : Vec2
up =
    vec2 0 1


down : Vec2
down =
    vec2 0 -1


right : Vec2
right =
    vec2 1 0


left : Vec2
left =
    vec2 -1 0
