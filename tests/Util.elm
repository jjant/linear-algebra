module Util exposing (compareFloat, ignoreEquals, ignoreZero)

import Expect exposing (Expectation, FloatingPointTolerance(..))


epsilon : Float
epsilon =
    2 ^ -52


compareFloat : Float -> Float -> Expectation
compareFloat a b =
    if isNaN a && isNaN b then
        Expect.pass

    else
        Expect.within (Absolute epsilon) a b


ignoreZero : a -> (a -> a) -> a -> a
ignoreZero zero f v =
    if v == zero then
        v

    else
        f v


ignoreEquals : a -> (a -> a -> a) -> a -> a -> a
ignoreEquals zero f v1 v2 =
    if v1 == v2 then
        zero

    else
        f v1 v2
