module Util exposing
    ( compareFloat
    , compareMaybes
    , comparePrecision
    , ignoreEquals
    , ignoreZero
    )

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


comparePrecision : Float -> Float -> Float -> Expectation
comparePrecision precision a b =
    if isNaN a && isNaN b then
        Expect.pass

    else
        Expect.within (Absolute precision) a b


compareMaybes : (a -> b -> Expectation) -> Maybe a -> Maybe b -> Expectation
compareMaybes f ma mb =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            f a b

        ( Nothing, Nothing ) ->
            Expect.pass

        _ ->
            Expect.fail "Differing maybes"
