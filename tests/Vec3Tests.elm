module Vec3Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, test)
import Vec3 exposing (Vec3, vec3)


addTests : Test
addTests =
    describe "Vec3.add"
        [ Test.fuzz vec3Fuzzer "with zero" <|
            \v ->
                Vec3.add v Vec3.zero
                    |> Expect.equal v
        , test "it works" <|
            \_ ->
                Vec3.add (vec3 3 4 5) (vec3 0.1 0.2 0.3)
                    |> within (Relative 0.0001) (vec3 3.1 4.2 5.3)
        ]


vec3Fuzzer : Fuzzer Vec3
vec3Fuzzer =
    Fuzz.map3 Vec3
        Fuzz.float
        Fuzz.float
        Fuzz.float


within : FloatingPointTolerance -> Vec3 -> Vec3 -> Expectation
within fp v1 v2 =
    Expect.all
        [ \_ -> Expect.within fp v1.x v2.x
        , \_ -> Expect.within fp v1.y v2.y
        , \_ -> Expect.within fp v1.z v2.z
        ]
        ()
