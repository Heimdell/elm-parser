module CoreTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Core exposing (..)

suite : Test
suite =
  describe "Core combinators"
    [ describe "basics"
        [ test "pure 1 should parse" <| \_ ->
            pure 1
              |> runString ""
              |> Expect.equal (Result.Ok 1)
        
        , test "fail 1 should fail" <| \_ ->
            fail 1
              |> runString ""
              |> Expect.equal (err 1 |> at 0)

        , test "token should succeed on nonempty string" <| \_ ->
            token
              |> runString "a"
              |> Expect.equal (Result.Ok 'a')

        , test "token should fail on empty string" <| \_ ->
            token
              |> runString ""
              |> Expect.equal (noErr |> at 0)

        , test "match should succeed when string starts with it" <| \_ ->
            match 'a'
              |> runString "a"
              |> Expect.equal (Result.Ok 'a')

        , test "match should not succeed unless string starts with it" <| \_ ->
            match 'a'
              |> runString "b"
              |> Expect.equal (noErr |> at 0)
        ]

    , describe "error decorators"
        [ test "aka should replace error message" <| \_ ->
            fail 1
              |> aka 2
              |> runString ""
              |> Expect.equal (err 2 |> at 0)

        , test "while should wrap error message" <| \_ ->
            fail 1
              |> while 2
              |> runString ""
              |> Expect.equal (2 |> with (err 1) |> at 0)
        ]

    , describe "or"
        [ test "or should recover from clean failures" <| \_ ->
            fail 1
              |> or (pure 3)
              |> runString ""
              |> Expect.equal (Result.Ok 3)

        , test "or should NOT recover from dirty failures" <| \_ ->
            token
              |> use (fail 1)
              |> or  (pure 3)
              |> runString "a"
              |> Expect.equal (err 1 |> at 1)

        , test "or should NOT recover from clean success" <| \_ ->
            pure 1
              |> or (pure 2)
              |> runString "a"
              |> Expect.equal (Result.Ok 1)

        , test "or should NOT recover from dirty success" <| \_ ->
            token
              |> or (pure '3')
              |> runString "a"
              |> Expect.equal (Result.Ok 'a')
        ]

    , describe "ap"
        [ test "should properly add offset" <| \_ ->
            token
              |> omit token
              |> omit (match 'a' |> aka 'a')
              |> runString "aab"
              |> Expect.equal (err 'a' |> at 2)
        ]

    , describe "many"
        [ test "should work" <| \_ ->
            match 'a'
              |> many
              |> runString "aaab"
              |> Expect.equal (Ok ['a', 'a', 'a'])

        , test "should accept empty sequence" <| \_ ->
            match 'a'
              |> many
              |> runString "baaa"
              |> Expect.equal (Ok [])

        , test "should fail if argument fails dirtily" <| \_ ->
            match 'a'
              |> omit (match 'b' |> aka 'b')
              |> many
              |> runString "ababac"
              |> Expect.equal (err 'b' |> at 5)
        ]

    , describe "sepBy"
        [ test "should work" <| \_ ->
            match 'a'
              |> sepBy (match ',')
              |> runString "a,a,ab"
              |> Expect.equal (Ok ['a', 'a', 'a'])

        , test "should accept empty sequence" <| \_ ->
            match 'a'
              |> aka 'a'
              |> sepBy (match ',')
              |> runString "baaa"
              |> Expect.equal (Ok [])

        , test "should fail if argument fails dirtily" <| \_ ->
            match 'a'
              |> omit (match 'b' |> aka 'b')
              |> sepBy (match ',')
              |> runString "ab,ab,ac"
              |> Expect.equal (err 'b' |> at 7)
        ]

    , describe "some"
        [ test "should work" <| \_ ->
            match 'a'
              |> some
              |> runString "aaab"
              |> Expect.equal (Ok ['a', 'a', 'a'])

        , test "should NOT accept empty sequence" <| \_ ->
            match 'a'
              |> aka 'a'
              |> some
              |> runString "baaa"
              |> Expect.equal (err 'a' |> at 0)

        , test "should fail if argument fails dirtily" <| \_ ->
            match 'a'
              |> omit (match 'b' |> aka 'b')
              |> some
              |> runString "ababac"
              |> Expect.equal (err 'b' |> at 5)
        ]

    , describe "sepBy1"
        [ test "should work" <| \_ ->
            match 'a'
              |> sepBy1 (match ',')
              |> runString "a,a,ab"
              |> Expect.equal (Ok ['a', 'a', 'a'])

        , test "should NOT accept empty sequence" <| \_ ->
            match 'a'
              |> aka 'a'
              |> sepBy1 (match ',')
              |> runString "baaa"
              |> Expect.equal (err 'a' |> at 0)

        , test "should fail if argument fails dirtily" <| \_ ->
            match 'a'
              |> omit (match 'b' |> aka 'b')
              |> sepBy1 (match ',')
              |> runString "ab,ab,ac"
              |> Expect.equal (err 'b' |> at 7)
        ]

    , describe "optional"
        [ test "should work" <| \_ ->
            match 'a'
              |> optional
              |> runString "a,a,ab"
              |> Expect.equal (Ok (Just 'a'))

        , test "should accept clean failure" <| \_ ->
            match 'a'
              |> optional
              |> runString "ba,a,ab"
              |> Expect.equal (Ok Nothing)

        , test "should NOT accept dirty failure" <| \_ ->
            match 'a'
              |> omit (match 'b')
              |> aka 'b'
              |> optional
              |> runString "acaa"
              |> Expect.equal (err 'b' |> at 1)
        ]

    , describe "try"
        [ test "should make any failure clean" <| \_ ->
            match 'a'
              |> omit (match 'b')
              |> omit (fail 1)
              |> try
              |> runString "abc"
              |> Expect.equal (err 1 |> at 0)

        ]

    , describe "recursive"
        [ test "it works" <| \_ ->
            let
              lisp = recursive <| \lisp_ ->
                match '('
                  |> omit (many lisp_)
                  |> omit (match ')')
            in
              lisp
                |> runString "((()())(()()))"
                |> Expect.equal (Ok '(')
        ]

    , describe "trace"
        [ test "trace should return a part of the text covered by argument" <| \_ ->
            match 'a'
              |> omit (match 'b')
              |> many
              |> trace
              |> runString "abababc"
              |> Expect.equal (Ok "ababab")
        ]
    ]
