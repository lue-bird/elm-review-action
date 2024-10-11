module Review.ActionTest exposing (all)

import Review.Action
import Review.Test
import Test


all : Test.Test
all =
    Test.describe "Review.Action"
        [ Test.describe "!remove"
            [ Test.test "value declaration, no module documentation, exposing all"
                (\() ->
                    """module A exposing (..)

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)


b =
    2
"""
                            ]
                )
            , Test.test "function declaration, no module documentation, exposing all"
                (\() ->
                    """module A exposing (..)

a x {-!remove-} =
    x

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)


b =
    2
"""
                            ]
                )
            , Test.test "type alias declaration with parameters, no module documentation, exposing all"
                (\() ->
                    """module A exposing (..)

type alias T a {-!remove-} =
    a

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration T"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration T triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "T"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)


b =
    2
"""
                            ]
                )
            , Test.test "choice type declaration with parameters, no module documentation, exposing all"
                (\() ->
                    """module A exposing (..)

type T a {-!remove-}
    = V a

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration T"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration T triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "T"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, module documentation without correct @docs, exposing all"
                (\() ->
                    """module A exposing (..)

{-| A

@docsa
@docs ab, ba, aa

-}

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 10, column = 1 }, end = { row = 10, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

{-| A

@docsa
@docs ab, ba, aa

-}


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, module documentation first in multi @docs line, exposing all"
                (\() ->
                    """module A exposing (..)

{-| A

@docs a, b

-}

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 9, column = 1 }, end = { row = 9, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

{-| A

@docs b

-}


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, module documentation last in multi @docs line, exposing all"
                (\() ->
                    """module A exposing (..)

{-| A

@docs b, a

-}

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 9, column = 1 }, end = { row = 9, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

{-| A

@docs b

-}


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, module documentation in single @docs line, exposing all"
                (\() ->
                    """module A exposing (..)

{-| A

@docs a

-}

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 9, column = 1 }, end = { row = 9, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

{-| A


-}


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, without module documentation, first in exposing explicit"
                (\() ->
                    """module A exposing (a, b)

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (b)


b =
    2
"""
                            ]
                )
            , Test.test "value declaration, without module documentation, last in exposing explicit"
                (\() ->
                    """module A exposing (b, a)

a =
    --!remove
    1

b =
    2
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "remove declaration a"
                                , details = [ "The action command !remove placed in a comment at the top of the declaration a triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "a"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (b)


b =
    2
"""
                            ]
                )
            ]
        , Test.describe "!inline"
            [ Test.test "fully applied, module-locally declared, variable pattern, simple argument"
                (\() ->
                    """module A exposing (..)
add2 x =
    x + 2

a =
        add2
            {-!inline-}
            1
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "inline add2"
                                , details = [ "The action command !inline placed in a comment after add2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "add2"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 6, column = 9 }, end = { row = 6, column = 13 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
add2 x =
    x + 2

a =
        (1 + 2)
"""
                            ]
                )
            , Test.test "partially applied, module-locally declared, variable pattern, simple argument"
                (\() ->
                    """module A exposing (..)
multiplyAndAdd2 x y =
    x * y + 2

a =
        multiplyAndAdd2
            {-!inline-}
            1
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "inline multiplyAndAdd2"
                                , details = [ "The action command !inline placed in a comment after multiplyAndAdd2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "multiplyAndAdd2"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 6, column = 9 }, end = { row = 6, column = 24 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
multiplyAndAdd2 x y =
    x * y + 2

a =
        (\\y ->
            1 * y + 2)
"""
                            ]
                )
            , Test.test "fully applied, declared in imported module, variable pattern, simple argument"
                (\() ->
                    [ """module Add2 exposing (add2)
add2 x =
    x + 2
"""
                    , """module A exposing (..)
import Add2

a =
        Add2.add2
            {-!inline-}
            1
"""
                    ]
                        |> Review.Test.runOnModules Review.Action.rule
                        |> Review.Test.expectErrorsForModules
                            [ ( "A"
                              , [ Review.Test.error
                                    { message = "inline Add2.add2"
                                    , details = [ "The action command !inline placed in a comment after Add2.add2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                    , under = "Add2.add2"
                                    }
                                    |> Review.Test.whenFixed
                                        """module A exposing (..)
import Add2

a =
        (1 + 2)
"""
                                ]
                              )
                            ]
                )
            , Test.test "fully applied, declared in imported module with new imports, variable pattern, simple argument"
                (\() ->
                    [ """module Add2 exposing (add2)
import Array
add2 x =
    x + Array.length (Array.fromList [ (), () ])
"""
                    , """module A exposing (..)
import Add2

a =
        Add2.add2
            {-!inline-}
            1
"""
                    ]
                        |> Review.Test.runOnModules Review.Action.rule
                        |> Review.Test.expectErrorsForModules
                            [ ( "A"
                              , [ Review.Test.error
                                    { message = "inline Add2.add2"
                                    , details = [ "The action command !inline placed in a comment after Add2.add2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                    , under = "Add2.add2"
                                    }
                                    |> Review.Test.whenFixed
                                        """module A exposing (..)
import Add2
import Array

a =
        (1 + Array.length (Array.fromList [ (), () ]))
"""
                                ]
                              )
                            ]
                )
            , Test.test "fully applied, ignore pattern"
                (\() ->
                    """module A exposing (..)
always2 _ =
    2

a =
        always2
            {-!inline-}
            1
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "inline always2"
                                , details = [ "The action command !inline placed in a comment after always2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "always2"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 6, column = 9 }, end = { row = 6, column = 16 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
always2 _ =
    2

a =
        (2)
"""
                            ]
                )
            , Test.test "fully applied, non-simple pattern"
                (\() ->
                    """module A exposing (..)
add2 (_ as x) =
    x + 2

a =
        add2
            {-!inline-}
            1
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "inline add2"
                                , details = [ "The action command !inline placed in a comment after add2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "add2"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 6, column = 9 }, end = { row = 6, column = 13 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
add2 (_ as x) =
    x + 2

a =
        (let
            (_ as x) =
                1
         in
         x + 2)
"""
                            ]
                )
            , Test.test "fully applied, variable pattern, non-simple argument"
                (\() ->
                    """module A exposing (..)
add2 x =
    x + 2

a =
        add2
            {-!inline-}
            (1 * 1)
"""
                        |> Review.Test.run Review.Action.rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "inline add2"
                                , details = [ "The action command !inline placed in a comment after add2 in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment." ]
                                , under = "add2"
                                }
                                |> Review.Test.atExactly
                                    { start = { row = 6, column = 9 }, end = { row = 6, column = 13 } }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
add2 x =
    x + 2

a =
        (let
            x =
                (1 * 1)
         in
         x + 2)
"""
                            ]
                )
            ]
        ]
