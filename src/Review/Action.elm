module Review.Action exposing (rule)

{-| Transform code at `!action` comments

@docs rule

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import FastDict
import Review.Fix
import Review.ModuleNameLookupTable
import Review.Rule


{-| Applies code transformations marked by !action comments

    config =
        [ Review.Action.rule
        ]


### available actions

  - `function {-!inline-} arguments`: substitute its arguments into the implementation code

-}
rule : Review.Rule.Rule
rule =
    Review.Rule.newProjectRuleSchema "Review.Action" initialContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleVisitor
            (\moduleVisitor ->
                moduleVisitor
                    |> Review.Rule.withCommentsVisitor
                        (\comments context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , comments = comments
                              , declaredExpressionImplementations = context.declaredExpressionImplementations
                              }
                            )
                        )
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarations context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , comments = context.comments
                              , declaredExpressionImplementations =
                                    declarations
                                        |> List.foldl
                                            (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                                case declaration of
                                                    Elm.Syntax.Declaration.FunctionDeclaration expressionDeclaration ->
                                                        let
                                                            implementation : Elm.Syntax.Expression.FunctionImplementation
                                                            implementation =
                                                                expressionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                        in
                                                        soFar
                                                            |> FastDict.insert (implementation.name |> Elm.Syntax.Node.value)
                                                                { parameters = implementation.arguments
                                                                , result = implementation.expression
                                                                }

                                                    _ ->
                                                        soFar
                                            )
                                            FastDict.empty
                              }
                            )
                        )
                    |> Review.Rule.withExpressionEnterVisitor
                        (\expressionNode context ->
                            ( case expressionNode of
                                Elm.Syntax.Node.Node callRange (Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node referenceRange (Elm.Syntax.Expression.FunctionOrValue _ referenceUnqualified)) :: argument0 :: argument1Up)) ->
                                    checkCall
                                        { range = callRange
                                        , referenceRange = referenceRange
                                        , referenceUnqualified = referenceUnqualified
                                        , argument0 = argument0
                                        , argument1Up = argument1Up
                                        }
                                        context

                                _ ->
                                    []
                            , context
                            )
                        )
            )
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromModuleToProject = moduleToProjectContext
            , fromProjectToModule = projectToModuleContext
            , foldProjectContexts = projectContextMerge
            }
        |> Review.Rule.fromProjectRuleSchema


checkCall :
    { referenceRange : Elm.Syntax.Range.Range
    , argument0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    , referenceUnqualified : String
    , range : { end : Elm.Syntax.Range.Location, start : Elm.Syntax.Range.Location }
    , argument1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
    }
    -> ModuleContext
    -> List (Review.Rule.Error {})
checkCall call context =
    let
        hasInlineMark : Bool
        hasInlineMark =
            case commentsInRange { start = call.referenceRange.end, end = call.argument0 |> Elm.Syntax.Node.range |> .start } context.comments of
                [] ->
                    False

                comment0 :: comment1Up ->
                    (comment0 :: comment1Up)
                        |> List.any (\comment -> comment |> String.contains "!inline")
    in
    if Basics.not hasInlineMark then
        []

    else
        case Review.ModuleNameLookupTable.moduleNameAt context.moduleOriginLookup call.referenceRange of
            Nothing ->
                []

            Just referenceModuleOrigin ->
                let
                    maybeReferenceImplementationModule :
                        Maybe
                            { declaredExpressionImplementations :
                                FastDict.Dict
                                    String
                                    { parameters : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                                    , result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                                    }
                            , extractSourceCode : Elm.Syntax.Range.Range -> String
                            }
                    maybeReferenceImplementationModule =
                        case referenceModuleOrigin of
                            [] ->
                                Just
                                    { declaredExpressionImplementations = context.declaredExpressionImplementations
                                    , extractSourceCode = context.extractSourceCode
                                    }

                            moduleNamePart0 :: moduleNamePart1Up ->
                                context.importedModules |> FastDict.get (moduleNamePart0 :: moduleNamePart1Up)
                in
                case maybeReferenceImplementationModule of
                    Nothing ->
                        []

                    Just referenceImplementationModule ->
                        case referenceImplementationModule.declaredExpressionImplementations |> FastDict.get call.referenceUnqualified of
                            Nothing ->
                                []

                            Just referenceImplementation ->
                                let
                                    referenceString : String
                                    referenceString =
                                        qualifiedToString { qualification = referenceModuleOrigin, unqualified = call.referenceUnqualified }

                                    indentation : Int
                                    indentation =
                                        call.range.start.column - 1

                                    appliedArgumentCount : Int
                                    appliedArgumentCount =
                                        1 + (call.argument1Up |> List.length)

                                    filledInList :
                                        { letDestructuringListReverse : List { pattern : String, expression : String }
                                        , variables : List { name : String, replacement : String }
                                        }
                                    filledInList =
                                        List.map2
                                            (\curriedParameter replacement ->
                                                { curriedParameter = curriedParameter
                                                , replacement = replacement
                                                }
                                            )
                                            referenceImplementation.parameters
                                            (call.argument0 :: call.argument1Up)
                                            |> List.foldl
                                                (\filledIn soFar ->
                                                    let
                                                        (Elm.Syntax.Node.Node curriedParameterRange curriedParameterPattern) =
                                                            filledIn.curriedParameter
                                                    in
                                                    case curriedParameterPattern of
                                                        Elm.Syntax.Pattern.AllPattern ->
                                                            soFar

                                                        Elm.Syntax.Pattern.UnitPattern ->
                                                            soFar

                                                        Elm.Syntax.Pattern.VarPattern name ->
                                                            if filledIn.replacement |> Elm.Syntax.Node.value |> expressionIsSimple then
                                                                { letDestructuringListReverse = soFar.letDestructuringListReverse
                                                                , variables =
                                                                    { name = name
                                                                    , replacement =
                                                                        context.extractSourceCode
                                                                            (filledIn.replacement |> Elm.Syntax.Node.range)
                                                                    }
                                                                        :: soFar.variables
                                                                }

                                                            else
                                                                { variables = soFar.variables
                                                                , letDestructuringListReverse =
                                                                    { pattern = referenceImplementationModule.extractSourceCode curriedParameterRange
                                                                    , expression =
                                                                        context.extractSourceCode (filledIn.replacement |> Elm.Syntax.Node.range)
                                                                    }
                                                                        :: soFar.letDestructuringListReverse
                                                                }

                                                        _ ->
                                                            { variables = soFar.variables
                                                            , letDestructuringListReverse =
                                                                { pattern = referenceImplementationModule.extractSourceCode curriedParameterRange
                                                                , expression =
                                                                    context.extractSourceCode (filledIn.replacement |> Elm.Syntax.Node.range)
                                                                }
                                                                    :: soFar.letDestructuringListReverse
                                                            }
                                                )
                                                { variables = []
                                                , letDestructuringListReverse = []
                                                }

                                    resultString : String
                                    resultString =
                                        referenceImplementationModule.extractSourceCode
                                            (referenceImplementation.result
                                                |> Elm.Syntax.Node.range
                                            )
                                            |> expressionStringSubstituteVariables
                                                filledInList.variables
                                                referenceImplementation.result
                                            |> unindent
                                            |> indentBy (indentation + 1)
                                in
                                [ Review.Rule.errorWithFix
                                    { message = "inline " ++ referenceString
                                    , details =
                                        [ "The action command !inline placed in a comment after "
                                            ++ referenceString
                                            ++ " in the call triggers the suggestion of this automatic fix. Either apply the fix or remove the comment."
                                        ]
                                    }
                                    call.referenceRange
                                    [ Review.Fix.replaceRangeBy
                                        call.range
                                        ("("
                                            ++ (case filledInList.letDestructuringListReverse of
                                                    [] ->
                                                        ""

                                                    letDestructuringLast :: letDestructuringBeforeLastToFirst ->
                                                        "let\n"
                                                            ++ ((letDestructuringLast :: letDestructuringBeforeLastToFirst)
                                                                    |> List.reverse
                                                                    |> List.map
                                                                        (\letDestructuring ->
                                                                            String.repeat (indentation + 4) " "
                                                                                ++ letDestructuring.pattern
                                                                                ++ " =\n"
                                                                                ++ String.repeat (indentation + 8) " "
                                                                                ++ (letDestructuring.expression
                                                                                        |> indentBy (indentation + 8)
                                                                                   )
                                                                        )
                                                                    |> String.join "\n\n"
                                                               )
                                                            ++ "\n"
                                                            ++ String.repeat (indentation + 1) " "
                                                            ++ "in\n"
                                                            ++ String.repeat (indentation + 1) " "
                                               )
                                            ++ (case
                                                    referenceImplementation.parameters
                                                        |> List.drop appliedArgumentCount
                                                of
                                                    [] ->
                                                        resultString

                                                    curriedParameter0 :: curriedParameter1Up ->
                                                        "\\"
                                                            ++ ((curriedParameter0 :: curriedParameter1Up)
                                                                    |> List.map
                                                                        (\(Elm.Syntax.Node.Node curriedParameterRange _) ->
                                                                            referenceImplementationModule.extractSourceCode curriedParameterRange
                                                                                ++ " "
                                                                        )
                                                                    |> String.concat
                                                               )
                                                            ++ "->\n"
                                                            ++ String.repeat (indentation + 4) " "
                                                            ++ (resultString |> indentBy 4)
                                               )
                                            ++ ")"
                                        )
                                    ]
                                ]


type alias ProjectContext =
    { byModule :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { extractSourceCode : Elm.Syntax.Range.Range -> String
            , declaredExpressionImplementations :
                FastDict.Dict
                    String
                    { parameters : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    }
            }
    }


type alias ModuleContext =
    { extractSourceCode : Elm.Syntax.Range.Range -> String
    , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , comments : List (Elm.Syntax.Node.Node String)
    , declaredExpressionImplementations :
        FastDict.Dict
            String
            { parameters : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
            , result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
            }
    , importedModules :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { extractSourceCode : Elm.Syntax.Range.Range -> String
            , declaredExpressionImplementations :
                FastDict.Dict
                    String
                    { parameters : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    , result : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
                    }
            }
    }


initialContext : ProjectContext
initialContext =
    { byModule = FastDict.empty }


projectContextMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextMerge a b =
    { byModule = FastDict.union a.byModule b.byModule
    }


projectToModuleContext : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContext =
    Review.Rule.initContextCreator
        (\moduleOriginLookup extractSourceCode projectContext ->
            { moduleOriginLookup = moduleOriginLookup
            , extractSourceCode = extractSourceCode
            , comments = []
            , declaredExpressionImplementations = FastDict.empty
            , importedModules = projectContext.byModule
            }
        )
        |> Review.Rule.withModuleNameLookupTable
        |> Review.Rule.withSourceCodeExtractor


moduleToProjectContext : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContext =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            { byModule =
                FastDict.singleton moduleName
                    { extractSourceCode = moduleContext.extractSourceCode
                    , declaredExpressionImplementations = moduleContext.declaredExpressionImplementations
                    }
            }
        )
        |> Review.Rule.withModuleName


expressionStringSubstituteVariables :
    List { name : String, replacement : String }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> String
    -> String
expressionStringSubstituteVariables variableSubstitutions expressionNode expressionSource =
    let
        maximumIndentation : Int
        maximumIndentation =
            expressionSource
                |> String.lines
                |> List.map lineIndentation
                |> List.maximum
                |> Maybe.withDefault 1
    in
    expressionSource
        |> sourceApplyEdits
            (variableSubstitutions
                |> List.concatMap
                    (\variableSubstitution ->
                        let
                            replacement : String
                            replacement =
                                variableSubstitution.replacement |> indentBy maximumIndentation
                        in
                        expressionNode
                            |> expressionVariableUsesOf variableSubstitution.name
                            |> List.map
                                (\variableUseRange ->
                                    { range =
                                        variableUseRange
                                            |> rangeAsRelativeTo
                                                (expressionNode |> Elm.Syntax.Node.range |> .start)
                                    , replacement = replacement
                                    }
                                )
                    )
            )


rangeAsRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range)
rangeAsRelativeTo baseStart offsetRange =
    { start = offsetRange.start |> locationRelativeTo baseStart
    , end = offsetRange.end |> locationRelativeTo baseStart
    }


locationRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location)
locationRelativeTo baseStart absoluteLocation =
    { row = absoluteLocation.row - baseStart.row + 1
    , column =
        if absoluteLocation.row == baseStart.row then
            absoluteLocation.column - baseStart.column + 1

        else
            absoluteLocation.column
    }


commentsInRange : Elm.Syntax.Range.Range -> List (Elm.Syntax.Node.Node String) -> List String
commentsInRange range sortedComments =
    case sortedComments of
        [] ->
            []

        (Elm.Syntax.Node.Node headCommentRange headComment) :: tailComments ->
            case Elm.Syntax.Range.compareLocations headCommentRange.start range.start of
                LT ->
                    commentsInRange range tailComments

                EQ ->
                    headComment :: commentsInRange range tailComments

                GT ->
                    case Elm.Syntax.Range.compareLocations headCommentRange.end range.end of
                        GT ->
                            []

                        LT ->
                            headComment :: commentsInRange range tailComments

                        EQ ->
                            headComment :: commentsInRange range tailComments


qualifiedToString : { qualification : List String, unqualified : String } -> String
qualifiedToString qualified =
    case qualified.qualification of
        [] ->
            qualified.unqualified

        moduleNamePart0 :: moduleNamePart1Up ->
            (moduleNamePart0 :: moduleNamePart1Up |> String.join ".")
                ++ "."
                ++ qualified.unqualified


sourceApplyEdits :
    List { range : Elm.Syntax.Range.Range, replacement : String }
    -> String
    -> String
sourceApplyEdits fixes sourceCode =
    fixes
        |> List.sortWith
            (\a b ->
                -- flipped order
                Elm.Syntax.Range.compareLocations b.range.start a.range.start
            )
        |> List.foldl sourceLinesApplyEdit (sourceCode |> String.lines)
        |> String.join "\n"


sourceLinesApplyEdit : { range : Elm.Syntax.Range.Range, replacement : String } -> (List String -> List String)
sourceLinesApplyEdit replace lines =
    let
        linesBefore : List String
        linesBefore =
            lines |> List.take (replace.range.start.row - 1)

        linesAfter : List String
        linesAfter =
            lines |> List.drop replace.range.end.row

        startLine : String
        startLine =
            lines
                |> listElementAtIndex (replace.range.start.row - 1)
                |> Maybe.withDefault ""
                |> stringUnicodeLeft (replace.range.start.column - 1)

        endLine : String
        endLine =
            lines
                |> listElementAtIndex (replace.range.end.row - 1)
                |> Maybe.withDefault ""
                |> stringUnicodeDropLeft (replace.range.end.column - 1)
    in
    linesBefore
        ++ (replace.replacement
                |> String.lines
                |> listHeadAlter (\replacementFirstLine -> startLine ++ replacementFirstLine)
                |> listLastAlter (\replacementLastLine -> replacementLastLine ++ endLine)
           )
        ++ linesAfter


stringUnicodeLeft : Int -> String -> String
stringUnicodeLeft n string =
    string
        |> String.toList
        |> List.take n
        |> String.fromList


stringUnicodeDropLeft : Int -> String -> String
stringUnicodeDropLeft n string =
    string
        |> String.toList
        |> List.drop n
        |> String.fromList


unindent : String -> String
unindent string =
    case string |> String.lines |> List.map lineIndentation |> List.minimum of
        Nothing ->
            string

        Just minimumIndentation ->
            string
                |> String.lines
                |> List.map (\line -> line |> String.dropLeft minimumIndentation)
                |> String.join "\n"


indentBy : Int -> String -> String
indentBy indentation string =
    case string |> String.lines of
        [] ->
            ""

        line0 :: line1Up ->
            (line0
                :: (line1Up
                        |> List.map
                            (\line ->
                                String.repeat indentation " " ++ line
                            )
                   )
            )
                |> String.join "\n"


lineIndentation : String -> Int
lineIndentation line =
    line
        |> String.foldl
            (\char soFar ->
                if soFar.onlySpacesSoFar then
                    case char of
                        ' ' ->
                            { spaceCount = soFar.spaceCount + 1, onlySpacesSoFar = True }

                        _ ->
                            { spaceCount = soFar.spaceCount, onlySpacesSoFar = False }

                else
                    soFar
            )
            { spaceCount = 0, onlySpacesSoFar = True }
        |> .spaceCount


expressionIsSimple : Elm.Syntax.Expression.Expression -> Bool
expressionIsSimple expression =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            True

        Elm.Syntax.Expression.Application _ ->
            False

        Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
            False

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            True

        Elm.Syntax.Expression.IfBlock _ _ _ ->
            False

        Elm.Syntax.Expression.PrefixOperator _ ->
            True

        Elm.Syntax.Expression.Operator _ ->
            True

        Elm.Syntax.Expression.Integer _ ->
            True

        Elm.Syntax.Expression.Hex _ ->
            True

        Elm.Syntax.Expression.Floatable _ ->
            True

        Elm.Syntax.Expression.Negation (Elm.Syntax.Node.Node _ negated) ->
            expressionIsSimple negated

        Elm.Syntax.Expression.Literal _ ->
            True

        Elm.Syntax.Expression.CharLiteral _ ->
            True

        Elm.Syntax.Expression.TupledExpression _ ->
            False

        Elm.Syntax.Expression.ParenthesizedExpression (Elm.Syntax.Node.Node _ inParens) ->
            expressionIsSimple inParens

        Elm.Syntax.Expression.LetExpression _ ->
            False

        Elm.Syntax.Expression.CaseExpression _ ->
            False

        Elm.Syntax.Expression.LambdaExpression _ ->
            False

        Elm.Syntax.Expression.RecordExpr fields ->
            case fields of
                [] ->
                    True

                _ :: _ ->
                    False

        Elm.Syntax.Expression.ListExpr elements ->
            case elements of
                [] ->
                    True

                _ :: _ ->
                    False

        Elm.Syntax.Expression.RecordAccess (Elm.Syntax.Node.Node _ recordExpression) _ ->
            expressionIsSimple recordExpression

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            True

        Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
            False

        Elm.Syntax.Expression.GLSLExpression _ ->
            False


expressionVariableUsesOf :
    String
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List Elm.Syntax.Range.Range
expressionVariableUsesOf variableNameToFind expressionNode =
    case expressionNode of
        Elm.Syntax.Node.Node variableRange (Elm.Syntax.Expression.FunctionOrValue [] unqualified) ->
            if variableNameToFind == unqualified then
                [ variableRange ]

            else
                []

        nonVariable ->
            nonVariable
                |> expressionSubs
                |> List.concatMap (\sub -> sub |> expressionVariableUsesOf variableNameToFind)


{-| All surface-level child expressions
-}
expressionSubs :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionSubs (Elm.Syntax.Node.Node _ expression) =
    case expression of
        Elm.Syntax.Expression.Application expressions ->
            expressions

        Elm.Syntax.Expression.ListExpr elements ->
            elements

        Elm.Syntax.Expression.RecordExpr fields ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) fields

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) setters

        Elm.Syntax.Expression.ParenthesizedExpression expr ->
            [ expr ]

        Elm.Syntax.Expression.OperatorApplication _ direction left right ->
            case direction of
                Elm.Syntax.Infix.Left ->
                    [ left, right ]

                Elm.Syntax.Infix.Right ->
                    [ right, left ]

                Elm.Syntax.Infix.Non ->
                    [ left, right ]

        Elm.Syntax.Expression.IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        Elm.Syntax.Expression.LetExpression letIn ->
            List.foldr
                (\declaration soFar ->
                    case Elm.Syntax.Node.value declaration of
                        Elm.Syntax.Expression.LetFunction function ->
                            (function.declaration
                                |> Elm.Syntax.Node.value
                                |> .expression
                            )
                                :: soFar

                        Elm.Syntax.Expression.LetDestructuring _ expr ->
                            expr :: soFar
                )
                [ letIn.expression ]
                letIn.declarations

        Elm.Syntax.Expression.CaseExpression caseOf ->
            caseOf.expression
                :: List.map (\( _, caseExpression ) -> caseExpression) caseOf.cases

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.Negation expr ->
            [ expr ]

        Elm.Syntax.Expression.RecordAccess expr _ ->
            [ expr ]

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


listLastAlter : (a -> a) -> (List a -> List a)
listLastAlter elementAlter lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (elementAlter first :: rest)


listHeadAlter : (a -> a) -> (List a -> List a)
listHeadAlter headAlter list =
    case list of
        [] ->
            []

        head :: tail ->
            (head |> headAlter) :: tail


listElementAtIndex : Int -> (List a -> Maybe a)
listElementAtIndex index list =
    list |> List.drop index |> List.head
