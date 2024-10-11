module Review.Action exposing (rule)

{-| Transform code at `!action` comments

@docs rule

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import FastDict
import FastSet
import Review.Fix
import Review.ModuleNameLookupTable
import Review.Rule


{-| Applies code transformations marked by !action comments

    config =
        [ Review.Action.rule
        ]


### available actions

  - `function {-!inline-} arguments`: substitute its arguments into the implementation code
  - `declarationName ... = {-!remove-} ...`: Unexpose `declarationName`, remove it's @docs tag and delete its declaration code

-}
rule : Review.Rule.Rule
rule =
    Review.Rule.newProjectRuleSchema "Review.Action" initialContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleVisitor
            (\moduleVisitor ->
                moduleVisitor
                    |> Review.Rule.withModuleDefinitionVisitor
                        (\(Elm.Syntax.Node.Node moduleHeaderRange moduleHeader) context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , comments = context.comments
                              , declaredExpressionImplementations = context.declaredExpressionImplementations
                              , moduleDocumentation = context.moduleDocumentation
                              , moduleExposing = moduleHeader |> moduleHeaderExposing
                              , importStartRow = moduleHeaderRange.end.row + 1
                              , imports = context.imports
                              }
                            )
                        )
                    |> Review.Rule.withModuleDocumentationVisitor
                        (\maybeModuleDocumentationNode context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , moduleExposing = context.moduleExposing
                              , comments = context.comments
                              , declaredExpressionImplementations = context.declaredExpressionImplementations
                              , moduleDocumentation = maybeModuleDocumentationNode
                              , imports = context.imports
                              , importStartRow =
                                    case maybeModuleDocumentationNode of
                                        Nothing ->
                                            context.importStartRow

                                        Just (Elm.Syntax.Node.Node moduleDocumentationRange _) ->
                                            moduleDocumentationRange.end.row + 1
                              }
                            )
                        )
                    |> Review.Rule.withImportVisitor
                        (\(Elm.Syntax.Node.Node importRange import_) context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , moduleExposing = context.moduleExposing
                              , moduleDocumentation = context.moduleDocumentation
                              , comments = context.comments
                              , declaredExpressionImplementations = context.declaredExpressionImplementations
                              , importStartRow = importRange.end.row + 1
                              , imports =
                                    context.imports
                                        |> FastSet.insert (import_.moduleName |> Elm.Syntax.Node.value)
                              }
                            )
                        )
                    |> Review.Rule.withCommentsVisitor
                        (\comments context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , moduleExposing = context.moduleExposing
                              , moduleDocumentation = context.moduleDocumentation
                              , importStartRow = context.importStartRow
                              , imports = context.imports
                              , declaredExpressionImplementations = context.declaredExpressionImplementations
                              , comments = comments
                              }
                            )
                        )
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarations context ->
                            ( []
                            , { moduleOriginLookup = context.moduleOriginLookup
                              , extractSourceCode = context.extractSourceCode
                              , importedModules = context.importedModules
                              , moduleExposing = context.moduleExposing
                              , moduleDocumentation = context.moduleDocumentation
                              , importStartRow = context.importStartRow
                              , comments = context.comments
                              , imports = context.imports
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
                    |> Review.Rule.withDeclarationEnterVisitor
                        (\declarationNode context ->
                            ( declarationCheck context declarationNode
                            , context
                            )
                        )
                    |> Review.Rule.withExpressionEnterVisitor
                        (\expressionNode context ->
                            ( case expressionNode of
                                Elm.Syntax.Node.Node callRange (Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node referenceRange (Elm.Syntax.Expression.FunctionOrValue _ referenceUnqualified)) :: argument0 :: argument1Up)) ->
                                    callCheck context
                                        { range = callRange
                                        , referenceRange = referenceRange
                                        , referenceUnqualified = referenceUnqualified
                                        , argument0 = argument0
                                        , argument1Up = argument1Up
                                        }

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


declarationCheck :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List (Review.Rule.Error {})
declarationCheck context (Elm.Syntax.Node.Node declarationRange declaration) =
    let
        maybeDeclarationInfo :
            Maybe
                { removeMarkCommentRange : Elm.Syntax.Range.Range
                , name : Elm.Syntax.Node.Node String
                }
        maybeDeclarationInfo =
            case declaration of
                Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                    let
                        implementation : Elm.Syntax.Expression.FunctionImplementation
                        implementation =
                            valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value
                    in
                    Just
                        { removeMarkCommentRange =
                            { start =
                                case implementation.arguments of
                                    [] ->
                                        implementation.name |> Elm.Syntax.Node.range |> .end

                                    parameter0 :: parameter1Up ->
                                        listFilledLast ( parameter0, parameter1Up )
                                            |> Elm.Syntax.Node.range
                                            |> .end
                            , end =
                                implementation.expression
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            }
                        , name = implementation.name
                        }

                Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                    Just
                        { removeMarkCommentRange =
                            { start =
                                case typeAliasDeclaration.generics of
                                    [] ->
                                        typeAliasDeclaration.name |> Elm.Syntax.Node.range |> .end

                                    parameter0 :: parameter1Up ->
                                        listFilledLast ( parameter0, parameter1Up )
                                            |> Elm.Syntax.Node.range
                                            |> .end
                            , end =
                                typeAliasDeclaration.typeAnnotation
                                    |> Elm.Syntax.Node.range
                                    |> .start
                            }
                        , name = typeAliasDeclaration.name
                        }

                Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                    case choiceTypeDeclaration.constructors of
                        [] ->
                            Nothing

                        (Elm.Syntax.Node.Node firstVariantRange _) :: _ ->
                            Just
                                { removeMarkCommentRange =
                                    { start =
                                        case choiceTypeDeclaration.generics of
                                            [] ->
                                                choiceTypeDeclaration.name |> Elm.Syntax.Node.range |> .end

                                            parameter0 :: parameter1Up ->
                                                listFilledLast ( parameter0, parameter1Up )
                                                    |> Elm.Syntax.Node.range
                                                    |> .end
                                    , end = firstVariantRange.start
                                    }
                                , name = choiceTypeDeclaration.name
                                }

                Elm.Syntax.Declaration.PortDeclaration _ ->
                    Nothing

                Elm.Syntax.Declaration.InfixDeclaration _ ->
                    Nothing

                Elm.Syntax.Declaration.Destructuring _ _ ->
                    Nothing
    in
    case maybeDeclarationInfo of
        Nothing ->
            []

        Just declarationInfo ->
            if
                commentsInRange declarationInfo.removeMarkCommentRange context.comments
                    |> List.any (\comment -> comment |> String.contains "!remove")
            then
                [ Review.Rule.errorWithFix
                    { message = "remove declaration " ++ (declarationInfo.name |> Elm.Syntax.Node.value)
                    , details =
                        [ "The action command !remove placed in a comment at the top of the declaration "
                            ++ (declarationInfo.name |> Elm.Syntax.Node.value)
                            ++ " triggers the suggestion of this automatic fix. Either apply the fix or remove the comment."
                        ]
                    }
                    (declarationInfo.name |> Elm.Syntax.Node.range)
                    (Review.Fix.removeRange
                        { start = declarationRange.start
                        , end = { row = declarationRange.end.row + 1, column = 1 }
                        }
                        :: (case context.moduleDocumentation of
                                Nothing ->
                                    []

                                Just moduleDocumentationNode ->
                                    moduleDocumentationRemoveDocsFix
                                        (declarationInfo.name |> Elm.Syntax.Node.value)
                                        moduleDocumentationNode
                           )
                        ++ (case context.moduleExposing of
                                Elm.Syntax.Exposing.All _ ->
                                    []

                                Elm.Syntax.Exposing.Explicit exposes ->
                                    case exposes of
                                        [] ->
                                            []

                                        [ _ ] ->
                                            []

                                        (Elm.Syntax.Node.Node expose0Range expose0) :: (Elm.Syntax.Node.Node expose1Range expose1) :: expose2Up ->
                                            if exposeDeclarationName expose0 == (declarationInfo.name |> Elm.Syntax.Node.value) then
                                                [ Review.Fix.removeRange
                                                    { start = expose0Range.start, end = expose1Range.start }
                                                ]

                                            else
                                                (Elm.Syntax.Node.Node expose1Range expose1 :: expose2Up)
                                                    |> List.foldl
                                                        (\(Elm.Syntax.Node.Node exposeRange expose) soFar ->
                                                            { fixes =
                                                                if exposeDeclarationName expose == (declarationInfo.name |> Elm.Syntax.Node.value) then
                                                                    Review.Fix.removeRange { start = soFar.end, end = exposeRange.end }
                                                                        :: soFar.fixes

                                                                else
                                                                    soFar.fixes
                                                            , end = exposeRange.end
                                                            }
                                                        )
                                                        { fixes = [], end = expose0Range.end }
                                                    |> .fixes
                           )
                    )
                ]

            else
                []


moduleDocumentationRemoveDocsFix : String -> Elm.Syntax.Node.Node String -> List Review.Fix.Fix
moduleDocumentationRemoveDocsFix nameToRemove (Elm.Syntax.Node.Node moduleDocumentationRange moduleDocumentation) =
    moduleDocumentation
        |> String.lines
        |> List.indexedMap
            (\lineIndex line ->
                if line |> String.startsWith "@docs " then
                    let
                        declarationNamesInDocsLine : List String
                        declarationNamesInDocsLine =
                            line
                                |> String.slice 6 (line |> String.length)
                                |> String.split ","
                                |> List.map String.trim
                    in
                    if declarationNamesInDocsLine |> List.member nameToRemove then
                        let
                            absoluteRow : Int
                            absoluteRow =
                                moduleDocumentationRange.start.row + lineIndex
                        in
                        [ Review.Fix.replaceRangeBy
                            { start = { row = absoluteRow, column = 1 }
                            , end = { row = absoluteRow + 1, column = 1 }
                            }
                            (case declarationNamesInDocsLine of
                                [] ->
                                    ""

                                [ _ ] ->
                                    ""

                                name0 :: name1Up ->
                                    "@docs "
                                        ++ ((name0 :: name1Up)
                                                |> List.filter (\name -> name /= nameToRemove)
                                                |> String.join ", "
                                           )
                                        ++ "\n"
                            )
                        ]

                    else
                        []

                else
                    []
            )
        |> List.concat


exposeDeclarationName : Elm.Syntax.Exposing.TopLevelExpose -> String
exposeDeclarationName expose =
    case expose of
        Elm.Syntax.Exposing.InfixExpose operator ->
            operator

        Elm.Syntax.Exposing.FunctionExpose name ->
            name

        Elm.Syntax.Exposing.TypeOrAliasExpose name ->
            name

        Elm.Syntax.Exposing.TypeExpose typeExpose ->
            typeExpose.name


callCheck :
    ModuleContext
    ->
        { referenceRange : Elm.Syntax.Range.Range
        , argument0 : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , referenceUnqualified : String
        , range : { end : Elm.Syntax.Range.Location, start : Elm.Syntax.Range.Location }
        , argument1Up : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
        }
    -> List (Review.Rule.Error {})
callCheck context call =
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
                            , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
                            , extractSourceCode : Elm.Syntax.Range.Range -> String
                            }
                    maybeReferenceImplementationModule =
                        case referenceModuleOrigin of
                            [] ->
                                Just
                                    { declaredExpressionImplementations = context.declaredExpressionImplementations
                                    , extractSourceCode = context.extractSourceCode
                                    , moduleOriginLookup = context.moduleOriginLookup
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

                                    importFixes : List Review.Fix.Fix
                                    importFixes =
                                        case referenceModuleOrigin of
                                            [] ->
                                                []

                                            _ :: _ ->
                                                FastSet.diff
                                                    (referenceImplementation.result
                                                        |> expressionReferences
                                                        |> List.foldl
                                                            (\(Elm.Syntax.Node.Node resultReferenceRange _) soFar ->
                                                                case
                                                                    Review.ModuleNameLookupTable.moduleNameAt
                                                                        referenceImplementationModule.moduleOriginLookup
                                                                        resultReferenceRange
                                                                of
                                                                    Nothing ->
                                                                        soFar

                                                                    Just [] ->
                                                                        -- since we called a reference from the same module,
                                                                        -- we must have already imported it
                                                                        soFar

                                                                    Just (resultReferenceModuleNamePart0 :: resultReferenceModuleNamePart1Up) ->
                                                                        soFar |> FastSet.insert (resultReferenceModuleNamePart0 :: resultReferenceModuleNamePart1Up)
                                                            )
                                                            FastSet.empty
                                                    )
                                                    context.imports
                                                    |> FastSet.toList
                                                    |> List.map
                                                        (\moduleNameToImport ->
                                                            Review.Fix.insertAt { row = context.importStartRow, column = 1 }
                                                                ("import "
                                                                    ++ (moduleNameToImport |> String.join ".")
                                                                    ++ "\n"
                                                                )
                                                        )
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
                                    (Review.Fix.replaceRangeBy
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
                                        :: importFixes
                                    )
                                ]


type alias ProjectContext =
    { byModule :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { extractSourceCode : Elm.Syntax.Range.Range -> String
            , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
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
    , moduleExposing : Elm.Syntax.Exposing.Exposing
    , moduleDocumentation : Maybe (Elm.Syntax.Node.Node String)
    , importStartRow : Int
    , imports : FastSet.Set Elm.Syntax.ModuleName.ModuleName
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
            , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
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
            , moduleExposing = Elm.Syntax.Exposing.All Elm.Syntax.Range.empty
            , moduleDocumentation = Nothing
            , importStartRow = 2
            , imports = FastSet.empty
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
                    , moduleOriginLookup = moduleContext.moduleOriginLookup
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


expressionReferences :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node ( Elm.Syntax.ModuleName.ModuleName, String ))
expressionReferences expressionNode =
    case expressionNode of
        Elm.Syntax.Node.Node variableRange (Elm.Syntax.Expression.FunctionOrValue qualification unqualified) ->
            [ Elm.Syntax.Node.Node variableRange ( qualification, unqualified ) ]

        nonVariable ->
            nonVariable
                |> expressionSubs
                |> List.concatMap expressionReferences


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


moduleHeaderExposing : Elm.Syntax.Module.Module -> Elm.Syntax.Exposing.Exposing
moduleHeaderExposing moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule info ->
            info.exposingList |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule info ->
            info.exposingList |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule info ->
            info.exposingList |> Elm.Syntax.Node.value


listFilledLast : ( a, List a ) -> a
listFilledLast ( head, tail ) =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast ( tailHead, tailTail )


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
