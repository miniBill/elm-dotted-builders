module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation
import Elm.Parser
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.File as File
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Gen.CodeGen.Generate as Generate
import Parser
import Result.Extra
import String.Extra


main : Program String () ()
main =
    Platform.worker
        { init =
            \flags ->
                ( ()
                , flags
                    |> Elm.Parser.parseToFile
                    |> Result.mapError (List.map toGenerateError)
                    |> Result.andThen toFile
                    |> Result.mapError Generate.error
                    |> Result.map (Generate.files << List.singleton)
                    |> Result.Extra.merge
                )
        , update =
            \_ model ->
                ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


toGenerateError : Parser.DeadEnd -> Generate.Error
toGenerateError deadEnd =
    { title = "Parsing error"
    , description = deadEndToString deadEnd
    }


type alias Results a =
    Result (List (Node Generate.Error)) a


toFile : File.File -> Result (List Generate.Error) Elm.File
toFile file =
    file.declarations
        |> combineMapAccumulate declarationToDeclarations
        |> Result.map (List.concat >> Elm.file [ "Types" ])
        |> Result.mapError (List.map denodeError)


denodeError : Node Generate.Error -> Generate.Error
denodeError (Node range error) =
    { title = error.title
    , description =
        "@{row}:{col} {desc}\n"
            |> String.replace "{row}" (String.fromInt range.start.row)
            |> String.replace "{col}" (String.fromInt range.start.column)
            |> String.replace "{desc}" error.description
    }


combineMapAccumulate : (a -> Results x) -> List a -> Results (List x)
combineMapAccumulate f list =
    list
        |> List.map f
        |> combineErrors


combineErrors : List (Results x) -> Results (List x)
combineErrors list =
    List.foldl
        (\e acc ->
            case ( e, acc ) of
                ( Ok oe, Ok oacc ) ->
                    Ok (oe :: oacc)

                ( Err ee, Ok _ ) ->
                    Err ee

                ( Err ee, Err eacc ) ->
                    Err (List.reverse ee ++ eacc)

                ( Ok _, Err _ ) ->
                    acc
        )
        (Ok [])
        list
        |> Result.Extra.mapBoth List.reverse List.reverse


err : Range -> Generate.Error -> Results value
err range inner =
    Err [ Node range inner ]


declarationToDeclarations : Node Declaration.Declaration -> Results (List Elm.Declaration)
declarationToDeclarations (Node range declaration) =
    case declaration of
        Declaration.CustomTypeDeclaration type_ ->
            let
                typeName : String
                typeName =
                    Node.value type_.name

                allName : String
                allName =
                    toAllName typeName

                documentation : String
                documentation =
                    type_.documentation
                        |> Maybe.map (Node.value >> String.slice 3 -2)
                        |> Maybe.withDefault ""

                classified : Results Classified
                classified =
                    toClassified type_.constructors

                redefinition : Results Elm.Declaration
                redefinition =
                    type_.constructors
                        |> combineMapAccumulate constructorToVariant
                        |> Result.map
                            (\variants ->
                                variants
                                    |> Elm.customType typeName
                                    |> Elm.withDocumentation documentation
                            )

                allAlias : List ( String, List String ) -> Elm.Declaration
                allAlias variants =
                    variants
                        |> classifiedToAlias
                        |> Elm.alias allName

                builderAlias : Elm.Declaration
                builderAlias =
                    Elm.Annotation.namedWith [] allName [ Elm.Annotation.named [] typeName ]
                        |> Elm.alias (typeName ++ "Builder")

                variantToBuilder : Maybe Elm.Expression -> ( String, List String ) -> Elm.Expression
                variantToBuilder builder ( ctor, args ) =
                    args
                        |> List.foldr
                            (\arg acc ->
                                Elm.apply (Elm.val <| String.Extra.decapitalize <| toAllName arg)
                                    [ Elm.fn ( arg, Nothing ) <| \_ -> acc
                                    ]
                            )
                            (let
                                arg =
                                    Elm.apply (Elm.val ctor) (List.map (Elm.val << String.Extra.decapitalize) args)
                             in
                             builder
                                |> Maybe.map (\b -> Elm.apply b [ arg ])
                                |> Maybe.withDefault arg
                            )

                toBuilder : Maybe Elm.Expression -> List ( String, List String ) -> Elm.Expression
                toBuilder builder variants =
                    case variants of
                        [ variant ] ->
                            variantToBuilder builder variant

                        _ ->
                            variants
                                |> List.map
                                    (\(( variantName, _ ) as variant) ->
                                        ( variantName
                                        , variantToBuilder builder variant
                                        )
                                    )
                                |> Elm.record

                allDeclaration : List ( String, List String ) -> Elm.Declaration
                allDeclaration variants =
                    (Elm.fn
                        ( "toBuilder"
                        , Just
                            (Elm.Annotation.function [ Elm.Annotation.named [] typeName ]
                                (Elm.Annotation.var "builder")
                            )
                        )
                     <|
                        \builder ->
                            toBuilder (Just builder) variants
                                |> Elm.withType (Elm.Annotation.namedWith [] allName [ Elm.Annotation.var "builder" ])
                    )
                        |> Elm.declaration allName

                builderDeclaration : Classified -> Elm.Declaration
                builderDeclaration variants =
                    toBuilder Nothing variants
                        |> Elm.withType (Elm.Annotation.named [] (typeName ++ "Builder"))
                        |> Elm.declaration (typeName ++ "Builder")
            in
            Result.map2
                (\variants redef ->
                    [ redef
                    , allAlias variants
                    , allDeclaration variants
                    , builderAlias
                    , builderDeclaration variants
                    ]
                        |> List.map (Elm.exposeWith { exposeConstructor = True, group = Nothing })
                )
                classified
                redefinition

        _ ->
            err range
                { title = "Unsupported declaration"
                , description = "Found an unsupported declaration, only custom type declarations are supported"
                }


classifiedToAlias : Classified -> Elm.Annotation.Annotation
classifiedToAlias variants =
    let
        variantToValue args =
            List.foldr
                (\arg acc ->
                    Elm.Annotation.namedWith [] (toAllName arg) [ acc ]
                )
                (Elm.Annotation.var "builder")
                args
    in
    case variants of
        [ ( _, args ) ] ->
            variantToValue args

        _ ->
            variants
                |> List.map
                    (\( variant, args ) ->
                        ( variant, variantToValue args )
                    )
                |> Elm.Annotation.record


toAllName : String -> String
toAllName arg =
    if String.endsWith "y" arg then
        "All" ++ String.dropRight 1 arg ++ "ies"

    else
        "All" ++ arg ++ "s"


type alias Classified =
    List ( String, List String )


toClassified : List (Node Type.ValueConstructor) -> Results Classified
toClassified constructors =
    constructors
        |> combineMapAccumulate
            (\(Node _ { name, arguments }) ->
                arguments
                    |> combineMapAccumulate
                        (\(Node argRange arg) ->
                            case arg of
                                TypeAnnotation.Typed (Node _ ( [], argName )) [] ->
                                    Ok argName

                                _ ->
                                    err argRange
                                        { title = "Unsupported argument"
                                        , description = "TODO"
                                        }
                        )
                    |> Result.map (Tuple.pair (Node.value name))
            )


constructorToVariant : Node Type.ValueConstructor -> Results Elm.Variant
constructorToVariant (Node _ { name, arguments }) =
    Result.map (Elm.variantWith <| Node.value name) (combineMapAccumulate typeToType arguments)


typeToType : Node TypeAnnotation.TypeAnnotation -> Results Elm.Annotation.Annotation
typeToType (Node range type_) =
    case type_ of
        TypeAnnotation.Unit ->
            Ok Elm.Annotation.unit

        TypeAnnotation.GenericType t ->
            Ok <| Elm.Annotation.var t

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) children ->
            Result.map (Elm.Annotation.namedWith moduleName typeName) (Result.Extra.combineMap typeToType children)

        TypeAnnotation.Tupled [ l, r ] ->
            Result.map2 Elm.Annotation.tuple (typeToType l) (typeToType r)

        TypeAnnotation.Tupled [ l, m, r ] ->
            Result.map3 Elm.Annotation.triple (typeToType l) (typeToType m) (typeToType r)

        TypeAnnotation.Tupled _ ->
            err range
                { title = "Cannot convert type"
                , description = "Invalid number of elements in tuple"
                }

        TypeAnnotation.Record fields ->
            Result.map Elm.Annotation.record
                (Result.Extra.combineMap
                    (\(Node _ ( Node _ fieldName, fieldType )) ->
                        Result.map (Tuple.pair fieldName) (typeToType fieldType)
                    )
                    fields
                )

        TypeAnnotation.GenericRecord (Node _ var) (Node _ fields) ->
            Result.map (Elm.Annotation.extensible var)
                (Result.Extra.combineMap
                    (\(Node _ ( Node _ fieldName, fieldType )) ->
                        Result.map (Tuple.pair fieldName) (typeToType fieldType)
                    )
                    fields
                )

        TypeAnnotation.FunctionTypeAnnotation from to ->
            Result.map2 (\f t -> Elm.Annotation.function [ f ] t) (typeToType from) (typeToType to)



{- https://github.com/elm/parser/pull/16/files -}


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            "problem " ++ s

        Parser.BadRepeat ->
            "bad repeat"
