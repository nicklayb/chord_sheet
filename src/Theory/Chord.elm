module Theory.Chord exposing
    ( Chord
    , ChordSpecifier(..)
    , add
    , all
    , build
    , chordToString
    , equal
    , filterSelected
    , isSelected
    , nameToString
    , remove
    , specifierNameToString
    , specifierToString
    )

import List.Extra
import Theory.Key as Key exposing (Key)


type alias ScaleMap =
    List Int


type Scale
    = MajorScale
    | MinorScale


type alias NoteVariant =
    ( Scale, Int )


type ChordSpecifier
    = Major
    | Minor
    | Major7
    | Minor7
    | Dominant7


type alias Chord =
    ( Key, ChordSpecifier )


all : List ChordSpecifier
all =
    [ Major, Minor, Major7, Minor7, Dominant7 ]


equal : ChordSpecifier -> ChordSpecifier -> Bool
equal left right =
    case ( left, right ) of
        ( Major, Major ) ->
            True

        ( Minor, Minor ) ->
            True

        ( Major7, Major7 ) ->
            True

        ( Minor7, Minor7 ) ->
            True

        ( Dominant7, Dominant7 ) ->
            True

        ( _, _ ) ->
            False


build : Key -> ChordSpecifier -> List Key
build key =
    let
        function ( note, index ) acc =
            note
                |> scaleMap
                |> getScale key
                |> List.Extra.at index
                |> Maybe.map (\n -> n :: acc)
                |> Maybe.withDefault acc
    in
    indexes >> List.foldl function [] >> List.reverse


nameToString : Chord -> String
nameToString ( key, specifier ) =
    Key.toString key ++ specifierToString specifier


chordToString : Chord -> String
chordToString ( key, chordSpecifier ) =
    chordSpecifier
        |> build key
        |> List.map Key.toString
        |> String.join " | "


indexes : ChordSpecifier -> List NoteVariant
indexes chordSpecifier =
    case chordSpecifier of
        Major ->
            [ ( MajorScale, 0 ), ( MajorScale, 2 ), ( MajorScale, 4 ) ]

        Dominant7 ->
            [ ( MajorScale, 0 ), ( MajorScale, 2 ), ( MajorScale, 4 ), ( MinorScale, 6 ) ]

        Major7 ->
            [ ( MajorScale, 0 ), ( MajorScale, 2 ), ( MajorScale, 4 ), ( MajorScale, 6 ) ]

        Minor ->
            [ ( MinorScale, 0 ), ( MinorScale, 2 ), ( MinorScale, 4 ) ]

        Minor7 ->
            [ ( MinorScale, 0 ), ( MinorScale, 2 ), ( MinorScale, 4 ), ( MinorScale, 6 ) ]


minorScaleMap : ScaleMap
minorScaleMap =
    [ 0, 2, 3, 5, 7, 8, 10 ]


majorScaleMap : ScaleMap
majorScaleMap =
    [ 0, 2, 4, 5, 7, 9, 11 ]


scaleMap : Scale -> ScaleMap
scaleMap variant =
    case variant of
        MinorScale ->
            minorScaleMap

        MajorScale ->
            majorScaleMap


specifierToString : ChordSpecifier -> String
specifierToString specifier =
    case specifier of
        Major ->
            ""

        Major7 ->
            "maj7"

        Minor ->
            "m"

        Minor7 ->
            "m7"

        Dominant7 ->
            "7"


specifierNameToString : ChordSpecifier -> String
specifierNameToString specifier =
    case specifier of
        Major ->
            "Major"

        Major7 ->
            "Major 7th"

        Minor ->
            "Minor"

        Minor7 ->
            "Minor 7th"

        Dominant7 ->
            "Dominant 7"


getScale : Key -> ScaleMap -> List Key
getScale key =
    let
        shiftedKeys =
            Key.all
                |> List.Extra.indexOf (\k -> k == key)
                |> Maybe.withDefault 0
                |> (\index -> List.Extra.shift index Key.all)

        function index acc =
            shiftedKeys
                |> List.Extra.at index
                |> Maybe.map (\note -> note :: acc)
                |> Maybe.withDefault acc
    in
    List.foldl function [] >> List.reverse


add : ChordSpecifier -> List ChordSpecifier -> List ChordSpecifier
add chord modelChords =
    chord :: modelChords


remove : ChordSpecifier -> List ChordSpecifier -> List ChordSpecifier
remove chord =
    List.filter (equal chord >> not)


isSelected : ChordSpecifier -> List ChordSpecifier -> Bool
isSelected chord =
    List.any (equal chord)


filterSelected : List ChordSpecifier -> List ChordSpecifier
filterSelected selectedChords =
    List.Extra.filterSelected equal selectedChords all
