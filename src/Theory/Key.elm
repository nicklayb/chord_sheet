module Theory.Key exposing
    ( Key(..)
    , add
    , all
    , equal
    , filterSelected
    , getRoot
    , isSelected
    , isSharp
    , remove
    , toString
    )

import List.Extra


type Key
    = C
    | CSharp
    | D
    | DSharp
    | E
    | F
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B


all : List Key
all =
    [ C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B ]


add : Key -> List Key -> List Key
add key modelKeys =
    key :: modelKeys


remove : Key -> List Key -> List Key
remove key =
    List.filter (equal key >> not)


isSelected : Key -> List Key -> Bool
isSelected key =
    List.any (equal key)


equal : Key -> Key -> Bool
equal left right =
    case ( left, right ) of
        ( C, C ) ->
            True

        ( CSharp, CSharp ) ->
            True

        ( D, D ) ->
            True

        ( DSharp, DSharp ) ->
            True

        ( E, E ) ->
            True

        ( F, F ) ->
            True

        ( FSharp, FSharp ) ->
            True

        ( G, G ) ->
            True

        ( GSharp, GSharp ) ->
            True

        ( A, A ) ->
            True

        ( ASharp, ASharp ) ->
            True

        ( B, B ) ->
            True

        _ ->
            False


isSharp : Key -> Bool
isSharp key =
    case key of
        CSharp ->
            True

        DSharp ->
            True

        FSharp ->
            True

        GSharp ->
            True

        ASharp ->
            True

        _ ->
            False


toString : Key -> String
toString key =
    case key of
        C ->
            "C"

        CSharp ->
            "C#"

        D ->
            "D"

        DSharp ->
            "D#"

        E ->
            "E"

        F ->
            "F"

        FSharp ->
            "F#"

        G ->
            "G"

        GSharp ->
            "G#"

        A ->
            "A"

        ASharp ->
            "A#"

        B ->
            "B"


getRoot : Int -> Maybe Key
getRoot index =
    all
        |> List.Extra.shift index
        |> List.head


filterSelected : List Key -> List Key
filterSelected selectedKeys =
    List.Extra.filterSelected equal selectedKeys all
