module Theory.KeyboardMap exposing (KeyboardMap, empty, setChord)

import Theory.Chord as Chord exposing (Chord)
import Theory.Key as Key exposing (Key(..))


type alias KeyboardMap =
    List ( Key, Bool )


empty : KeyboardMap
empty =
    [ ( C, False )
    , ( CSharp, False )
    , ( D, False )
    , ( DSharp, False )
    , ( E, False )
    , ( F, False )
    , ( FSharp, False )
    , ( G, False )
    , ( GSharp, False )
    , ( A, False )
    , ( ASharp, False )
    , ( B, False )
    , ( C, False )
    , ( CSharp, False )
    , ( D, False )
    , ( DSharp, False )
    , ( E, False )
    , ( F, False )
    , ( FSharp, False )
    , ( G, False )
    , ( GSharp, False )
    , ( A, False )
    , ( ASharp, False )
    , ( B, False )
    ]


setChord : Chord -> KeyboardMap -> KeyboardMap
setChord ( key, specifier ) keyboardMap =
    let
        chordKeys =
            Chord.build key specifier

        function =
            setNoteState True key
    in
    List.foldl function keyboardMap chordKeys


setNoteState : Bool -> Key -> Key -> KeyboardMap -> KeyboardMap
setNoteState setOn rootKey key keyboardMap =
    let
        keyIsRoot currentKey =
            Key.equal rootKey currentKey

        function ( currentKey, isOn ) ( beenSet, rootPlaced, acc ) =
            if Key.equal key currentKey then
                case ( beenSet, rootPlaced, keyIsRoot currentKey ) of
                    ( False, True, _ ) ->
                        ( True, True, ( currentKey, setOn ) :: acc )

                    ( False, False, True ) ->
                        ( True, True, ( currentKey, setOn ) :: acc )

                    ( _, _, _ ) ->
                        ( beenSet, rootPlaced, ( currentKey, isOn ) :: acc )

            else if keyIsRoot currentKey && isOn then
                ( beenSet, True, ( currentKey, isOn ) :: acc )

            else
                ( beenSet, rootPlaced, ( currentKey, isOn ) :: acc )
    in
    case List.foldl function ( False, False, [] ) keyboardMap of
        ( _, _, updatedMap ) ->
            List.reverse updatedMap
