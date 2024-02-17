module Model exposing (Model, Msg(..), init, template, update)

import Theory.Chord as Chord exposing (ChordSpecifier)
import Theory.Key as Key exposing (Key)


type alias Model =
    { keys : List Key
    , chords : List ChordSpecifier
    }


type alias Template =
    List ( Key, List ChordSpecifier )


init : Model
init =
    { keys = Key.all
    , chords = Chord.all
    }


type Msg
    = SetKey Key Bool
    | SetChord ChordSpecifier Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetKey key bool ->
            setKey bool key model

        SetChord chord bool ->
            setChord bool chord model


setChord : Bool -> ChordSpecifier -> Model -> Model
setChord addOrRemove chord model =
    if addOrRemove then
        { model | chords = Chord.add chord model.chords }

    else
        { model | chords = Chord.remove chord model.chords }


setKey : Bool -> Key -> Model -> Model
setKey addOrRemove key model =
    if addOrRemove then
        { model | keys = Key.add key model.keys }

    else
        { model | keys = Key.remove key model.keys }


buildTemplate : List Key -> List ChordSpecifier -> Template
buildTemplate usedKeys usedChords =
    List.map (\key -> ( key, usedChords )) usedKeys


template : Model -> Template
template model =
    buildTemplate (Key.filterSelected model.keys) (Chord.filterSelected model.chords)
