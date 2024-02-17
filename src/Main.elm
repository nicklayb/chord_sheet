module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, h2, input, label, span, text)
import Html.Attributes exposing (attribute, checked, class, type_)
import Html.Events exposing (onCheck)
import Model exposing (Model, Msg(..))
import Theory.Chord as Chord exposing (ChordSpecifier(..))
import Theory.Key as Key exposing (Key(..))
import Theory.KeyboardMap as KeyboardMap exposing (KeyboardMap)


view : Model -> Html Msg
view model =
    div []
        [ configurationView model
        , titleView "Chord sheet"
        , gridView model
        ]


titleView : String -> Html Msg
titleView textValue =
    h2 [ class "text-2xl mb-1 mt-5 ml-2" ] [ text textValue ]


configurationView : Model -> Html Msg
configurationView model =
    div [ class "mb-2" ]
        [ titleView "Keys"
        , keysView model
        , titleView "Chords"
        , chordsView model
        ]


keysView : Model -> Html Msg
keysView model =
    let
        labelView item =
            text <| Key.toString item

        isSelected item =
            Key.isSelected item model.keys
    in
    checkboxListView Key.all labelView isSelected SetKey


chordsView : Model -> Html Msg
chordsView model =
    let
        labelView item =
            text <| Chord.specifierNameToString item

        isSelected item =
            Chord.isSelected item model.chords
    in
    checkboxListView Chord.all labelView isSelected SetChord


checkboxListView : List a -> (a -> Html Msg) -> (a -> Bool) -> (a -> Bool -> Msg) -> Html Msg
checkboxListView items labelView isChecked message =
    let
        checkboxView item =
            div [ class "p-2 border-2 border-red rounded-md mr-2 first:ml-2" ]
                [ label []
                    [ input
                        [ type_ "checkbox"
                        , checked (isChecked item)
                        , onCheck (message item)
                        , class "mr-1"
                        ]
                        []
                    , labelView item
                    ]
                ]
    in
    div [ class "flex mb-2" ] <| List.map checkboxView items


dataKey : Int -> Key -> Attribute Msg
dataKey index key =
    attribute "data-key" <| (Key.toString key ++ String.fromInt index)


keyboardView : KeyboardMap -> Html Msg
keyboardView keyboardMap =
    let
        isOnClass isOn =
            if isOn then
                "active"

            else
                ""

        isSharpClass key =
            if Key.isSharp key then
                "sharp"

            else
                ""

        activeDot isOn =
            if isOn then
                [ span [ class "dot active" ] [] ]

            else
                []

        keyView index ( key, isOn ) =
            div
                [ class "key"
                , class <| isOnClass isOn
                , class <| isSharpClass key
                , dataKey index key
                ]
                (activeDot isOn)
    in
    div [ class "keys" ] <| List.indexedMap keyView keyboardMap


gridView : Model -> Html Msg
gridView model =
    let
        chordItemView key specifier =
            keyboardView (KeyboardMap.setChord ( key, specifier ) KeyboardMap.empty)

        rowView ( key, specifiers ) =
            div [ class "flex flex-1 first:border-t-2 border-l-2 border-r-2 border-b-2 border-neutral-200" ]
                (div [ class "flex basis-14 content-center items-center" ]
                    [ div [ class "flex p-2 text-2xl" ] [ text <| Key.toString key ] ]
                    :: List.map (cellView key) specifiers
                )

        cellView key specifier =
            div [ class "flex flex-1 items-center flex-col border-l-2 border-neutral-200 p-2" ]
                [ div [] [ text <| Chord.nameToString ( key, specifier ) ]
                , chordItemView key specifier
                ]

        tableBodyView =
            List.map rowView (Model.template model)
    in
    div [ class "w-full flex flex-col p-2" ] tableBodyView


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Model.init
        , view = view
        , update = Model.update
        }
