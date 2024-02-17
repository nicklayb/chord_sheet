module List.Extra exposing (at, filterSelected, indexOf, shift)


at : Int -> List a -> Maybe a
at index list =
    case index of
        0 ->
            List.head list

        _ ->
            list
                |> List.tail
                |> Maybe.andThen (at (index - 1))


indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf function list =
    let
        searchFunction currentList index =
            case currentList of
                [] ->
                    Nothing

                head :: tail ->
                    if function head then
                        Just index

                    else
                        searchFunction tail (index + 1)
    in
    searchFunction list 0


shift : Int -> List a -> List a
shift amount list =
    case amount of
        0 ->
            list

        _ ->
            case list of
                [] ->
                    []

                head :: tail ->
                    [ head ]
                        |> List.append tail
                        |> shift (amount - 1)


filterSelected : (a -> a -> Bool) -> List a -> List a -> List a
filterSelected function selected all =
    List.filter (\item -> List.any (function item) selected) all
