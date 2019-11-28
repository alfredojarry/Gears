module Coll exposing (Coll, Id, decoder, empty, encoder, fillReserved, filter, get, idDecoder, idEncoder, idToString, ids, insert, insertTellId, maybeGet, remove, reserve, startId, toList, update, values)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Field as Field
import Json.Encode as E


type Id x
    = Id Int


getIdInternal : Id x -> Int
getIdInternal (Id i) =
    i


opacifyId : Int -> Id x
opacifyId i =
    Id i


idToString : Id x -> String
idToString (Id i) =
    String.fromInt i


idEncoder : Id x -> E.Value
idEncoder (Id i) =
    E.int i


idDecoder : D.Decoder (Id x)
idDecoder =
    D.map Id D.int


type Coll item
    = C { nextId : Int, d : Dict Int item, default : item }


startInt : Int
startInt =
    1


startId : Id item
startId =
    opacifyId startInt


empty : item -> Coll item
empty default =
    C { nextId = startInt, d = Dict.empty, default = default }


get : Id item -> Coll item -> item
get id (C coll) =
    case maybeGet id (C coll) of
        Nothing ->
            Debug.log ("No item for id " ++ (String.fromInt <| getIdInternal id)) coll.default

        Just item ->
            item


maybeGet : Id item -> Coll item -> Maybe item
maybeGet id (C { d }) =
    Dict.get (getIdInternal id) d


insert : item -> Coll item -> Coll item
insert el (C coll) =
    C { coll | d = Dict.insert coll.nextId el coll.d, nextId = coll.nextId + 1 }


insertTellId : item -> Coll item -> ( Id item, Coll item )
insertTellId el (C coll) =
    ( opacifyId coll.nextId, C { coll | d = Dict.insert coll.nextId el coll.d, nextId = coll.nextId + 1 } )


remove : Id item -> Coll item -> Coll item
remove id (C coll) =
    C { coll | d = Dict.remove (getIdInternal id) coll.d }


reserve : Coll item -> ( Coll item, Id item )
reserve (C coll) =
    ( C { coll | nextId = coll.nextId + 1 }, opacifyId coll.nextId )


fillReserved : Id item -> item -> Coll item -> Coll item
fillReserved id el (C coll) =
    C { coll | d = Dict.insert (getIdInternal id) el coll.d }


update : Id item -> (item -> item) -> Coll item -> Coll item
update id f (C coll) =
    C { coll | d = Dict.update (getIdInternal id) (Maybe.map f) coll.d }


filter : (item -> Bool) -> Coll item -> Coll item
filter isGood (C coll) =
    C { coll | d = Dict.filter (\_ v -> isGood v) coll.d }


toList : Coll item -> List ( Id item, item )
toList (C { d }) =
    Dict.toList d
        |> List.map (Tuple.mapFirst opacifyId)


ids : Coll item -> List (Id item)
ids (C { d }) =
    List.map opacifyId <| Dict.keys d


values : Coll item -> List item
values (C { d }) =
    Dict.values d


encoder : Coll item -> (item -> E.Value) -> E.Value
encoder (C coll) itemEncoder =
    E.list (keyValueEncoder itemEncoder) (Dict.toList coll.d)


decoder : D.Decoder item -> item -> D.Decoder (Coll item)
decoder itemDecoder defaultItem =
    D.map (fromKeyValue defaultItem) <| D.list (keyValueDecoder itemDecoder)


keyValueEncoder : (item -> E.Value) -> ( Int, item ) -> E.Value
keyValueEncoder itemEncoder ( i, item ) =
    E.object [ ( "id", E.int i ), ( "item", itemEncoder item ) ]


keyValueDecoder : D.Decoder item -> D.Decoder ( Int, item )
keyValueDecoder itemDecoder =
    Field.require "id" D.int <|
        \i ->
            Field.require "item" itemDecoder <|
                \item ->
                    D.succeed ( i, item )


fromKeyValue : item -> List ( Int, item ) -> Coll item
fromKeyValue default l =
    let
        ( ints, _ ) =
            List.unzip l
    in
    C
        { d = Dict.fromList l
        , default = default
        , nextId =
            case List.maximum ints of
                Nothing ->
                    getIdInternal startId

                Just i ->
                    i + 1
        }
