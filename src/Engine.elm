module Engine exposing
    ( Engine
    , addPlaying
    , init
    , muted
    , playingIds
    , setParentUid
    , setPlaying
    , stop
    , volumeChanged
    )

import Coll exposing (Coll, Id)
import Data.Collar as Collar exposing (Beed, Colleer)
import Data.Common exposing (Identifier)
import Data.Content as Content
import Data.Gear as Gear exposing (Gear)
import Data.Mobile exposing (Geer, Mobeel)
import Data.Wheel as Wheel exposing (Wheel)
import Harmony as Harmo
import Json.Encode as E
import Motor
import Sound


type Engine
    = E { playing : List (Id Geer), parentUid : String }


init : Engine
init =
    E { playing = [], parentUid = "" }


setParentUid : String -> Engine -> Engine
setParentUid str (E e) =
    E { e | parentUid = str }


playingIds :
    Engine
    -> List (Id Geer) -- Needed to compute which needs to be paused or stopped in motor, when cut
playingIds (E { playing }) =
    playing


setPlaying : List (Id Geer) -> Coll Geer -> Engine -> ( Engine, List E.Value )
setPlaying l coll (E e) =
    ( E { e | playing = l }
    , if List.isEmpty l then
        []

      else
        [ playPause e.parentUid coll <| List.filter (\el -> not <| List.member el l) e.playing ]
    )


addPlaying : List (Id Geer) -> Coll Geer -> Engine -> ( Engine, List E.Value )
addPlaying l coll (E e) =
    ( E { e | playing = e.playing ++ l }
    , if List.isEmpty l then
        []

      else
        [ playPause e.parentUid coll l ]
    )


playPause : String -> Coll Geer -> List (Id Geer) -> E.Value
playPause parentUid coll els =
    E.object
        [ ( "action", E.string "playPause" )
        , ( "gears", E.list (encodeGear True parentUid coll) els )
        ]


stop : E.Value
stop =
    E.object [ ( "action", E.string "stopReset" ) ]


muted : Identifier -> Bool -> Engine -> List E.Value
muted ( id, list ) mute (E e) =
    [ E.object
        [ ( "action", E.string "mute" )
        , ( "id", E.string <| e.parentUid ++ Gear.toUID id )
        , ( "beadIndexes", E.list E.int list )
        , ( "value", E.bool mute )
        ]
    ]


volumeChanged : Identifier -> Float -> Engine -> List E.Value
volumeChanged ( id, list ) volume (E e) =
    [ E.object
        [ ( "action", E.string "volume" )
        , ( "id", E.string <| e.parentUid ++ Gear.toUID id )
        , ( "beadIndexes", E.list E.int list )
        , ( "value", E.float <| clamp 0 1 volume )
        ]
    ]


encodeWheel : Wheel -> Bool -> String -> List ( String, E.Value )
encodeWheel w hasView parentUid =
    [ ( "mute", E.bool w.mute )
    , ( "volume", E.float <| clamp 0 1 w.volume )
    , ( "startPercent", E.float w.startPercent )
    , ( "view", E.bool hasView )
    ]
        ++ (case Wheel.getWheelContent w of
                Content.S s ->
                    [ ( "soundName", E.string <| Sound.toString s )
                    , ( "loopPoints", E.list E.float <| Sound.getLoopPoints s )
                    ]

                Content.M m ->
                    [ ( "mobile", encodeMobile m False parentUid ) ]

                Content.C c ->
                    [ ( "collar", encodeCollar c hasView parentUid ) ]
           )


encodeGear : Bool -> String -> Coll Geer -> Id Geer -> E.Value
encodeGear hasView parentUid coll id =
    let
        g =
            Coll.get id coll

        length =
            Harmo.getLength g.harmony coll

        uid =
            parentUid ++ Gear.toUID id
    in
    if length == 0 then
        Debug.log (uid ++ "’s length is 0") E.null

    else
        E.object
            ([ ( "id", E.string <| uid )
             , ( "length", E.float length )
             ]
                ++ encodeWheel g.wheel hasView uid
            )


encodeMobile : Mobeel -> Bool -> String -> E.Value
encodeMobile { motor, gears } hasView parentUid =
    E.object
        [ ( "length", E.float <| Harmo.getLengthId motor gears )
        , ( "gears", E.list (encodeGear hasView parentUid gears) <| Motor.getMotored motor gears )
        ]


encodeCollar : Colleer -> Bool -> String -> E.Value
encodeCollar c hasView parentUid =
    E.object
        [ ( "length", E.float <| Collar.getCumulLengthAt c.matrice c )
        , ( "loopStart", E.float c.loop )
        , ( "beads", E.list (encodeBead hasView parentUid) <| List.indexedMap (\i el -> ( i, el )) <| Collar.getBeads c )
        ]


encodeBead : Bool -> String -> ( Int, Beed ) -> E.Value
encodeBead hasView parentUid ( i, b ) =
    let
        uid =
            Content.beadUIDExtension parentUid i
    in
    E.object
        ([ ( "length", E.float b.length )
         , ( "id", E.string uid )
         ]
            ++ encodeWheel b.wheel hasView uid
        )
