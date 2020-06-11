
module Interact exposing (..)

import Browser.Events as BE
import Html
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer --mod Pointers multitouch
import Json.Decode as D
import Math.Vector2 as Vec exposing (Vec2, vec2)
import Set exposing (Set) --mod Pointers multitouch
import Time


holdTime : Float
holdTime =
    500


type alias Interact item =
    Maybe ( item, Mode )


type Mode
    = Hover
    | Click
    | Hold
    | Drag


getInteract : State item zone -> Interact item
getInteract (S s) =
    case ( s.hover, s.click ) of
        ( Just item, Nothing ) ->
            Just ( item, Hover )

        ( _, Just { item, hold } ) ->
            case hold of
                Moving _ ->
                    Just ( item, Drag )

                Holding ->
                    Just ( item, Hold )

                Clicking ->
                    Just ( item, Click )

        _ ->
            Nothing


type State item zone
    = S
        { hover : Maybe item
        , click : Maybe (ClickState item zone)
        }


type alias ClickState item zone =
    { item : item
    , pos : Vec2
    , abs : Vec2
    , hold : HoldState zone
    , pointerId : Int --mod Pointers multitouch
    , pointerIds : Set Int --
    , keys : Mouse.Keys
    }


type HoldState zone
    = Clicking
    | Holding
    | Moving ( Vec2, zone )


init : State item zone
init =
    S
        { hover = Nothing
        , click = Nothing
        }


type Msg item zone
    = HoverIn item
    | HoverOut
    -- | StartClick item Vec2 Vec2 Mouse.Keys -- offsetPos clientPos --original
    | StartClick item Vec2 Vec2 Int Mouse.Keys -- offsetPos clientPos --mod
    -- | ClickMove zone Vec2 Vec2 --original
    | ClickMove zone Vec2 Vec2 Int --mod Pointers multitouch
    -- | ClickHold --original
    | ClickHold Int --mod Pointers multitouch
    -- | EndClick --original
    | EndClick Int --mod Pointers multitouch
    | AbortClick
    | NOOP


map : (a -> b) -> Msg a c -> Msg b c
map f m =
    case m of
        HoverIn a ->
            HoverIn (f a)

        -- StartClick a v c k -> --original
            -- StartClick (f a) v c k --
        StartClick a v c i k -> --mod Pointers multitouch
             StartClick (f a) v c i k --
             
        HoverOut ->
            HoverOut

        -- ClickMove z v c -> --original
            -- ClickMove z v c --
        ClickMove z v c i -> --mod Pointers multitouch
            ClickMove z v c i --

        -- ClickHold -> --original
            -- ClickHold --
        ClickHold i -> --mod Pointers multitouch
            ClickHold i --
            
        -- EndClick -> --original
            -- EndClick --
        EndClick i -> --mod Pointers multitouch
            EndClick i --
            
        AbortClick ->
            AbortClick

        NOOP ->
            NOOP


type alias Event item zone =
    { action : Action zone
    , item : item
    }


type Action zone
    = Clicked ( Bool, Bool, Bool )
    | Dragged (DragInfo zone) zone ( Bool, Bool, Bool ) -- Shift Ctrl Alt
    | DragIn
    | DragOut
    | MultiDragMode Int --mod Pointers multitouch
    -- | DragEnded Bool -- True for Up, False for Abort --original
    | DragEnded Bool Int --mod Pointers multitouch
    | Holded
    | HoldEnded


type alias DragInfo zone =
    { start : ( Vec2, zone )
    , oldPos : Vec2
    , newPos : Vec2
    , startD : Vec2
    , absD : Vec2
    }


update : Msg item zone -> State item zone -> ( State item zone, Maybe (Event item zone) )
update msg (S state) =
    case msg of
        HoverIn id ->
            ( S { state | hover = Just id }
            , Maybe.map (always <| Event DragIn id) state.click
            )

        HoverOut ->
            case state.hover of
                Just id ->
                    ( S { state | hover = Nothing }
                    , Maybe.map (always <| Event DragOut id) state.click
                    )

                Nothing ->
                    ( S state, Nothing )

        -- StartClick id pos abs keys -> --original
            -- ( S { state | click = Just <| ClickState id pos abs Clicking keys }, Nothing ) --
        StartClick id p a i keys -> --mod Pointers multitouch
            case state.click of --
                Just { item, pos, abs, hold, pointerId, pointerIds } -> --
                    ( S { state | click = Just <| ClickState item pos abs hold i (Set.insert i pointerIds) keys } --
                    , Nothing --
                    ) --
                    
                _ -> --
                    ( S { state | click = Just <| ClickState id p a Clicking i (Set.singleton i) keys } --
                    , Nothing  ) --

        ClickMove zone pos abs i ->
            case state.click of
                Just click ->
                    --let --original
                        --dragInit = --
                            --case click.hold of --
                                --Moving res -> --
                                    --res --

                                --_ -> --
                                    --( click.pos, zone ) --
                    if i == click.pointerId then --mod Pointers multitouch
                        let --
                            (dragInit, dragEvent) = --
                                case click.hold of --
                                    Moving res -> --
                                        (res
                                        , Just <| Event --
                                            (Dragged --
                                                { start = res --
                                                , oldPos = click.pos --
                                                , newPos = pos --
                                                , startD = Vec.sub abs click.abs --
                                                , absD = Vec.sub abs click.abs --
                                                } --
                                                zone <| tupleFromKeys click.keys --
                                            ) --
                                            click.item --
                                        ) --

                                    _ -> --
                                        ((click.pos, zone) --
                                        , Just <| Event (MultiDragMode <| Set.size click.pointerIds) click.item --
                                        ) --
                        
                        in
                        ( S
                            { state
                                 | click =
                                    Just { click | pos = pos, abs = abs, hold = Moving dragInit }
                            }
                        --, Just <| --original
                            --Event --
                                --(Dragged --
                                     --{ start = dragInit --
                                     --, oldPos = click.pos --
                                     --, newPos = pos --
                                     --, startD = Vec.sub abs click.abs --
                                     --, absD = Vec.sub abs click.abs --
                                     --} --
                                     --zone <| tupleFromKeys click.keys --
                                --) --
                                --click.item --
                        , dragEvent --mod Pointers multitouch
                        )
                            
                    else  --mod Pointers multitouch
                        ( S state, Nothing ) --
                            
                _ ->
                    ( S state, Nothing )

        -- ClickHold -> --original
        ClickHold i -> --mod Pointers multitouch
            case state.click of
                Just click ->
                    ( S { state | click = Just { click | hold = Holding } }
                    , Just <| Event Holded click.item
                    )

                _ ->
                    ( S state, Nothing )

        -- EndClick -> --original
        EndClick i -> --mod Pointers multitouch
            case state.click of
                --Just { item, hold, keys } -> --original
                Just { item, hold, keys, pointerIds } -> --mod Pointers multitouch
                    ( S { state | click = Nothing }
                    , case hold of
                        Moving _ ->
                            -- Just <| Event (DragEnded True) item --original
                            Just <| Event (DragEnded True <| Set.size pointerIds) item --mod Pointers multitouch
                            
                        Holding ->
                            Just <| Event HoldEnded item

                        Clicking ->
                            --Just <| Event (Clicked <| tupleFromKeys keys) item --original
                            case Set.size pointerIds of
                                2 -> --mod Pointers multitouch
                                    Just <| Event (Clicked (False, True, False)) item --
                                3 -> --
                                    Just <| Event (Clicked (True, False, False)) item --
                                _ -> --
                                    Just <| Event (Clicked <| tupleFromKeys keys) item --                              
                    )

                _ ->
                    ( S state, Nothing )

        AbortClick ->
            case state.click of
                Just { item, hold, keys, pointerIds } ->
                    ( S { state | click = Nothing }
                    , case hold of
                        Moving _ ->
                            Just <| Event (DragEnded False <| Set.size pointerIds) item

                        Holding ->
                            Just <| Event HoldEnded item

                        Clicking ->
                            Nothing
                    )

                _ ->
                    ( S state, Nothing )

        NOOP ->
            ( S state, Nothing )


subs : State item zone -> List (Sub (Msg item zone))
subs (S { click }) =
    case click of
        Nothing ->
            []

        --Just { hold } -> --original
            -- [ BE.onMouseUp <| D.succeed <| EndClick --
        Just { hold, pointerId } -> --mod Pointers multitouch
            [ BE.onMouseUp <| D.succeed <| EndClick 0 --
            , BE.onVisibilityChange
                (\v ->
                    Debug.log (Debug.toString v) <|
                        case v of
                            BE.Hidden ->
                                AbortClick

                            _ ->
                                NOOP
                )
            ]
                ++ (case hold of
                        Clicking ->
                            -- [ Time.every holdTime <| always ClickHold ] --original
                            [ Time.every holdTime <| always ( ClickHold pointerId ) ] --mod Pointers multitouch

                        _ ->
                            []
                   )


dragSpaceEvents : State item zone -> zone -> List (Html.Attribute (Msg item zone))
dragSpaceEvents (S { click }) zone =
    case click of
        Nothing ->--mod
            []

        Just _ ->
            -- [ Mouse.onMove <| \{ offsetPos, clientPos } -> ClickMove zone (vecFromTuple offsetPos) (vecFromTuple clientPos) ] --original
            [ Pointer.onMove <| \e -> ClickMove zone (vecFromTuple e.pointer.offsetPos) (vecFromTuple e.pointer.clientPos) e.pointerId --mod Pointers multitouch
            , Pointer.onUp <| \e -> EndClick e.pointerId ] --EndClick redundancy is needed to support multitouch


hoverEvents : item -> List (Html.Attribute (Msg item zone))
hoverEvents id =
    -- [ Mouse.onEnter <| always <| HoverIn id --original
    -- , Mouse.onLeave <| always HoverOut --
    -- ] --
    [ Pointer.onEnter <| always <| HoverIn id --mod Pointers multitouch
    , Pointer.onLeave <| always HoverOut --
    ] --


draggableEvents : item -> List (Html.Attribute (Msg item zone))
draggableEvents id =
    -- [ Mouse.onWithOptions "mousedown" { stopPropagation = True, preventDefault = False } <| --original
    --    \e -> StartClick id (vecFromTuple e.offsetPos) (vecFromTuple e.clientPos) e.keys --
    -- ] --
    [ Pointer.onWithOptions "pointerdown" { stopPropagation = True, preventDefault = False } <| --mod Pointers multitouch
      \e -> StartClick id (vecFromTuple e.pointer.offsetPos) (vecFromTuple e.pointer.clientPos) e.pointerId e.pointer.keys --
    ] -- 


-- MISC


tupleFromKeys : Mouse.Keys -> ( Bool, Bool, Bool )
tupleFromKeys { alt, shift, ctrl } =
    ( shift, ctrl, alt )


vecFromTuple : ( Float, Float ) -> Vec2
vecFromTuple t =
    vec2 (Tuple.first t) (Tuple.second t)
