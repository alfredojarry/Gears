module Editor.Collar exposing (..)

import Collar exposing (Colleer)
import Color
import Content exposing (Content)
import Editor.Common exposing (..)
import Element exposing (Element, text)
import Engine
import Html.Attributes
import Interact
import Json.Encode as E
import Math.Vector2 as Vec exposing (vec2)
import PanSvg
import Sound exposing (Sound)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Length(..))
import Wheel exposing (Wheel)


type alias Model =
    { tool : Tool
    , cursor : Int
    , interact : Interact.State Interactable
    , svg : PanSvg.Model
    }


type Tool
    = Play Bool


type Interactable
    = Ignore
    | IReizeHandle Int Bool


fromWheelInteractable : Wheel.Interactable Int -> Interactable
fromWheelInteractable i =
    case i of
        Wheel.IWheel id ->
            Ignore

        Wheel.IResizeHandle id bool ->
            IReizeHandle id bool


init : Colleer -> PanSvg.Model -> Model
init c svg =
    { tool = Play False
    , cursor = 0
    , interact = Interact.init
    , svg =
        { svg
            | viewPos =
                { c = Vec.add leftmostPoint <| vec2 (Collar.getTotalLength c / 2) 0
                , smallestSize = 8 * Collar.getMaxLength c
                }
        }
    }


type Msg
    = ToggleEngine
    | SoundClicked Sound
    | NewBead (Content Wheel)
    | SvgMsg PanSvg.Msg
    | InteractMsg (Interact.Msg Interactable)


type alias Return =
    { model : Model
    , collar : Colleer
    , toUndo : ToUndo
    , toEngine : Maybe E.Value
    }


update : Msg -> ( Model, Colleer ) -> Return
update msg ( model, collar ) =
    let
        return =
            { model = model
            , collar = collar
            , toUndo = NOOP
            , toEngine = Nothing
            }
    in
    case msg of
        ToggleEngine ->
            { return | toEngine = Just <| Engine.playCollar collar }

        SoundClicked s ->
            update (NewBead <| Content.S s) ( model, collar )

        NewBead c ->
            { return | collar = Collar.add model.cursor (Collar.beadFromContent c) collar, toUndo = Do }

        SvgMsg subMsg ->
            { return | model = { model | svg = PanSvg.update subMsg model.svg } }

        InteractMsg subMsg ->
            return


subs : Model -> List (Sub Msg)
subs { interact } =
    (Sub.map SvgMsg <| PanSvg.sub)
        :: (List.map (Sub.map InteractMsg) <| Interact.subs interact)


leftmostPoint : Vec.Vec2
leftmostPoint =
    vec2 0 0


viewContent : ( Model, Colleer ) -> Element Msg
viewContent ( model, collar ) =
    Element.html <|
        S.svg
            (List.map (Html.Attributes.map SvgMsg) (PanSvg.svgAttributes model.svg)
                ++ (List.map (Html.Attributes.map InteractMsg) <| Interact.dragSpaceEvents model.interact)
            )
        <|
            List.map (Svg.map <| InteractMsg << Interact.map fromWheelInteractable)
                (List.foldl
                    (\b ( l, ( p, i ) ) ->
                        ( Wheel.view b.wheel
                            (vec2 (p + b.length / 2) <| Vec.getY leftmostPoint)
                            b.length
                            { mod = Wheel.None, motor = False, dashed = False }
                            i
                            (Collar.toUID i)
                            :: l
                        , ( p + b.length
                          , i + 1
                          )
                        )
                    )
                    ( [], ( Vec.getX leftmostPoint, 0 ) )
                    (Collar.getBeads collar)
                    |> Tuple.first
                )
                ++ viewCursor model collar


viewCursor : Model -> Colleer -> List (Svg msg)
viewCursor { cursor } c =
    let
        medLength =
            Collar.getMinLength c + Collar.getMaxLength c / 2

        cursorW =
            medLength / 15

        cursorH =
            medLength * 2
    in
    [ S.rect
        [ SA.x <| Num <| Collar.getLengthAt cursor c - cursorW / 2
        , SA.y <| Num <| -cursorH / 2
        , SA.width <| Num cursorW
        , SA.height <| Num cursorH
        , SA.fill <| Fill Color.lightBlue
        ]
        []
    ]


viewTools : Model -> Element msg
viewTools model =
    text "TOOLS"
