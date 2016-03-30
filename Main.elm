import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import Time exposing (..)

import StartApp.Simple as StartApp



--##############################################################################
--#### SIGNALS
--##############################################################################
-- model : Signal Model
-- model =
--   Signal.foldp update action initialModel

--##############################################################################
--#### MODEL
--##############################################################################
type alias Model =
  { latitude: Int
  , longitude: Int
  }

initialModel : Model
initialModel =
  { latitude = 40
  , longitude = 40
  }

--##############################################################################
--#### UPDATE
--##############################################################################
type Action
  = NoOp
  | IncreaseLat
  | DecreaseLat
  | IncreaseLng
  | DecreaseLng

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    IncreaseLat ->
      { model | latitude = model.latitude + 1 }
    DecreaseLat ->
      { model | latitude = model.latitude - 1 }
    IncreaseLng ->
      { model | longitude = model.longitude + 1 }
    DecreaseLng ->
      { model | longitude = model.longitude - 1 }

--##############################################################################
--#### VIEW
--##############################################################################
view : Address Action -> Model -> Html
view address initialModel =
  div
    [ id "container" ]
    [ text (toString initialModel.latitude)
    , button
        [ onClick address IncreaseLat ]
        [ text "Lat +" ]
    , button
        [ onClick address DecreaseLat ]
        [ text "Lat -" ]
    , text (toString initialModel.longitude)
    , button
        [ onClick address IncreaseLng ]
        [ text "Lng +" ]
    , button
        [ onClick address DecreaseLng ]
        [ text "Lng -" ]
    ]

--##############################################################################
--#### COMBINE
--##############################################################################
port modelChanges : Signal Model
port modelChanges =
  model

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp

actions : Signal Action
actions =
  inbox.signal

model : Signal Model
model =
  Signal.foldp update initialModel actions

main : Signal Html
main =
  Signal.map (view inbox.address) model
