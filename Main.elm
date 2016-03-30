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
  { counter: Int
  }

initialModel : Model
initialModel =
  { counter = 0
  }

--##############################################################################
--#### UPDATE
--##############################################################################
type Action
  = NoOp
  | Increase
  | Decrease

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Increase ->
      { model | counter = model.counter + 1 }

    Decrease ->
      { model | counter = model.counter - 1 }

--##############################################################################
--#### VIEW
--##############################################################################
view : Address Action -> Model -> Html
view address initialModel =
  div
    [ id "container" ]
    [ text (toString initialModel.counter),
      button
        [ onClick address Increase ]
        [ text "Increase" ],
      button
        [ onClick address Decrease ]
        [ text "Decrease" ]
    ]

--##############################################################################
--#### COMBINE
--##############################################################################
port modelChanges : Signal Model
port modelChanges =
  model

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox Increase

actions : Signal Action
actions =
  inbox.signal

model : Signal Model
model =
  Signal.foldp update initialModel actions

main : Signal Html
main =
  Signal.map (view inbox.address) model
