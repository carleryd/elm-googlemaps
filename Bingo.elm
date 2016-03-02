module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String exposing (toUpper, repeat, trimRight)

import StartApp.Simple as StartApp
import BingoUtils as Utils

--##############################################################################
-- MODEL
--##############################################################################

type alias Entry =
  { phrase: String,
    points: Int,
    wasSpoken: Bool,
    id: Int
  }

type alias Model =
  { entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextID: Int
  }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  { phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

initialModel : Model
initialModel =
  { entries = [ ],
    phraseInput = "",
    pointsInput = "",
    nextID = 1
  }

--##############################################################################
-- UPDATE
--##############################################################################

type Action -- Union Type, either NoOp or Sort
  = NoOp
  | Sort
  | Delete Int
  | Mark Int
  | UpdatePhraseInput String
  | UpdatePointsInput String
  | Add

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Sort ->
      { model | entries = List.sortBy .points model.entries }

    Delete id ->
      let
        remainingEntries =
          List.filter (\e -> e.id /= id) model.entries
      in
        { model | entries = remainingEntries }

    Mark id ->
      let
        updateEntry e =
          if e.id == id then { e | wasSpoken = (not e.wasSpoken) }
          else e
      in
        { model | entries = List.map updateEntry model.entries }

    UpdatePhraseInput contents ->
      { model | phraseInput = contents }


    UpdatePointsInput contents ->
      { model | pointsInput = contents }


    Add ->
      let
        entryToAdd =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextID
        isInvalid model =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalid model
        then model
        else
          { model |
              phraseInput = "",
              pointsInput = "",
              entries = entryToAdd :: model.entries,
              nextID = model.nextID + 1
          }


--##############################################################################
-- VIEW
--##############################################################################

title : String -> Int -> Html
title message times =
  message ++ " "
    |> String.repeat 3
    |> String.toUpper
    |> String.trimRight
    |> text

pageHeader : Html
pageHeader =
  h1
    [ ]
    [ title "bingo!" 3 ]

pageFooter : Html
pageFooter =
  footer
    [ ]
    [ a [ href "https://pragmaticstudios.com" ]
      [ text "The Pragmatic Studios" ]
    ]

entryItem : Address Action -> Entry -> Html
entryItem address entry =
  li
    [ classList [ ("highlight", entry.wasSpoken) ],
      onClick address (Mark entry.id)
    ]
    [ span [ class "phrase" ] [ text entry.phrase ],
      span [ class "points" ] [ text (toString entry.points) ],
      button
        [ class "delete", onClick address (Delete entry.id) ]
        [ ] -- child elements
    ]

totalPoints : List Entry -> Int
totalPoints entries =
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEntries)

totalItem : Int -> Html
totalItem total =
  li
    [ class "total" ]
    [ span [ class "label" ] [ text "Total" ],
      span [ class "points" ] [ text (toString total) ]
    ]

entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = List.map (entryItem address) entries
    items = entryItems ++ [ totalItem (totalPoints entries) ]
  in
    ul [ ] items

entryForm : Address Action -> Model -> Html
entryForm address model =
  div [ ]
    [ input
        [ type' "text", -- type' function
          placeholder "Phrase",
          value model.phraseInput,
          name "phrase",
          autofocus True,
          Utils.onInput address UpdatePhraseInput
        ]
        [ ],
      input
        [ type' "number", -- type' is a function
          placeholder "Points",
          value model.pointsInput,
          name "points",
          Utils.onInput address UpdatePointsInput
        ]
        [ ],
      button
        [ class "add", onClick address Add ]
        [ text "Add" ],
      h2
        [ ]
        [ text (model.phraseInput ++ " " ++ model.pointsInput) ]
    ]

view : Address Action -> Model -> Html
view address model =
  div
    [ id "container" ]
    [ pageHeader,
      entryForm address model,
      entryList address model.entries,
      button
        [ class "sort", onClick address Sort ]
        [ text "Sort" ],
      pageFooter
    ]

-- WIRE IT ALL TOGETHER

main : Signal Html
main =
  StartApp.start
    { model = initialModel,
      view = view,
      update = update
    }
  -- view (update Sort initialModel)
  --initialModel
  --  |> update Sort
  --  |> view
