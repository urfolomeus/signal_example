module SeatSaver where

import Html exposing (..)
import Html.Events exposing (..)


main =
  Signal.map  (view actions.address) model


port saveSeat : Signal Int


port reserveSeat : Signal String
port reserveSeat =
  Signal.map toString model


-- MODEL

type alias Seat =
  { seatNo: Int
  , occupied: Bool
  }


type alias Model =
  { seats : List Seat
  , nextSeatNo : Int
  }


newSeat : Int -> Bool -> Seat
newSeat number occupied =
  { seatNo = number
  , occupied = occupied
  }


initialModel : Model
initialModel =
  { seats =
    [ (newSeat 1 False)
    , (newSeat 2 True)
    , (newSeat 3 False)
    ]
  , nextSeatNo = 4
  }



-- UPDATE

type Action
  = NoOp
  | Mark Int


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Mark seatNo ->
      let
        updateSeat s =
          if s.seatNo == seatNo then { s | occupied <- (not s.occupied) } else s
      in
        { model | seats <- List.map updateSeat model.seats }


model : Signal Model
model =
  Signal.foldp update initialModel allSignals


allSignals: Signal Action
allSignals = Signal.merge actions.signal inputs


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


inputs : Signal Action
inputs =
    Signal.map (\seatNo -> Mark seatNo) saveSeat


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  ul [ ] (List.map (seatItem address) model.seats)


seatItem address seat =
  li [ onClick address (Mark seat.seatNo) ] [ text (toString seat)]
