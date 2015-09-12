module SeatSaver where

import Html exposing (..)
import Html.Events exposing (..)


main =
  Signal.map (view seatsToReserve.address) model


port seats : Signal (List Seat)


port updateSeat : Signal Int


port reserveSeat : Signal Seat
port reserveSeat =
  seatsToReserve.signal


-- MODEL

type alias Seat =
  { seatNo : Int
  , occupied : Bool
  }


type alias Model = List Seat


-- UPDATE

type Action
  = NoOp
  | Mark Int
  | Add (List Seat)


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
        List.map updateSeat model

    Add seats ->
      List.append seats model


model : Signal Model
model =
  Signal.foldp update [] allSignals


allSignals : Signal Action
allSignals =
  Signal.merge seatUpdates addSeats


seatUpdates : Signal Action
seatUpdates =
  Signal.map (\seatNo -> Mark seatNo) updateSeat


addSeats : Signal Action
addSeats =
  Signal.map (\seats -> Add seats) seats


seatsToReserve : Signal.Mailbox Seat
seatsToReserve =
  Signal.mailbox (Seat 0 False)


-- VIEW

view : Signal.Address Seat -> Model -> Html
view address model =
  ul [ ] (List.map (seatItem address) model)


seatItem address seat =
  li [ onClick address seat ] [ text (toString seat)]
