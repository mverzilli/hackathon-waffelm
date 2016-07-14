import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..) 
import Task
import Issues

main =
  App.program
    { init = init "bcardiff/themis"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  {
    openIssues : Issues.Model
  , closedIssues : Issues.Model
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : String -> (Model, Cmd Msg)
init url =
  let
    (openModel, openCmd) = Issues.init url "Open" (Just Issues.Open)
    (closedModel, closedCmd) = Issues.init url "Closed" (Just Issues.Closed)
  in
    {openIssues = openModel, closedIssues = closedModel}
    ! [Cmd.map OpenMsg openCmd
      , Cmd.map ClosedMsg closedCmd]

-- UPDATE


type Msg
  = OpenMsg Issues.Msg
  | ClosedMsg Issues.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OpenMsg msg ->
      let
        (newModel, cmd) = Issues.update msg model.openIssues
      in
        ({ model | openIssues = newModel }, Cmd.map OpenMsg cmd)
    ClosedMsg msg ->
      let
        (newModel, cmd) = Issues.update msg model.closedIssues
      in
        ({model | closedIssues = newModel }, Cmd.map ClosedMsg cmd)

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Themis Issues"]
    , br [] []
    , div [] [App.map OpenMsg (Issues.view model.openIssues)
             , hr [] []
             , App.map ClosedMsg (Issues.view model.closedIssues)]
    ]
