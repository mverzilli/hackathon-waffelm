import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..) 
import Task
import Issues
import Github exposing (..)

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
  , markedIssue : Maybe Issue
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : String -> (Model, Cmd Msg)
init url =
  let
    (openModel, openCmd) = Issues.init url "Open" (Just Open)
    (closedModel, closedCmd) = Issues.init url "Closed" (Just Closed)
  in
    {openIssues = openModel, closedIssues = closedModel, markedIssue = Nothing}
    ! [Cmd.map (IssuesMsg Open) openCmd
      , Cmd.map (IssuesMsg Closed) closedCmd]

-- UPDATE

type Msg = IssuesMsg IssueState Issues.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    IssuesMsg Open msg ->
      let
        (newModel, cmd) = Issues.update msg model.openIssues
      in
        ({ model | openIssues = newModel }, Cmd.map (IssuesMsg Open) cmd)

    IssuesMsg Closed msg ->
      let
        (newModel, cmd) = Issues.update msg model.closedIssues
      in
        ({model | closedIssues = newModel }, Cmd.map (IssuesMsg Closed) cmd)

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Themis Issues"]
    , br [] []
    , div [] [App.map (IssuesMsg Open) (Issues.view model.openIssues)
             , hr [] []
             , App.map (IssuesMsg Closed) (Issues.view model.closedIssues)]
    ]
