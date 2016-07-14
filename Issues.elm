module Issues exposing (Model, Msg(Mark, Drop, AddIssue, RemoveIssue), init, update, view)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..)
import Task
import Github exposing (..)

type alias Model =
  { title : String
  , issues : List Issue
  }



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : Repo -> String -> IssueFilter -> (Model, Cmd Msg)
init repo title filter =
  ( { title = title, issues = [] }
  , getIssues repo filter
  )


decodeIssue : Json.Decoder (List Issue)
decodeIssue =
  Json.list <| Json.object4 Issue
    ("id" := Json.int)
    ("title" := Json.string)
    ("state" := issueStateDecoder)
    ("number" := Json.int)

issueStateDecoder : Json.Decoder IssueState
issueStateDecoder = Json.string `andThen`
                    (\s -> case s of
                             "open" -> succeed Open
                             "closed" -> succeed Closed
                             _ -> fail <| "Unknown issue state from GH API (" ++ s ++ ")")


getIssues : Repo -> IssueFilter -> Cmd Msg
getIssues repo filter =
  let
    query = case filter of
              Nothing -> "all"
              Just Open -> "open"
              Just Closed -> "closed"
    issueUrl =
     "https://api.github.com/repos/" ++ repo ++ "/issues?state=" ++ query
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeIssue issueUrl)


-- UPDATE


type Msg
  = FetchSucceed (List Issue)
  | FetchFail Http.Error
  | AddIssue Issue
  | RemoveIssue Issue
  | Mark Issue
  | Drop
  | DragStart Issue

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed issues ->
      ({ model | issues = issues}, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    AddIssue issue ->
      (addIssue issue model, Cmd.none)

    RemoveIssue issue ->
      (removeIssue issue model, Cmd.none)

    Mark issue ->
      (model, Cmd.none)

    Drop ->
      Debug.log "Drop!" (model, Cmd.none)

    DragStart _ ->
      (model, Cmd.none)


addIssue : Issue -> Model -> Model
addIssue issue model = { model | issues = issue :: model.issues }

removeIssue : Issue -> Model -> Model
removeIssue issue model = { model | issues = List.filter ((/= ) issue) model.issues }


-- VIEW
view : Model -> Html Msg
view model = div [ on "drop" (Json.succeed Drop)
                 , attribute "ondragover" "event.preventDefault()"
                 ]
                 [ h1 [ ]
                      [ text model.title ]
                 , ul [] <| List.map issueView model.issues
                 ]

issueView : Issue -> Html Msg
issueView issue =
  li [ draggable "true"
     , on "dragstart" (Json.succeed (Mark issue))
     ]
     [text <| "(#" ++ (toString issue.number) ++ ") " ++ issue.title]
