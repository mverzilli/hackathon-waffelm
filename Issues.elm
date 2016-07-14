module Issues exposing (Model, Msg, init, update, view, IssueState(..))

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..)
import Task

type IssueState = Open | Closed

type alias IssueFilter = Maybe IssueState
type alias Repo = String

type alias Issue =
  { id : Int
  , title : String
  , state : IssueState
  , number : Int }

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
  | Drop
  | DragStart Issue

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed issues ->
      ({ model | issues = issues}, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    Drop ->
      -- TODO pensar :)
      Debug.log "Drop!" (model, Cmd.none)

    DragStart _ ->
      (model, Cmd.none)



-- VIEW
view : Model -> Html Msg
view model = div [] [ h1 [ on "drop" (Json.succeed Drop)
                         , attribute "ondragover" "event.preventDefault()"
                         ]
                         [ text model.title ]
                    , ul [] <| List.map issueView model.issues
                    ]

issueView : Issue -> Html Msg
issueView issue =
  li [ draggable "true"
     , on "dragstart" (Json.succeed (DragStart issue))
     ]
     [text <| "(#" ++ (toString issue.number) ++ ") " ++ issue.title]
