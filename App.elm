import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..) 
import Task

main =
  App.program
    { init = init "bcardiff/themis"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type IssueState = Open | Closed

type alias Issue =
  { id : Int
  , title : String
  , state : IssueState
  , number : Int }

type alias Model =
  {
    issues : List Issue
  }



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : String -> (Model, Cmd Msg)
init url =
  ( { issues = [] }
  , getIssues url
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


getIssues : String -> Cmd Msg
getIssues url =
  let
    issueUrl =
     "https://api.github.com/repos/" ++ url ++ "/issues?state=all"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeIssue issueUrl)


-- UPDATE


type Msg
  = FetchSucceed (List Issue)
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed issues ->
      ({ model | issues = issues}, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Themis Issues"]
    , br [] []
    , ul [] <| List.map issueView model.issues
    ]

issueView : Issue -> Html Msg
issueView issue =
  li [] [text <| "(#" ++ (toString issue.number) ++ ") " ++ issue.title]
