import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

main =
  App.program
    { init = init "bcardiff/themis"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Issue = String

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


decodeIssue : Json.Decoder (List String)
decodeIssue =
  Json.list <| Json.at ["title"] Json.string



getIssues : String -> Cmd Msg
getIssues url =
  let
    issueUrl =
     "https://api.github.com/repos/" ++ url ++ "/issues?per_page=4"
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeIssue issueUrl)


-- UPDATE


type Msg
  = FetchSucceed (List String)
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
  li [] [text issue]
