import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (..) 
import Task
import Issues
import Dict exposing (..)
import Github exposing (..)

encoded_credentials = "replace me"

main =
  App.program
    { init = init "juanedi/foobar" encoded_credentials
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias ColumnId = Int
type alias Credentials = String

type alias Model =
  { columns : Dict ColumnId Issues.Model
  , markedIssue : Maybe Issue
  , repo : Repo
  , credentials : Credentials
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : Repo -> Credentials -> (Model, Cmd Msg)
init repo credentials =
  let
    (openModel, openCmd) = Issues.init repo "Open" (Just Open)
    (closedModel, closedCmd) = Issues.init repo "Closed" (Just Closed)
    columns = fromList [ (1, openModel)
                       , (2, closedModel)
                       ]
    commands = Cmd.batch [ Cmd.map (IssuesMsg 1) openCmd
                         , Cmd.map (IssuesMsg 2) closedCmd
                         ]
  in
    ({ columns = columns, markedIssue = Nothing, repo = repo, credentials = credentials}, commands)

-- UPDATE

type Msg = IssuesMsg ColumnId Issues.Msg
         | UpdateFail Http.Error
         | UpdateSucceed Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateFail error ->
      Debug.log "update fail!" (model, Cmd.none)

    UpdateSucceed issueId ->
      Debug.log "update ok!" (model, Cmd.none)

    IssuesMsg targetId Issues.Drop ->
      case model.markedIssue of
        Nothing ->
          Debug.crash "invalid state!"

        Just issue ->
          let
              sourceId = sourceCol issue
              source = column sourceId model
              target = column targetId model
          in
              if sourceId == targetId
              then
                ({ model | markedIssue = Nothing }, Cmd.none)
              else
                let
                    updatedIssue = { issue | state = id2State targetId }
                    (sourceModel, sourceCmd) = Issues.update (Issues.RemoveIssue issue) source
                    (targetModel, targetCmd) = Issues.update (Issues.AddIssue updatedIssue) target

                    columns = insert sourceId sourceModel
                            <| insert targetId targetModel model.columns
                in
                    {model | columns = columns, markedIssue = Nothing} ! [ Cmd.map (IssuesMsg sourceId) sourceCmd
                                                                         , Cmd.map (IssuesMsg targetId) targetCmd
                                                                         , updateIssueState updatedIssue model
                                                                         ]


    IssuesMsg _ (Issues.Mark issue) ->
      ({model | markedIssue = Just issue}, Cmd.none)

    IssuesMsg id msg ->
      let
        (newModel, cmd) = Issues.update msg (column id model)
      in
        ({ model | columns = insert id newModel model.columns }, Cmd.map (IssuesMsg id) cmd)

updateIssueState : Issue -> Model -> Cmd Msg
updateIssueState issue model =
  let
    stateLabel = case issue.state of
                   Open   -> "open"
                   Closed -> "closed"
    issueUrl =
     "https://api.github.com/repos/" ++ model.repo ++ "/issues/" ++ (toString issue.number)
  in
    Task.perform UpdateFail UpdateSucceed (patch responseDecoder issueUrl (Http.string ("{\"state\": \"" ++ stateLabel ++ "\" }")) model.credentials)

patch : Json.Decoder value -> String -> Http.Body -> Credentials -> Task.Task Http.Error value
patch decoder url body credentials =
  let
      request =
        { verb = "PATCH"
        , headers = [ ("Authorization", "Basic " ++ credentials) ]
        , url = url
        , body = body
        }
  in
      Http.fromJson decoder (Http.send Http.defaultSettings request)

responseDecoder = Json.succeed 42

column : ColumnId -> Model -> Issues.Model
column id model = case get id model.columns of
                    Nothing -> Debug.crash "invalid state! maybe you forgot to initialize a column"
                    Just column -> column

sourceCol : Issue -> ColumnId
sourceCol issue = case issue.state of
                    Open -> 1
                    Closed -> 2

targetCol : Issue -> ColumnId
targetCol issue = case issue.state of
                    Open -> 2
                    Closed -> 1

id2State : ColumnId -> IssueState
id2State id = case id of
                1 -> Open
                2 -> Closed
                _ -> Debug.crash "invalid state!"

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Themis Issues"]
    , br [] []
    , div [] [ markedView model ]
    , div [] <| List.intersperse separator <| List.map (uncurry columnView) (toList model.columns)
    ]

separator : Html Msg
separator = hr [] []

columnView : ColumnId -> Issues.Model -> Html Msg
columnView id model = App.map (IssuesMsg id) (Issues.view model)

markedView : Model -> Html Msg
markedView model = case model.markedIssue of
                     Nothing -> text ""
                     Just issue -> div [] [ text (toString issue.number) ]
