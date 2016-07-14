module Github exposing (..)

type IssueState = Open | Closed

type alias Issue =
  { id : Int
  , title : String
  , state : IssueState
  , number : Int }

type alias Repo = String

type alias IssueFilter = Maybe IssueState
