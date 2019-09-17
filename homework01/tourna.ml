type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let team_as_name t = match t with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina-> "Argentina"

let rec parenize t = match t with
  | LEAF n -> (team_as_name n)
  | NODE (a, b) -> (String.concat "" ["("; parenize(a); " "; parenize(b); ")"])
