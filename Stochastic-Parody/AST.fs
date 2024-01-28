module AST

type TranslationUnit = {word: string; translate : bool; rhyme: bool}

type Grammar =
| Sentiment of string
| Keywords of string List
| Line of TranslationUnit List
| Section of string * (Grammar List)
| Section_Instance of string

