module CommandlineHandler.CommandlineParser where

import Text.Parsec
import Numeric.Natural
import Control.Applicative (asum)

import Domain.DomainTypes (EditorMode (VisualMode))

type Times = Natural

type Movement = (Natural, [Char])

type SelectionCommand = (Natural, [Char], Movement)

type RepeatedCommand = (Natural, [Char])

type DirectCommand = [Char]

type CommandlineCommand = [Char]

data ValidInput =
    VSelectionCommand SelectionCommand
  | VMovement Movement
  | VDirectCommand DirectCommand
  | VRepeatedCommand RepeatedCommand
  | VCommandlineCommand CommandlineCommand
  | NoOp
  deriving (Eq, Show)


countParser :: Parsec String EditorMode Times
countParser = option 1 $ do
  first <- oneOf ['1'..'9']
  rest  <- many digit
  return (read (first : rest))

selectionParser :: Parsec String EditorMode [Char]
selectionParser = (:[]) <$> oneOf "hjklbe$0G" <|> string "gg"

movementParser :: Parsec String EditorMode Movement
movementParser = (,) <$> countParser <*> selectionParser

directCommandParser :: Parsec String EditorMode DirectCommand
directCommandParser = do
    mode <- getState
    asum
        ( [ (:[]) <$> oneOf "iv:aAxpu"
          , try (string "dd")
          , try (string "yy")
          ]
          ++ [string "d" | mode == VisualMode]
          ++ [string "y" | mode == VisualMode]
        )

repeatedCommandParser :: Parsec String EditorMode RepeatedCommand
repeatedCommandParser = (,) <$> countParser <*> ((:[]) <$> oneOf "xp")

selectionCommandParser :: Parsec String EditorMode SelectionCommand
selectionCommandParser = (,,) <$> countParser <*> ((:[]) <$> oneOf "dy") <*> movementParser

commandlineCommandParser :: Parsec String EditorMode String
commandlineCommandParser =
      try (string ":wq")
  <|> try (string ":help")
  <|> try (string ":w")
  <|> try (string ":q")


commandlineParser :: Parsec String EditorMode ValidInput
commandlineParser =
      try (VSelectionCommand <$> selectionCommandParser)
  <|> try (VCommandlineCommand <$> commandlineCommandParser)
  <|> try (VMovement         <$> movementParser)
  <|> try (VDirectCommand    <$> directCommandParser)
  <|> try (VRepeatedCommand <$> repeatedCommandParser)



parseCommandline :: EditorMode -> [Char] -> ValidInput
parseCommandline mode str =
    case runParser commandlineParser mode "" str of
        Right x ->  x
        Left err -> NoOp

