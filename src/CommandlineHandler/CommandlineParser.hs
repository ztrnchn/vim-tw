module CommandlineHandler.CommandlineParser where

import Text.Parsec 
import Numeric.Natural


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


countParser :: Parsec String st Times
countParser = option 1 $ do
  first <- oneOf ['1'..'9']
  rest  <- many digit
  return (read (first : rest))

selectionParser :: Parsec String st [Char]
selectionParser = (:[]) <$> oneOf "hjklbe$0G" <|> string "gg" 

movementParser :: Parsec String st Movement
movementParser = (,) <$> countParser <*> selectionParser

directCommandParser :: Parsec String st DirectCommand
directCommandParser = (:[]) <$> oneOf "iv:aAxpu" <|> string "dd"  <|> string "yy"

repeatedCommandParser :: Parsec String st RepeatedCommand
repeatedCommandParser = (,) <$> countParser <*> ((:[]) <$> oneOf "xp")

selectionCommandParser :: Parsec String st SelectionCommand
selectionCommandParser = (,,) <$> countParser <*> ((:[]) <$> oneOf "dy") <*> movementParser

commandlineCommandParser :: Parsec String st String
commandlineCommandParser =
      try (string ":wq")
  <|> try (string ":help")
  <|> try (string ":w")
  <|> try (string ":q")


commandlineParser :: Parsec String st ValidInput
commandlineParser = 
      try (VSelectionCommand <$> selectionCommandParser)
  <|> try (VCommandlineCommand <$> commandlineCommandParser)
  <|> try (VMovement         <$> movementParser)
  <|> try (VDirectCommand    <$> directCommandParser)
  <|> try (VRepeatedCommand <$> repeatedCommandParser)
  


parseCommandline :: [Char] -> ValidInput
parseCommandline str = 
    case parse commandlineParser "" str of
        Right x ->  x 
        Left err -> NoOp
   
