module Main where

import Control.Applicative
import Data.Char

data Markdown
  = H1 String
  | H2 String
  | H3 String
  | H4 String
  | H5 String
  | H6 String
  | Text String
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser rp) = Parser $ \input -> do
    (input', x) <- rp input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser $ \input ->
  case input of
    x : xs | x == c -> Just (xs, x)
    _ -> Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

ws :: Parser String
ws = spanP isSpace

h1 :: Parser Markdown
h1 = H1 <$> (stringP "# " *> spanP isPrint)

h2 :: Parser Markdown
h2 = H2 <$> (stringP "## " *> spanP isPrint)

h3 :: Parser Markdown
h3 = H3 <$> (stringP "### " *> spanP isPrint)

h4 :: Parser Markdown
h4 = H4 <$> (stringP "#### " *> spanP isPrint)

h5 :: Parser Markdown
h5 = H5 <$> (stringP "##### " *> spanP isPrint)

h6 :: Parser Markdown
h6 = H6 <$> (stringP "###### " *> spanP isPrint)

text :: Parser Markdown
text = Text <$> spanP isPrint

markdown :: Parser Markdown
markdown = h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6 <|> text

readLines :: FilePath -> IO [String]
readLines fileName = do
  input <- readFile fileName
  return (lines input)

removeEmptyString :: [String] -> [String]
removeEmptyString = filter (not . null)

parse :: [String] -> [Maybe Markdown]
parse = map (\x -> snd <$> runParser markdown x)

parseFile :: FilePath -> IO ()
parseFile fileName = do
  input <- readLines fileName
  writeHtml . parse . removeEmptyString $ input

parseHtml :: Markdown -> Maybe String
parseHtml (H1 string) = Just ("<h1>" ++ filter (/= '#') string ++ "</h1>" ++ "\n")
parseHtml (H2 string) = Just ("<h2>" ++ filter (/= '#') string ++ "</h2>" ++ "\n")
parseHtml (H3 string) = Just ("<h3>" ++ filter (/= '#') string ++ "</h3>" ++ "\n")
parseHtml (H4 string) = Just ("<h4>" ++ filter (/= '#') string ++ "</h4>" ++ "\n")
parseHtml (H5 string) = Just ("<h5>" ++ filter (/= '#') string ++ "</h5>" ++ "\n")
parseHtml (H6 string) = Just ("<h6>" ++ filter (/= '#') string ++ "</h6>" ++ "\n")
parseHtml (Text string) = Just ("<p>" ++ string ++ "</p>" ++ "\n")

getString :: [Maybe String] -> String
getString [] = ""
getString [Nothing] = ""
getString xs = xs >>= foldr1 (\x s -> x ++ s)

writeHtml :: [Maybe Markdown] -> IO ()
writeHtml markdown = writeFile "output.html" (getString . map (>>= parseHtml) $ markdown)

main :: IO ()
main = undefined
