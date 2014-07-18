{-# LANGUAGE RecordWildCards #-}
module Parser where
import Rules
import Control.Applicative hiding ((<|>), many)
import Text.Parsec as P
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Maybe 
import Control.Monad

T.TokenParser {..} = T.makeTokenParser (haskellStyle { T.reservedOpNames = ["?", "]"]
                                                     , T.reservedNames = []
                                                     , T.opStart = opLetters
                                                     , T.opLetter = opLetters
                                                     })
       where opLetters = oneOf "~!@#$%^&*_+{}|:\"<>?`,./;'[]\\-="

type Parser a = Parsec String () a

term :: Parser Term
term =  Variable <$> variable 
    <|> Symbol   <$> identifier  
    <|> Symbol   <$> operator
    <|> List     <$> parens (many term)

variable :: Parser Variable
variable = reservedOp "?" *> identifier

rule :: Parser Rule
rule = brackets (Rule . (fromMaybe "") <$> optionMaybe identifier <*> many variable <* symbol ":" <*> premises <*> (handle1 <$> many term) )
 where  premises = many rule <* symbol "==>" <|> pure []

handle1 [x] = x
handle1 xs  = List xs

definition :: Parser Rule
definition =  do x <- symbol "axiom" *> rule
                 when (name x == "") $ fail "Need to provide a name for top-level rule" 
                 return x

parse :: FilePath -> IO (Maybe [Rule])
parse path = do 
    input <- P.parse (many definition <* eof) path <$> readFile path
    case input of Right rs -> return (Just rs) 
                  Left e -> print e >> return Nothing 

parseTerm :: String -> Maybe Term
parseTerm s = case P.parse (handle1 <$> many term <* eof) "" s of
    Right t -> Just t 
    Left  _ -> Nothing 
