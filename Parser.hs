{-# LANGUAGE RecordWildCards #-}
module Parser where
import Rules
import Control.Applicative hiding ((<|>), many)
import Text.Parsec as P
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Maybe 
import Control.Monad

T.TokenParser {..} = T.makeTokenParser (haskellStyle { T.reservedOpNames = ["?", "==>", "axiom", "prove", "schematic"]
                                                     , T.reservedNames = ["axiom", "prove", "schematic"]
                                                     , T.opStart = opLetters
                                                     , T.opLetter = opLetters
                                                     , T.commentLine = ""
                                                     , T.commentStart = "(*"
                                                     , T.commentEnd = "*)"
                                                     , T.nestedComments = True
                                                     })
       where opLetters = oneOf "~!@$%^&*_+{}|:\"<>`,./;'\\-=" <|> T.identLetter haskellStyle 

type Parser a = Parsec String () a
     

term :: Parser Term
term =  flip Variable [] <$> variable 
    <|> Symbol   <$> try (operator >>= notHyphenString)
    <|> List     <$> parens (many term)
  where notHyphenString xs | any (not . (== '-')) xs = return xs
        notHyphenString xs | length xs < 3 = return xs
        notHyphenString _ = fail "hyphen string!"

hyphenString = do x <- operator
                  unless (all (== '-') x && length x >= 3) $ fail "not hyphen string!"

variable :: Parser Variable
variable = symbol "?" *> identifier

rule :: Parser Rule
rule = brackets (Rule . (fromMaybe "") <$> optionMaybe identifier <*> many variable <* symbol ":" <*> premises <*> (handle1 <$> many term) )
    <|> single <$> term
 where  premises = try (many rule <* reservedOp "==>") <|> pure []
        single = Rule "" [] [] 
 
script :: Parser Script 
script = (whiteSpace >>) $ Axiom <$ reserved "axiom" <*> toplevel <*> script
                       <|> goal  <$ reserved "prove" <*> many schematic <*> toplevel <*> script
                       <|> End   <$ eof
    where schematic = reserved "schematic" *> variable
          goal sc (Rule {..}) = Goal sc Rule { schematics = filter (`notElem` sc) schematics, ..}

handle1 [x] = x
handle1 xs  = List xs

toplevel :: Parser Rule
toplevel = do premises <- many rule
              hyphenString
              name <- identifier 
              conc <- (handle1 <$> many term)
              let r = Rule name [] premises conc
              return $ Rule name (freeVariablesRule [] r) premises conc



parse :: FilePath -> IO (Maybe Script)
parse path = do 
    input <- P.parse script path <$> readFile path
    case input of Right rs -> return (Just rs) 
                  Left e -> print e >> return Nothing 

parseTerm :: String -> Maybe Term
parseTerm s = case P.parse (handle1 <$> many term <* eof) "" s of
    Right t -> Just t 
    Left  _ -> Nothing 
