{-# LANGUAGE RecordWildCards, PatternGuards #-}
module Parser where
import Rules
import Control.Applicative hiding ((<|>), many)
import Text.Parsec as P
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Maybe 
import Control.Monad
import qualified Vec 

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
     

term :: (String -> a) -> Parser (Term a)
term f =  Variable . f <$> variable 
      <|> Symbol   <$> try (operator >>= notHyphenString)
      <|> List     <$> parens (many (term f))
  where notHyphenString xs | any (not . (== '-')) xs = return xs
        notHyphenString xs | length xs < 3 = return xs
        notHyphenString _ = fail "hyphen string!"

hyphenString = do x <- operator
                  unless (all (== '-') x && length x >= 3) $ fail "not hyphen string!"

variable :: Parser Variable
variable = symbol "?" *> identifier

rule :: (String -> a) -> Parser (Rule a)
rule f =  brackets fullRule
      <|> single <$> term (Right . f)
 where 
        single = Rule "" Vec.Nil []
        fullRule = do mn <- optionMaybe identifier 
                      Vec.ExI (Vec.Flip vs) <- Vec.fromList <$> many variable 
                      let f' x | Just x' <- Vec.findIx x vs = Left x'
                               | otherwise                  = Right (f x)
                      symbol ":"
                      premises <- try (many (rule f') <* reservedOp "==>") <|> pure []
                      conc <- handle1 <$> many (term f')
                      return $ Rule (fromMaybe "" mn) vs premises conc
 
script :: Parser Script 
script = (whiteSpace >>) $ Axiom . generalise <$ reserved "axiom" <*> toplevel <*> script
                       <|> (\a -> Obligation . makeObligation a) <$ reserved "prove" <*> many schematic <*> toplevel <*> script
                       <|> End   <$ eof
    where schematic = reserved "schematic" *> variable

handle1 [x] = x
handle1 xs  = List xs

toplevel :: Parser Toplevel
toplevel = do premises <- many (rule id) 
              hyphenString
              name <- identifier 
              conc <- (handle1 <$> many (term id))
              return $ Toplevel name premises conc



parse :: FilePath -> IO (Maybe Script)
parse path = do 
    input <- P.parse script path <$> readFile path
    case input of Right rs -> return (Just rs) 
                  Left e -> print e >> return Nothing 

parseTerm :: String -> Maybe (Term SkolemsAndSchematics)
parseTerm s = case P.parse (handle1 <$> many (term (flip Schematic [])) <* eof) "" s of -- todo allow local skolems
    Right t -> Just t 
    Left  _ -> Nothing 
