
module Parser where
import Rules
import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Applicative

parse :: FilePath -> IO (Maybe [Rule])
parse path = do 
    strs <- zip [1..] . map stripComments . lines <$> readFile path
    x <- mapM parseRule $ filter (not . null) $ splitWhen (all isSpace . snd) strs
    return $ if not (any (any isNothing) x) then Just $ concatMap catMaybes x
                                            else Nothing
  where stripComments = takeWhile (/= '#')

parseRule :: [(Int, String)] -> IO [Maybe Rule]
parseRule input = let
     isVinculum = ("--" `isPrefixOf`) . dropWhile isSpace 
  in case split (keepDelimsL $ whenElt (isVinculum . snd)) input of
       [topLine,[vinc,bottomLine]] -> let
            tops = filter (any (not . isSpace)) 
                 $ concatMap (splitOn "  ") 
                 $ map snd topLine
            bot = unwords . words $ snd bottomLine
            nam = dropWhile (== '-') $ dropWhile isSpace $ snd vinc
         in if nam == "" then do 
              putStrLn $ "No name given after vinculum on line " ++ show (fst vinc)
              return [Nothing]
            else return [Just $ Rule (unwords $ words nam) (map (unwords . words) tops) bot]
       [one] -> do 
         putStrLn $ "No vinculum found for rule on line " ++ show (fst $ head one)
         return [Nothing]
       [one,_] -> do 
         putStrLn $ "Multiple lines found in conclusion of rule on line " ++ show (fst $ head one)
         return [Nothing]
       [] -> do 
         putStrLn "Unexpected error"
         return [Nothing]
       one:_:_ -> do 
         putStrLn $ "Multiple vinculi found for rule on line " ++ show (fst $ head one)
         return [Nothing]
