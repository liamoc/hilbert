{-# LANGUAGE RecordWildCards #-}
module Rules ( Rule (..)
             , RuleName 
             , Substitution
             , SentenceSchema
             , Sentence 
             , Variable
             , runRule
             , substitute
             , freeVariables
             ) where

import qualified Data.Map                       as M
import           Data.Maybe

type RuleName = String
type Substitution = M.Map Variable String
type SentenceSchema = String
type Sentence = String
type Variable = Char

data Rule = Rule { name       :: RuleName
                 , premises   :: [SentenceSchema]
                 , conclusion :: SentenceSchema
                 } deriving Show

data SchemaMatcher = Running SentenceSchema Substitution deriving Show

schemaToMachine :: SentenceSchema -> SchemaMatcher
schemaToMachine = flip Running M.empty

delta :: Char -> SchemaMatcher -> [Maybe SchemaMatcher]
delta c (Running s@('?':n:r) theta) = let 
       sigma1 = (M.insertWith (flip (++)) n [c] theta )
       sigma2 = M.alter (Just . fromMaybe "") n theta
    in (Just $ Running s sigma1) : delta c (Running (substitute' n (sigma2 M.! n) r) sigma2)
  where substitute' k v ('?':n':ns) | k == n' = v ++ substitute' k v ns
                                    | k /= n' = '?':n':substitute' k v ns
        substitute' k v (n':ns) = n':substitute' k v ns
        substitute' _ _ []      = []
delta c (Running (x:xs) m) | x /= c = [Nothing]
                           | x == c = [Just $ Running xs m]
delta _ (Running _ _) = [Nothing]

runString :: Sentence -> SchemaMatcher -> [Substitution]
runString [] (Running l m) | allGlobs l = [blankGlobs m l]
  where allGlobs ('?':_:r) = allGlobs r
        allGlobs []        = True
        allGlobs  _        = False
        blankGlobs acc ('?':x:r) = blankGlobs (M.alter (Just . fromMaybe "") x acc) r
        blankGlobs acc _         = acc
runString [] _               = []
runString (x:xs) m = concatMap (runString xs) (catMaybes $ delta x m)

runRule :: Rule -> Sentence -> [[SentenceSchema]]
runRule (Rule {..}) str = case runString (unwords $ words str) (schemaToMachine conclusion)
                            of [] -> []
                               list -> map (\m -> map (substitute m) premises) list

substitute :: Substitution -> SentenceSchema -> SentenceSchema
substitute m ('?':n:r) = case M.lookup n m
                                  of Just x  -> x ++ substitute m r
                                     Nothing -> '?':n:substitute m r
substitute m (x:xs)    = x:substitute m xs
substitute _ []        = []


freeVariables :: SentenceSchema -> [Variable]
freeVariables ('?':x:xs) = x : freeVariables xs
freeVariables (_:xs)     = freeVariables xs
freeVariables []         = []
