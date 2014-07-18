{-# LANGUAGE RecordWildCards, PatternGuards #-}
module Rules ( Rule (..)
             , Term (..)
             , RuleName 
             , Substitution
             , SentenceSchema
             , Sentence 
             , Variable
             , runRule
             , substitute
             , freeVariables
             , subst
             , getS
             , ignoreSubst
             , substituteRule
             , freeVariablesRule
             , skolemise
             , variableSetSubst
             ) where

import           Data.Monoid
import           Control.Arrow
import           Control.Applicative
import           Data.Maybe 
import           Data.List

data Term = Symbol String | Skolem String | Variable String [String] | List [Term] 
     deriving (Show, Eq)

type RuleName = String
data Substitution = S {getS :: [(Variable,Term)] } deriving Show
type SentenceSchema = Term
type Sentence = Term
type Variable = String

instance Monoid Substitution where 
  mappend (S d') s@(S d) = S (map (second (fromJust .  substitute s)) d' ++ d)
  mempty = S [] 

subst :: Variable -> Term -> Substitution 
subst v t = S [(v,t)]


data Rule = Rule { name       :: RuleName
                 , schematics :: [Variable]
                 , premises   :: [Rule]
                 , conclusion :: SentenceSchema
                 } deriving Show

-- First order unification as described by Robinson
mgu :: Term -> Term -> Maybe Substitution
mgu (Variable v _)  (Variable v' _) | v == v' = return mempty
mgu (Symbol t)    (Symbol t')       | t == t' = return mempty
mgu (Skolem t)    (Skolem t')       | t == t' = return mempty
mgu (List [])     (List [])               = return mempty
mgu (List (x:xs)) (List (y:ys))           = do sigma1 <- mgu  x y
                                               xs' <- List <$> mapM (substitute sigma1) xs
                                               ys' <- List <$> mapM (substitute sigma1) ys
                                               sigma2 <- mgu xs' ys'
                                               return (sigma1 <> sigma2)
mgu (Variable v k) t 
  | v `notElem` freeVariables t   
  , all (`elem` k) (skolems t) 
  = return (subst v t)
mgu t (Variable v k) 
  | v `notElem` freeVariables t   
  , all (`elem` k) (skolems t)
  = return (subst v t)
mgu _ _                                 = Nothing

runRule :: Rule -> [Variable] -> [Variable] -> Sentence -> [([Rule], Substitution)]
runRule (Rule {..}) sks fv str = let fs = freshen schematics (fv ++ sks)
                                  in maybeToList $ do
                                      sigma <- mgu str =<< substitute fs conclusion
                                      premises' <- mapM (substituteRule fs) premises
                                      return (premises', sigma)

        where freshen vars banned = mconcat $ snd $ mapAccumR forEachName banned vars
              forEachName banned v = let n = freshenName banned v
                                      in (n:banned , subst v (Variable n sks))

freshenName banned v = head $ dropWhile (`elem` banned) $ v : map (v ++) subscripts
    where subscripts = map show $ iterate (+1) 1

substituteRule :: Substitution -> Rule -> Maybe Rule 
substituteRule subst (Rule n vs ps c) = let
    subst' = ignoreSubst vs subst
 in Rule n vs <$> mapM (substituteRule subst) ps <*> substitute subst c

ignoreSubst :: [Variable] -> Substitution -> Substitution
ignoreSubst vs subst = S (filter ((`notElem` vs) . fst) $ getS subst )

skolems :: Term -> [Variable]
skolems (Variable v vs) = []
skolems (Skolem t) = [t]
skolems (Symbol s) = []
skolems (List ls) = ls >>= skolems


skolemise :: [Variable] -> Rule -> Rule 
skolemise banned (Rule {..}) = let renaming = map (id &&& freshenName banned) schematics
                                in Rule { name = name
                                        , schematics = schematics
                                        , premises = map (renameRule renaming) premises
                                        , conclusion = rename renaming conclusion 
                                        }
   where rename rs (Variable v vs) | Just x <- lookup v rs = Skolem x
         rename rs (List ls) = List (map (rename rs) ls)
         rename rs a = a
         renameRule rn (Rule {..}) = Rule { name = name
                                          , schematics = schematics
                                          , premises = map (renameRule renaming') premises
                                          , conclusion = rename renaming' conclusion
                                          }
           where renaming' = filter ((`notElem` schematics) . fst) rn

substitute :: Substitution -> SentenceSchema -> Maybe SentenceSchema
substitute sigma (Variable v vs) = case lookup v (getS sigma) of 
                                     Just t -> if not (any (`notElem` vs) (skolems t)) 
                                               then Just t else Nothing
                                     Nothing -> Just ( Variable v vs)
substitute sigma (List terms) = List <$> mapM (substitute sigma) terms
substitute _     x            = Just x

freeVariables :: Term -> [Variable]
freeVariables (Variable s _) = [s]
freeVariables (List t)     = t >>= freeVariables
freeVariables _  = []

freeVariablesRule :: [Variable] -> Rule -> [Variable]
freeVariablesRule sks (Rule {..}) = filter (`notElem` (schematics ++ sks)) $ freeVariables conclusion ++ concatMap (freeVariablesRule (schematics ++ sks)) premises


variableSetSubst :: Substitution -> [Variable] -> [Variable]
variableSetSubst (S ss) vs = (freeVariables =<< map snd ss) ++ filter (`notElem` map fst ss) vs
