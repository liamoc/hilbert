{-# LANGUAGE RecordWildCards, PatternGuards, ExistentialQuantification, DeriveFoldable, DeriveTraversable, DeriveFunctor, GADTs, DataKinds, KindSignatures, EmptyDataDecls #-}
module Rules where

import Data.Monoid
import Control.Arrow
import Control.Applicative
import Data.Maybe 
import Data.Foldable (toList, Foldable(..))
import Data.Traversable
import Debug.Trace
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Monad
import Vec hiding (head)
data Term v = Symbol String | Variable v | List [Term v] 
     deriving (Show, Eq, Foldable, Traversable,Functor)


instance Monad Term where 
  return = Variable 
  Symbol x   >>= _     = Symbol x 
  Variable v >>= sigma = sigma v 
  List vs    >>= sigma = List $ map (>>= sigma) vs

data SkolemsAndSchematics = Skolem Variable
                          | Schematic Variable [Variable]
                          deriving (Show)

instance Eq SkolemsAndSchematics where 
  Skolem a      == Skolem b      = a == b
  Schematic a _ == Schematic b _ = a == b
  _             == _             = False

type RuleName = String
type Variable = String
data Void
type GoalTerm = Term SkolemsAndSchematics

type LocalRule = Rule SkolemsAndSchematics
type Axiom = Rule Void



data Rule v = forall (t :: Nat).
              Rule { name       :: RuleName
                   , binders    :: Vec t Variable
                   , premises   :: [Rule (Either (Fin t) v)]
                   , conclusion :: Term (Either (Fin t) v)
                   }


instance Show (Rule a) where 
  show = const "blah"

(~>) :: (Eq a) => a -> Term a -> a -> Term a
(v ~> t) e | v == e = t 
           | otherwise = Variable e

data Goal = Goal { boundSkolems :: [Variable]
                 , assumptions :: [LocalRule]
                 , goal :: GoalTerm
                 }

data ApplicatedRule = AR { aName :: RuleName
                         , aPremises :: [LocalRule]
                         , aConclusion :: GoalTerm
                         }

data Script = Axiom Axiom Script | Obligation (RuleName, Goal) Script | End 

type Unifier = SkolemsAndSchematics -> Term SkolemsAndSchematics

-- First order unification as described by Robinson
mgu :: GoalTerm -> GoalTerm -> Maybe Unifier
mgu a b | a == b = return return
mgu (List (x:xs)) (List (y:ys)) = do 
  sigma1 <- mgu  x y
  sigma2 <- List ( map (>>= sigma1) xs) `mgu` List ( map (>>= sigma1) ys)
  return (sigma1 >=> sigma2)
mgu (Variable (Schematic v k)) t 
  | v `notElem` allSchematics t   
  , all (`elem` k) (allSkolems t) 
  = return (Schematic v k ~> t)
mgu t (Variable (Schematic v k)) 
  | v `notElem` allSchematics t   
  , all (`elem` k) (allSkolems t)
  = return (Schematic v k ~> t)
mgu _ _ = Nothing


asSchematic :: SkolemsAndSchematics -> Maybe Variable 
asSchematic (Skolem _)      = Nothing 
asSchematic (Schematic v _) = Just v

asSkolem :: SkolemsAndSchematics -> Maybe Variable 
asSkolem (Skolem v)      = Just v
asSkolem (Schematic _ _) = Nothing
           
allSchematics, allSkolems :: GoalTerm -> [Variable] 
allSchematics = mapMaybe asSchematic . toList 
allSkolems = mapMaybe asSkolem . toList

allInRule :: (a -> Maybe Variable) -> Rule a -> [Variable]
allInRule f (Rule {..}) = mapMaybe f' (toList conclusion) ++ concatMap (allInRule f') premises
  where f' = either (const Nothing) f

data KnownVars = KnownVars { knownSkolems    :: BoundSkolems
                           , knownSchematics :: KnownSchematics
                           , knownRules      :: [RuleName]
                           }
type BoundSkolems = [Variable]
type KnownSchematics = [Variable]

substRule :: Rule a -> (a -> Term b) -> Rule b
substRule (Rule {..}) f = Rule { conclusion = conclusion  >>= newSubst
                               , premises = map (`substRule` newSubst) premises
                               , ..
                               }
  where newSubst = either (return . Left) (runEitherT . lift . f)

-- skolemise and applicate seem like duals
skolemise :: KnownVars -> Rule SkolemsAndSchematics -> Goal
skolemise gamma (Rule {..}) = let (newSkolems, newNames) = mapAccumR forEachName [] binders
                                  subst (Left x)  = newNames `at` x
                                  subst (Right x) = Variable x
                               in ( Goal { boundSkolems = newSkolems
                                         , goal = conclusion >>= subst
                                         , assumptions = renameRules gamma (map (`substRule` subst) premises)
                                         })
  where forEachName banned v = let n = freshenName (banned ++ names gamma) v 
                                in (n:banned ,  Variable (Skolem n))

renameRules :: KnownVars -> [LocalRule] -> [LocalRule]
renameRules gamma x = snd $ mapAccumR renameRules' (knownRules gamma) x
  where renameRules' banned (Rule {..}) = let n = freshenName banned name 
                                           in (n:banned, Rule {name = n,..})


applicate :: KnownVars -> Rule SkolemsAndSchematics -> ([Variable],ApplicatedRule)
applicate gamma (Rule {..}) = let (newSchematics, newNames) = mapAccumR forEachName [] binders
                                  subst (Left x)  = newNames `at` x
                                  subst (Right x) = Variable x
                               in (newSchematics, AR { aConclusion = conclusion >>= subst
                                                     , aPremises   = map (`substRule` subst) premises
                                                     , aName       = name
                                                     })
  where forEachName banned v = let n = freshenName (banned ++ names gamma) v 
                                in (n:banned , Variable (Schematic n (knownSkolems gamma)))


data Toplevel = Toplevel RuleName [Rule Variable] (Term Variable)

generalise :: Toplevel -> Axiom 
generalise (Toplevel n ps c) 
  | ExI (Flip ss) <- fromList $ concatMap (allInRule Just) ps ++ toList c
  , sigma <- Variable . Left . fromJust . flip findIx ss
  =  Rule n ss (map (`substRule` sigma) ps) (c >>= sigma)

makeObligation :: [Variable] -> Toplevel -> (RuleName,Goal)
makeObligation schematics (Toplevel n ps c) = (n, Goal skolems (map (`substRule` sigma) ps) (c >>= sigma))
  where sigma x | x `elem` skolems = Variable (Skolem x)
                | otherwise        = Variable (Schematic x [])
        skolems = filter (`notElem` schematics) $ toList c ++ concatMap (allInRule Just) ps


names :: KnownVars -> [Variable]
names (KnownVars a b _) = a ++ b 


freshenName :: [Variable] -> Variable -> Variable
freshenName banned v = head $ filter (not . null) $ filter (`notElem` banned) $ v : map (v ++) subscripts
  where subscripts = map show $ iterate (succ :: Integer -> Integer) 1


runRule :: KnownVars -> Rule SkolemsAndSchematics -> GoalTerm -> Maybe (RuleName, Unifier, [Goal], KnownVars)
runRule gamma rule t = let (newSchematics, AR name ass conc) = applicate gamma rule 
                           gamma' = gamma { knownSchematics = knownSchematics gamma ++ newSchematics }
                        in flip fmap (t `mgu` conc) $ \u -> 
                             ( name
                             , u
                             , map (skolemise gamma' . (`substRule` u)) ass
                             , gamma' { knownSchematics = knownSchematics gamma' `afterSubst` u }
                             )

afterSubst :: [Variable] -> Unifier -> [Variable]
afterSubst (v:vs) u | u (Schematic v []) == Variable (Schematic v []) = v:afterSubst vs u
                    | otherwise = afterSubst vs u
afterSubst [] _ = []

assume :: RuleName -> Goal -> Maybe Axiom
assume n (Goal _ as c) = do
  guard $ null $ concatMap (allInRule asSchematic) as
  guard $ null $ allSchematics c 
  let t = Toplevel n (map (`substRule` (\(Skolem k) -> Variable k)) as) (c >>= \(Skolem k) -> Variable k)
  return $ generalise t 

  
schematicsInGoal :: Goal -> [Variable]
schematicsInGoal (Goal vs ps c) = allSchematics c ++ concatMap (allInRule asSchematic) ps

localise :: Axiom -> LocalRule
localise = (`substRule` undefined) -- not a placeholder undefined
