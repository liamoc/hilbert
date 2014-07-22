{-# LANGUAGE DataKinds, DeriveFoldable, DeriveFunctor, ExistentialQuantification, GADTs, PolyKinds, StandaloneDeriving, TypeFamilies, TypeOperators, ViewPatterns, FlexibleContexts, PatternGuards #-}

module Vec where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Prelude hiding (zip, unzip, length, zipWith, take, splitAt)

newtype Flip f (a :: a') (b :: b') = Flip { unflip :: f b a }
newtype Flip2 f (a :: a') (b :: b') (c :: c') = Flip2 { unflip2 :: f c b a }

ffmap :: (Functor (Flip f c)) => (a -> b) -> f a c -> f b c
ffmap f = unflip . fmap f . Flip
fffmap :: (Functor (Flip2 f a b)) => (c -> c') -> f c b a -> f c' b a
fffmap f = unflip2 . fmap f . Flip2


data Nat = Zero | Suc Nat

data Exists :: (k -> *) -> * where
  ExI :: l v -> Exists l

type family (:+:) (a :: Nat) (b :: Nat) :: Nat
type instance x :+: Zero = x
type instance x :+: (Suc n) = Suc (x :+: n)

data (:=:) :: k -> k -> * where 
  Refl :: a :=: a  

zeroPlusNEqualsN :: SNat n -> (Zero :+: n) :=: n
zeroPlusNEqualsN SZero = Refl
zeroPlusNEqualsN (SSuc n) | Refl <- zeroPlusNEqualsN n = Refl

addSucLeft :: SNat v -> SNat n -> (Suc (v :+: n)) :=: (Suc v :+: n)
addSucLeft v SZero = Refl
addSucLeft v (SSuc n) | Refl <- addSucLeft v n = Refl

sucZeroIsSuc :: SNat n -> (Suc Zero :+: n) :=: (Suc n)
sucZeroIsSuc n | Refl <- sym (addSucLeft SZero n), Refl <- zeroPlusNEqualsN n = Refl

sym :: a :=: b -> b :=: a
sym Refl = Refl


(=?)  :: SNat a -> SNat b -> Maybe (a :=: b)
SZero =? SZero  = Just Refl
SSuc n =? SSuc m | Just Refl <- n =? m = Just Refl
_ =? _ = Nothing

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSuc :: SNat n -> SNat (Suc n)

deriving instance Show (SNat n)

data Fin :: Nat -> * where
  FZero :: Fin (Suc n)
  FSuc :: Fin n -> Fin (Suc n)

deriving instance Eq (Fin n)
deriving instance Show (Fin n)

data Vec :: Nat -> * -> * where
  Nil :: Vec Zero a
  Cons :: a -> Vec n a -> Vec (Suc n) a

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)

instance Functor (Vec n) where
  fmap f Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Foldable (Vec n) where
  foldMap f Nil = mempty
  foldMap f (Cons x y) = f x <> foldMap f y

instance Traversable (Vec n) where
  traverse f Nil = pure Nil
  traverse f (Cons x y) = Cons <$> f x <*> traverse f y

(<++>) :: Vec a t -> Vec b t -> Vec (a :+: b) t
v <++> Nil = v
v <++> Cons x xs = Cons x (v <++> xs)

sadd :: SNat v -> SNat n -> SNat (v :+: n)
sadd m SZero = m
sadd m (SSuc n) = SSuc (m `sadd` n)

length :: Vec a t -> SNat a
length Nil = SZero
length (Cons x xs) = SSuc (length xs)

fromList :: [a] -> Exists (Flip Vec a)
fromList [] = ExI $ Flip $ Nil
fromList (x:xs) | ExI (Flip xs') <- fromList xs = ExI $ Flip $ Cons x xs'

takeFromList :: SNat n -> [a] -> Maybe (Vec n a)
takeFromList SZero _ = Just Nil
takeFromList (SSuc n) (x:xs) = Cons x <$> takeFromList n xs 
takeFromList (SSuc n) []     = Nothing

cvtFromList :: SNat n -> [a] -> Maybe (Vec n a)
cvtFromList SZero [] = Just Nil
cvtFromList SZero _  = Nothing
cvtFromList (SSuc n) [] = Nothing
cvtFromList (SSuc n) (x:xs) = Cons x <$> cvtFromList n xs

cvtToList :: Vec n a -> [a]
cvtToList Nil = []
cvtToList (Cons a v) = a:cvtToList v

head :: Vec (Suc a) t -> t
head (Cons x xs) = x

tail :: Vec (Suc a) t -> Vec a t
tail (Cons x xs) = xs

splitAt :: SNat n -> Vec (v :+: n) a -> (Vec n a, Vec v a)
splitAt SZero x = (Nil, x)
splitAt (SSuc n) (Cons x xs) = let (a, b) = splitAt n xs in (Cons x a, b)

at :: Vec a t -> Fin a -> t
at (Cons x xs) FZero    = x
at (Cons x xs) (FSuc s) = at xs s
at _ _ = error "Vec.at: Haskell doesn't get exhaustivity"

update :: Vec a t -> Fin a -> t -> Vec a t   
update (Cons _ xs) FZero    x' = Cons x' xs
update (Cons x xs) (FSuc s) x' = Cons x (update xs s x')
update _ _ _ = error "Vec.update: exhaustive"

modifyAt :: Fin a -> (t -> t) -> Vec a t -> Vec a t
modifyAt l f v = update v l (f (v `at` l))

findIx :: (Eq t) => t -> Vec a t -> Maybe (Fin a)
findIx x ls = fmap snd . find (\(y,_) -> x == y) . zip ls $ allFins $ length ls

allFins :: SNat n -> Vec n (Fin n)
allFins SZero = Nil
allFins (SSuc n) = FZero `Cons` (FSuc <$> allFins n)

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f Nil Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)
zipWith _ _ _ = error "Vec.zipWith: Haskell doesn't understand this is exhaustive"

zip :: Vec n a -> Vec n b -> Vec n (a,b)
zip = zipWith (,)

unzip :: Vec n (a,b) -> (Vec n a , Vec n b)
unzip Nil = (Nil, Nil)
unzip (Cons (x,y) (unzip -> (xs, ys))) = (Cons x xs, Cons y ys)

toInt :: SNat v -> Int
toInt SZero = 0
toInt (SSuc n) = 1 + toInt n

shiftIdx :: Fin n -> SNat v -> Fin (Suc n)
shiftIdx v SZero = FSuc v
shiftIdx FZero (SSuc v) = FZero
shiftIdx (FSuc n) (SSuc v) = FSuc $ shiftIdx n v
