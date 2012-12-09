{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Main where


import           Control.Applicative            hiding ((<|>))
import           Control.Arrow                  hiding (left, right)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad                  (msum)
import           Data.Char
import qualified Data.Foldable                  as F
import           Data.IORef
import           Data.List
import           Data.List.Split
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                      as T
import           Graphics.Vty
import           Graphics.Vty.Image
import           Graphics.Vty.Picture
import           Graphics.Vty.Widgets.Borders
import           Graphics.Vty.Widgets.Box
import           Graphics.Vty.Widgets.Centering
import           Graphics.Vty.Widgets.Core
import           Graphics.Vty.Widgets.Edit
import           Graphics.Vty.Widgets.EventLoop
import           Graphics.Vty.Widgets.Skins
import           Graphics.Vty.Widgets.Text
import           System.Environment
vincOut, leftArrow, rightArrow :: Char

vincOut = '─'
leftArrow = '←'
rightArrow = '→'

parse :: FilePath -> IO (Maybe [Rule])
parse path = do strs <-  map (second $ takeWhile (/= '#')) . zip [1..] . lines <$> readFile path
                x <- mapM parseRule $ filter (not . null) $ splitWhen (all isSpace . snd) strs
                return $ if not (any (any isNothing) x) then Just $ concatMap catMaybes x
                                                        else Nothing
parseRule :: [(Int, String)] -> IO [Maybe Rule]
parseRule stuff = case split (keepDelimsL $ whenElt (("--" `isPrefixOf`) . dropWhile isSpace . snd)) stuff
                   of [topLine,[vinc,bottomLine]] -> let tops = filter (any (not . isSpace)) $ concatMap (splitOn "  ") $ map snd topLine
                                                         bot = unwords . words $ snd bottomLine
                                                         nam = dropWhile (== '-') $ dropWhile isSpace $ snd vinc
                                                       in if nam == "" then do putStrLn $ "No name given after vinculum on line " ++ show (fst vinc)
                                                                               return [Nothing]
                                                                       else return [Just $ Rule (unwords $ words nam) (map (unwords . words) tops) bot]
                      [one] -> do putStrLn $ "No vinculum found for rule on line " ++ show (fst $ head one)
                                  return [Nothing]
                      [one,_] -> do putStrLn $ "Multiple lines found in conclusion of rule on line " ++ show (fst $ head one)
                                    return [Nothing]
                      [] -> do putStrLn "Unexpected error"
                               return [Nothing]
                      one:_:_ -> do putStrLn $ "Multiple vinculi found for rule on line " ++ show (fst $ head one)
                                    return [Nothing]

data Rule = Rule { name       :: String
                 , premises   :: [String]
                 , conclusion :: String
                 } deriving Show

data StateMachine = Running String (M.Map Char String) deriving Show

globToNFA :: String -> StateMachine
globToNFA = flip Running M.empty

delta :: Char -> StateMachine -> [Maybe StateMachine]
delta c (Running s@('?':n:r) m)
   = let newMap1 = (M.insertWith (flip (++)) n [c] m )
         newMap2 = M.alter (Just . fromMaybe "") n m
      in (Just $ Running s newMap1) : delta c (Running (substitute' n (newMap2 M.! n) r) newMap2)
    where substitute' k v ('?':n':ns) | k == n' = v ++ substitute' k v ns
                                     | k /= n' = '?':n':substitute' k v ns
          substitute' k v (n':ns) = n':substitute' k v ns
          substitute' _ _ []      = []
delta c (Running (x:xs) m) | x /= c = [Nothing]
                           | x == c = [Just $ Running xs m]
delta _ (Running _ _) = [Nothing]

runString :: String -> StateMachine -> [M.Map Char String]
runString [] (Running l m) | allGlobs l = [blankGlobs m l]
  where allGlobs ('?':_:r) = allGlobs r
        allGlobs []        = True
        allGlobs  _        = False
        blankGlobs acc ('?':x:r) = blankGlobs (M.alter (Just . fromMaybe "") x acc) r
        blankGlobs acc _         = acc
runString [] _               = []
runString (x:xs) m = concatMap (runString xs) (catMaybes $ delta x m)

runRule :: Rule -> String -> [[String]]
runRule (Rule {..}) str = case runString (unwords $ words str) (globToNFA conclusion)
                            of [] -> []
                               list -> map (\m -> map (substitute m) premises) list
substitute :: M.Map Char String -> String -> String
substitute m ('?':n:r) = case M.lookup n m
                                  of Just x  -> x ++ substitute m r
                                     Nothing -> '?':n:substitute m r
substitute m (x:xs)    = x:substitute m xs
substitute _ []        = []

data DrawMode = Selected
              | Shaky Bool Bool Bool Bool
              | Normal deriving (Show, Eq)

data ProofTree = PT String String DrawMode (M.Map Char String) [ProofTree]
               deriving (Show, Eq)

data ProofTreeZipper = PTZ [ProofTreeContext] ProofTree
                     | Tentative [([ProofTreeZipper],[ProofTreeZipper])]
                                 [([ProofTreeZipper],[ProofTreeZipper])]
                                 ProofTreeZipper
                     deriving (Show, Eq)

data ProofTreeContext = PTC String String (M.Map Char String) [ProofTree] [ProofTree] deriving (Show, Eq)


dropImageRows :: Int -> Image -> Image
dropImageRows 0 i = i
dropImageRows v (HorizText {}) = EmptyImage
dropImageRows v (HorizJoin l r _ _) = dropImageRows v l <|> dropImageRows v r
dropImageRows v (VertJoin t b _ _) | v >= fromEnum (image_height t) = dropImageRows (v - fromEnum (image_height t)) b
                                   | otherwise = dropImageRows v t <-> b
dropImageRows v (BGFill w h) | h > fromIntegral v = BGFill w (h - fromIntegral v)
                             | otherwise = EmptyImage
dropImageRows v EmptyImage = EmptyImage
dropImageRows v (Translation c i) = Translation c $ dropImageRows v i
dropImageRows v (ImageCrop c i) = ImageCrop c $ dropImageRows v i
dropImageRows v (ImagePad c i) = ImagePad c $ dropImageRows v i
dropImageCols :: Int -> Image -> Image
dropImageCols 0 i = i
dropImageCols v (HorizText attr txt owidth twidth) = string attr (drop v $ map fst $ F.toList txt)
dropImageCols v (VertJoin t b _ _) = dropImageCols v t <-> dropImageCols v b
dropImageCols v (HorizJoin l r _ _) | v >= fromEnum (image_width l) = dropImageCols (v - fromEnum (image_width l)) r
                                    | otherwise                     = dropImageCols v l <|> r
dropImageCols v (BGFill w h) | w > fromIntegral v = BGFill (w - fromIntegral v) h
                             | otherwise = EmptyImage
dropImageCols v (EmptyImage) = EmptyImage
dropImageCols v (Translation c i) = Translation c $ dropImageCols v i
dropImageCols v (ImageCrop c i) = ImageCrop c $ dropImageCols v i
dropImageCols v (ImagePad c i) = ImagePad c $ dropImageCols v i


getRow :: Int -> Image -> [Image]
getRow 0 x@(HorizText {}) = [x]
getRow _ x@(HorizText {}) = []
getRow v (HorizJoin l r _ _) = getRow v l ++ getRow v r
getRow v (VertJoin t b _ _) | v >= fromEnum (image_height t) = getRow (v - fromEnum (image_height t)) b
                            | otherwise = getRow v t
getRow v (BGFill w h) | h > fromIntegral v = [BGFill w 1]
                      | otherwise = []
getRow v EmptyImage = []
getRow v (Translation c l) = getRow v l
getRow v (ImageCrop c l) = getRow v l
getRow v (ImagePad c l) = getRow v l

getColOfSelected :: Int -> Image -> Maybe Int
getColOfSelected r i = getCol' 0 (getRow r i)
  where getCol' :: Int -> [Image] -> Maybe Int
        getCol' (!acc) [] = Nothing
        getCol' (!acc) (HorizText attr text outputWidth charWidth :xs)
                 | attr `elem` [Attr Default (SetTo bright_yellow) Default, Attr Default (SetTo bright_green) Default]
                 , all (== vincOut) (map fst (F.toList text)) = Just $  acc + (fromIntegral $ charWidth `div` 2)
                 | otherwise = getCol' (acc + fromIntegral charWidth) xs
        getCol' (!acc) (BGFill w 1:xs) = getCol' (acc + fromIntegral w) xs

printTree :: Bool -> M.Map Char String -> ProofTree -> (Image, Bool)
printTree tentative subst  (PT statement tag isCurrent subst' children)
   = let statementI = renderStatement (attrFor isCurrent) subst statement
         vinculumW :: Int
         vinculumW  = fromIntegral $ max (max (image_width statementI - 2) 4)
                                              (image_width topI - 2)
         (rec, wfs) = unzip $ map (printTree (isShaky isCurrent || tentative) subst') children
         isWf = and wfs && tag /= ""
         vinculumI  =  decorationL' isCurrent
                   <|> string def_attr " "
                   <|> string (attrFor'' isCurrent) (replicate vinculumW vincOut)
                   <|> string def_attr " "
                   <|> string (attrFor' isCurrent isWf) tag
                   <|> string def_attr " "
                   <|> decorationR' isCurrent
         topI       = horiz_cat_bot ((decorationL isCurrent : rec) ++ [decorationR isCurrent] )
      in (vert_cat_mid [topI,vinculumI,statementI], isWf)
     where isShaky (Shaky {}) = True
           isShaky _          = False
           attrFor x | tentative = Attr (SetTo dim) (SetTo bright_black) Default
           attrFor Selected = Attr Default (SetTo bright_yellow) Default
           attrFor Normal = Attr Default Default Default
           attrFor (Shaky {}) = Attr Default (SetTo bright_green) Default
           attrFor' _ _ | tentative = Attr (SetTo dim) (SetTo bright_black) Default
           attrFor' Selected _ = Attr (SetTo reverse_video) (SetTo bright_yellow) Default
           attrFor' Normal True = Attr Default (SetTo blue) Default
           attrFor' Normal False = Attr Default (SetTo red) Default
           attrFor' (Shaky {}) _ = Attr (SetTo reverse_video) (SetTo bright_green) Default
           attrFor'' x | tentative = Attr (SetTo dim) (SetTo bright_black) Default
           attrFor'' Selected = Attr Default (SetTo bright_yellow) Default
           attrFor'' Normal = Attr Default (SetTo white) Default
           attrFor'' (Shaky {}) = Attr Default (SetTo bright_green) Default
           decorationL (Shaky True _ _ _) = string (attrFor Selected) [leftArrow]
           decorationL x = string (attrFor x) " "
           decorationR (Shaky _ True _ _) = string (attrFor Selected) [rightArrow]
           decorationR x = string (attrFor x) " "
           decorationL' x@(Shaky _ _ True _) = string (attrFor x) [leftArrow]
           decorationL' x = string (attrFor x) " "
           decorationR' x@(Shaky _ _ _ True) = string (attrFor x) [rightArrow]
           decorationR' x = string (attrFor x) " "
           renderStatement attr s ('?':n:r) | Just x <- M.lookup n s = string (def_attr <> attr <> attr {attr_style = SetTo bold }) x
                                                                   <|> renderStatement attr s r
           renderStatement attr s ('?':n:r) = char (attr {attr_style = SetTo bold, attr_fore_color = SetTo red }) n
                                                                   <|> renderStatement attr s r
           renderStatement attr s (c:r) = char (def_attr <> attr) c
                                       <|> renderStatement attr s r
           renderStatement _  _ [] = char def_attr ' '
           horiz_cat_bot = horiz_cat . uniform
           uniform ls = let m = maximum $ map image_height ls
                         in intersperse (background_fill 2 m) $ map (\i -> background_fill (image_width i) (m - image_height i) <-> i) ls
           vert_cat_mid = vert_cat . uniform'
           uniform' ls = let m = maximum $ map image_width ls
                          in map (\i -> let p1 = (m - image_width i) `div` 2
                                            p2 = (m - image_width i) - p1
                                         in horiz_cat [background_fill p1 (image_height i), i, background_fill p2 (image_height i)]) ls

main :: IO ()
main = do x <- getArgs
          rules' <- mapM parse x
          if any isNothing rules' then do
            putStrLn "Errors found in rules definition."
          else if null rules' then do
            putStrLn "No rules files given"
          else do
            let Just rules = mconcat rules'
            c <- newCollection
            f1 <- newFocusGroup
            mv <- newEmptyMVar

            e <- editWidget
            txt <- plainText (T.pack "Enter proof goal:")
            let proc prompt = schedule (do
                  focus e
                  setText txt (T.pack prompt)
                  setEditText e (T.pack ""))
                        >> return mv
            t <- mkProofTreeWidget proc rules
            b1 <- vBorder
            b2 <- vBorder
            _ <- addToFocusGroup f1 e
            _ <- addToFocusGroup f1 t
            onActivate e $ \e' -> (T.unpack <$> getEditText e') >>= putMVar mv
                               >> setEditText e' (T.pack "")
                               >> setText txt (T.pack "Hilbert 1.0")
                               >> focus t
            w' <- vBox txt e
            w  <- flip vBox w' =<< hBox b1 =<< hBox t b2
            setNormalAttribute w' (  Attr (SetTo bold) (SetTo bright_white) (SetTo bright_black))
            setNormalAttribute e ( Attr (SetTo default_style_mask) (SetTo bright_white) (SetTo black))
            setFocusAttribute e ( Attr (SetTo default_style_mask) (SetTo bright_white) (SetTo black))
            setBoxChildSizePolicy w $ PerChild BoxAuto $ BoxFixed 2
            _ <- addToCollection c w f1
            _ <- forkIO $ do goal <- takeMVar mv
                             schedule $ updateWidgetState t $ const $ lemma goal
            runUi c $ defaultContext {skin = (skin defaultContext) { skinVertical = ' ' }}



mkProofTreeWidget :: (String -> IO (MVar String)) -> [Rule] -> IO (Widget ProofTreeZipper)
mkProofTreeWidget proc rules = newWidget (lemma "") $ \w ->
      w { render_ = \ref h _ -> toImage ref h
        , keyEventHandler = \ref k _ -> handleInput ref k
        , growHorizontal_ = const $ return True
        , growVertical_ = const $ return True
        }
  where handleInput _   (KASCII ' ') = shutdownUi >> return True
        handleInput ref (KASCII 's') = updateWidgetState ref up    >> return True
        handleInput ref (KASCII 'a') = updateWidgetState ref left  >> return True
        handleInput ref (KASCII 'w') = updateWidgetState ref down  >> return True
        handleInput ref (KASCII 'd') = updateWidgetState ref right >> return True
        handleInput ref (KASCII 'o') = updateWidgetState ref oops  >> return True
        handleInput ref (KASCII 'r') = updateWidgetState ref (rulemode rules) >> return True
        handleInput ref (KASCII 'e') = updateWidgetState ref cycleLeft >> return True
        handleInput ref (KASCII 'q') = updateWidgetState ref cycleRight >> return True
        handleInput ref (KASCII 'f') = getAllVars >>= handleVarSubst >> return True
         where
          handleVarSubst (x:xs) = do mv <- proc $ "Enter assignment for: " ++ [x]
                                     _ <- forkIO $ do v <- takeMVar mv
                                                      schedule $ updateWidgetState ref (addSubst x v)
                                                      handleVarSubst xs
                                     return ()
          handleVarSubst [] = return ()
          getAllVars = do st <- getState ref
                          let vs = availableVariables st
                              st' = up st
                              vs' = availableVariables st'
                           in if null vs && not (null vs') then do
                                 updateWidgetState ref up
                                 getAllVars
                              else return vs

        handleInput _ _ = return False
        toImage ref h = do
             (tree, height) <- treeForViewing <$> state <$> readIORef ref
             let img =  fst $ printTree False M.empty tree
                 size_diff :: Int
                 size_diff = fromIntegral (region_height h) `div` 2 - fromIntegral (image_height img) + height * 2
                 shiftUpDown | size_diff > 0 = (background_fill (image_width img) (fromIntegral $ size_diff)  <->)
                             | size_diff == 0 = id
                             | size_diff < 0 = dropImageRows (negate size_diff)
                 focus_left :: Maybe Int
                 focus_left = msum $ map (flip getColOfSelected img) [0..fromIntegral (image_height img)] --  getColOfSelected (height * 2 - 1) img

                 left_delta :: Int
                 left_delta =  (fromIntegral (region_width h) `div` 2) - (fromMaybe (fromIntegral $ image_width img `div` 2) focus_left)
                 shiftLeftRight | left_delta < 0 = dropImageCols (negate $ fromIntegral $ left_delta)
                                | left_delta == 0 = id
                                | left_delta > 0 = (background_fill (fromIntegral $ left_delta) (image_height img) <|>)
             return $ crop (region_width h, region_height h) $ pad (region_width h, region_height h) $ shiftUpDown $ shiftLeftRight img



rewind :: ProofTreeZipper -> (ProofTree, Int)
rewind z =
          let (tk,drp) = span (\(PTZ ctx _) -> not $ null ctx) . iterate up $ z
           in ((\(PTZ _ p) -> p) . head $ drp, length tk )

treeForViewing :: ProofTreeZipper -> (ProofTree, Int)
treeForViewing (PTZ ctx' (PT str nam _ sbst cs))  = rewind (PTZ ctx' (PT str nam Selected sbst cs))
treeForViewing (Tentative as ((ls, PTZ ctx' (PT star nam _ subst cs) :rs):bs) _) = rewind (PTZ ctx' (PT star nam (Shaky (not $ null ls) (not $ null rs) (not $ null as) (not $ null bs)) subst cs))
treeForViewing x = error $ show x

lemma :: String -> ProofTreeZipper
lemma str = PTZ [] (PT str "" Normal M.empty [])

availableVariables :: ProofTreeZipper -> String
availableVariables (PTZ _ (PT _ _ _ _ cs)) = nub $ concatMap availableVariables'' cs
  where availableVariables' ('?':x:xs) = x : availableVariables' xs
        availableVariables' (_:xs)     = availableVariables' xs
        availableVariables' []         = []
        availableVariables'' (PT str _ _ _ _) = availableVariables' str
availableVariables _ = []

rule :: Rule -> ProofTreeZipper -> [ProofTreeZipper]
rule r (PTZ ctx (PT str _ _ _ _)) = map (PTZ ctx . smartPT str (name r) M.empty . map (\x -> PT x "" Normal M.empty [])) $ runRule r (substitute (getSubst ctx) str)
   where smartPT s name = PT s name Normal
rule _ _ = []
getSubst :: [ProofTreeContext] -> M.Map Char String
getSubst [] = M.empty
getSubst (PTC _ _ s _ _:_) = s
up :: ProofTreeZipper -> ProofTreeZipper
up (PTZ (PTC str name subst l r :cs) pt) = PTZ cs (PT str name Normal subst (l ++ (pt:r)))
up (Tentative _ _ pto) = pto
up x = x

down :: ProofTreeZipper -> ProofTreeZipper
down (PTZ cs (PT str name _ subst (x:xs))) = PTZ (PTC str name subst [] xs:cs) x
down (Tentative _ ((_,x:_):_) _) = x
down x = x

left :: ProofTreeZipper -> ProofTreeZipper
left (PTZ (PTC str name subst (l:ls) rs : cs) pt) = PTZ (PTC str name subst ls (pt:rs) : cs) l
left (Tentative (l:ls) rs orig) = Tentative ls (l:rs) orig
left x = x

addSubst :: Char -> String -> ProofTreeZipper -> ProofTreeZipper
addSubst k s (PTZ ctx (PT str name v subst cs)) = PTZ ctx (PT str name v (M.insert k s subst) cs)
addSubst _ _ x = x

right :: ProofTreeZipper -> ProofTreeZipper
right (PTZ (PTC str name subst ls (r:rs) : cs) pt) = PTZ (PTC str name subst (pt:ls) rs : cs) r
right (Tentative ls (r:rs) orig) | not (null rs) = Tentative (r:ls) rs orig
right x = x

oops :: ProofTreeZipper -> ProofTreeZipper
oops (PTZ ctx (PT str _ _ _ _)) = PTZ ctx (PT str "" Normal M.empty [])
oops (Tentative _ _ orig) = orig

rulemode :: [Rule] -> ProofTreeZipper -> ProofTreeZipper
rulemode rules z = if nonSchema z then let it = filter (/= ([],[])) $ map (([],) . flip rule z) rules
                                        in if null it then z else Tentative [] it z
                                  else z
   where nonSchema :: ProofTreeZipper -> Bool
         nonSchema (PTZ ctx (PT str _ _ _ _)) = '?' `notElem` substitute (getSubst ctx) str
         nonSchema _ = False

cycleLeft :: ProofTreeZipper -> ProofTreeZipper
cycleLeft (Tentative ls ((ts,b:bs):rs) o) | not (null bs) = Tentative ls ((b:ts,bs):rs) o
cycleLeft x = x


cycleRight :: ProofTreeZipper -> ProofTreeZipper
cycleRight (Tentative ls ((t:ts,bs):rs) o) = Tentative ls ((ts,t:bs):rs) o
cycleRight x = x

{-

abc(?x,?y)  dga(?x,?y)
------------------- MAIN
def -}
