{-# LANGUAGE DoAndIfThenElse, RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
module Main where


import           Control.Applicative            hiding ((<|>))
import           Control.Concurrent
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                      as T
import           Graphics.Vty
import           Graphics.Vty.Image
import           System.Environment
import Display
import Parser
import TreeWidget
import UIModel
import View
import Data.Default

vincOut :: Char
vincOut = '─'
leftArrow, rightArrow :: String
leftArrow = "←"
rightArrow = "→"

defaultSkin :: DisplaySkin
defaultSkin = DS { titleAttr = titleFunc
                 , sentenceAttr  = defSentenceAttr
                 , variableAttr = const $ Attr (SetTo bold) (SetTo brightGreen) Default
                 , skolemAttr = const $ Attr Default (SetTo brightMagenta) Default
                 , skolemIntroAttr = const $ Attr Default (SetTo brightMagenta) Default
                 , ruleVarAttr = const $ Attr Default (SetTo brightCyan) Default
                 , ruleIntroAttr = const $ Attr Default (SetTo brightBlue) Default
                 , separatePremises = emptyImage 
                 , vinculumPadding = 0
                 , verticalLine = '│'
                 , unprovenChar = '○'
                 , provenChar = '●'
                 , topCornerChar = '├'
                 , horizontalLine = vincOut
                 , rulesPanelVertPadding = 1
                 , rulesPanelCenterRules = False
                 , vinculumAttr   = defSentenceAttr
                 , vinculumChar   = \x -> if x then ' ' else vincOut
                 , vinculumCenterChar   = \x -> if x then '⋮' else vincOut
                 , displayTopBar = not
                 , showSchematicDependencies = True
                 , premiseLeftAnnot = onlyInSelL' ( defAttr {attrForeColor=SetTo brightYellow} 
                                                  , leftArrow)
                 , premiseRightAnnot = onlyInSelR' ( defAttr {attrForeColor=SetTo brightYellow} 
                                                   , rightArrow)
                 , vincLeftAnnot = onlyInSelL ( defAttr {attrForeColor = SetTo brightGreen} 
                                              , leftArrow)
                 , vincRightAnnot = onlyInSelR ( defAttr {attrForeColor = SetTo brightGreen} 
                                               , rightArrow)
                 , bgAttr =  defAttr
                 }
  where defSentenceAttr x= case x of 
                            Normal -> defAttr
                            Speculative -> Attr (SetTo bold) (SetTo brightBlack) Default
                            Selection   -> Attr Default (SetTo brightYellow) Default
                            Selecting {} -> Attr Default (SetTo brightGreen) Default
        onlyInSelL x (Selecting {isPrevRule = True}) = x
        onlyInSelL _ _ = (defAttr, "")
        onlyInSelR x (Selecting {isNextRule = True}) = x
        onlyInSelR _ _ = (defAttr, "")
        onlyInSelL' x (Selecting {isPrevAssign = True}) = x
        onlyInSelL' _ _ = (defAttr, "")
        onlyInSelR' x  (Selecting {isNextAssign = True}) = x
        onlyInSelR' _ _ = (defAttr, "")
        titleFunc _ (Selecting {}) = Attr (SetTo reverseVideo) (SetTo brightGreen) Default
        titleFunc _ (Selection   ) = Attr (SetTo reverseVideo) (SetTo brightYellow) Default
        titleFunc True _ = Attr (SetTo bold) (SetTo brightBlue) Default 
        titleFunc False _ = Attr (SetTo bold) (SetTo brightRed) Default  

defaultKeyBindings :: KeyBindings
defaultKeyBindings = [(KChar 'w', MoveForward)
                     ,(KChar 'd', MovePrev)
                     ,(KChar 's', MoveBack)
                     ,(KChar 'a', MoveNext)
                     ,(KChar 'z', ClearSubtree)
                     ,(KChar 'r', EnterRulesMode)
                     ,(KChar 'q', NextAssignment)
                     ,(KChar 'e', PrevAssignment)
                     ,(KChar '<', NextLemma)
                     ,(KChar '>', PrevLemma)
                     ,(KChar '+', DecreasePane)
                     ,(KChar '-', IncreasePane)
                     ,(KChar 'f', SubstituteVars)
                     ]


main :: IO ()
main = do
  x <- getArgs
  if null x then 
    putStrLn "No rules files given"
  else do
    rules' <- parse $ head x
    if isNothing rules' then
      putStrLn "Errors found in rules definition."
    else do
      let Just rules = rules'
          Just m =  newModel rules 
      Vty {..} <- mkVty def
      let redraw m' =  displayBounds outputIface >>= update . picForImage . toImage defaultSkin m'
          eventLoop m' = nextEvent >>= \case 
            EvKey (KChar 'x') _  -> shutdown
            EvKey (KChar k)   _  -> case lookup (KChar k) defaultKeyBindings of 
                                      Just f -> let m'' = keyActionToOp f m' in redraw m'' >> eventLoop m''
                                      Nothing -> eventLoop m'
            EvResize {} -> redraw m' >> eventLoop m'
            _ -> eventLoop m'
      redraw m
      eventLoop m
       
  

     -- let proc' text = setText txt (T.pack $ "Hilbert 2.0 │ " ++ text)
