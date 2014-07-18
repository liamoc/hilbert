{-# LANGUAGE DoAndIfThenElse #-}
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
import           Graphics.Vty.Widgets.Borders
import           Graphics.Vty.Widgets.Box
import           Graphics.Vty.Widgets.Core
import           Graphics.Vty.Widgets.Edit
import           Graphics.Vty.Widgets.EventLoop
import           Graphics.Vty.Widgets.Skins
import           Graphics.Vty.Widgets.Text
import           System.Environment
import Display
import Parser
import TreeWidget
import UIModel
import View

vincOut :: Char
vincOut = '─'
leftArrow, rightArrow :: String
leftArrow = "←"
rightArrow = "→"

defaultSkin :: DisplaySkin
defaultSkin = DS { titleAttr = titleFunc
                 , sentenceAttr  = defSentenceAttr
                 , variableAttr = const $ Attr (SetTo bold) (SetTo bright_green) Default
                 , skolemAttr = const $ Attr Default (SetTo bright_magenta) Default
                 , skolemIntroAttr = const $ Attr Default (SetTo bright_magenta) Default
                 , ruleIntroAttr = const $ Attr Default (SetTo bright_blue) Default
                 , separatePremises = EmptyImage 
                 , vinculumPadding = 0
                 , vinculumAttr   = defSentenceAttr
                 , vinculumChar   = vincOut
                 , showSchematicDependencies = True
                 , premiseLeftAnnot = onlyInSelL' ( def_attr {attr_fore_color=SetTo bright_yellow} 
                                                  , leftArrow)
                 , premiseRightAnnot = onlyInSelR' ( def_attr {attr_fore_color=SetTo bright_yellow} 
                                                   , rightArrow)
                 , vincLeftAnnot = onlyInSelL ( def_attr {attr_fore_color = SetTo bright_green} 
                                              , leftArrow)
                 , vincRightAnnot = onlyInSelR ( def_attr {attr_fore_color = SetTo bright_green} 
                                               , rightArrow)
                 , bgAttr =  def_attr
                 }
  where defSentenceAttr x= case x of 
                            Normal -> def_attr
                            Speculative -> Attr (SetTo bold) (SetTo bright_black) Default
                            Selection   -> Attr Default (SetTo bright_yellow) Default
                            Selecting {} -> Attr Default (SetTo bright_green) Default
        onlyInSelL x (Selecting {isPrevRule = True}) = x
        onlyInSelL _ _ = (def_attr, "")
        onlyInSelR x (Selecting {isNextRule = True}) = x
        onlyInSelR _ _ = (def_attr, "")
        onlyInSelL' x (Selecting {isPrevAssign = True}) = x
        onlyInSelL' _ _ = (def_attr, "")
        onlyInSelR' x  (Selecting {isNextAssign = True}) = x
        onlyInSelR' _ _ = (def_attr, "")
        titleFunc _ (Selecting {}) = Attr (SetTo reverse_video) (SetTo bright_green) Default
        titleFunc _ (Selection   ) = Attr (SetTo reverse_video) (SetTo bright_yellow) Default
        titleFunc True _ = Attr (SetTo bold) (SetTo bright_blue) Default 
        titleFunc False _ = Attr (SetTo bold) (SetTo bright_red) Default  

defaultKeyBindings :: KeyBindings
defaultKeyBindings = [(KASCII 'x', ArbitraryIO shutdownUi)
                     ,(KASCII 'w', MoveForward)
                     ,(KASCII 'd', MovePrev)
                     ,(KASCII 's', MoveBack)
                     ,(KASCII 'a', MoveNext)
                     ,(KASCII 'z', ClearSubtree)
                     ,(KASCII 'r', EnterRulesMode)
                     ,(KASCII 'q', NextAssignment)
                     ,(KASCII 'e', PrevAssignment)
                     ,(KASCII 'f', SubstituteVars)
                     ]


main :: IO ()
main = do
  x <- getArgs
  rules' <- mapM parse x
  if any isNothing rules' then
    putStrLn "Errors found in rules definition."
  else if null rules' then 
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
    t <- proofTreeWidget rules proc defaultSkin defaultKeyBindings
    b1 <- vBorder
    b2 <- vBorder
    _ <- addToFocusGroup f1 e
    _ <- addToFocusGroup f1 t
    onActivate e $ \e' -> (T.unpack <$> getEditText e') >>= putMVar mv
                              >> setEditText e' (T.pack "")
                              >> setText txt (T.pack "Hilbert 2.0")
                              >> focus t
    w' <- vBox txt e
    w  <- flip vBox w' =<< hBox b1 =<< hBox t b2
    setNormalAttribute w' (  def_attr `with_style` bold `with_style` reverse_video)
    setNormalAttribute e (  def_attr  `with_style` default_style_mask)
    setFocusAttribute e  def_attr
    setBoxChildSizePolicy w $ PerChild BoxAuto $ BoxFixed 2
    _ <- addToCollection c w f1
    _ <- forkIO $ do goal <- takeMVar mv
                     case parseTerm goal of 
                          Just goal -> schedule $ updateWidgetState t $ const $ newModel goal
                          Nothing   -> return ()
    runUi c $ defaultContext {skin = (skin defaultContext) { skinVertical = ' ' }
                             ,normalAttr = def_attr }



