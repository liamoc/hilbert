{-# LANGUAGE DoAndIfThenElse, RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TupleSections   #-}
module Main where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Default
import           Data.Maybe
import           Display
import           Graphics.Vty hiding ((<|>))
import qualified MVC
import qualified MVC.Prelude as MVC
import           Parser
import           Pipes hiding (next)
import qualified Pipes.Prelude as Pipes
import           System.Environment
import           UIModel
import           View

defaultKeyBindings :: [(Event, Model -> Model)]
defaultKeyBindings = [ (EvKey (KChar 'w') [], forward)
                     , (EvKey (KChar 'd') [], prev)
                     , (EvKey (KChar 's') [], back)
                     , (EvKey (KChar 'a') [], next)
                     , (EvKey (KChar 'z') [], clearSubtree)
                     , (EvKey (KChar 'r') [], rulemode)
                     , (EvKey (KChar 'q') [], nextVariant)
                     , (EvKey (KChar 'e') [], prevVariant)
                     , (EvKey (KChar '<') [], nextLemma)
                     , (EvKey (KChar '>') [], prevLemma)
                     , (EvKey (KChar '+') [], decreasePane)
                     , (EvKey (KChar '-') [], increasePane)
                     ]


mvcModel :: MVC.Model Model Event ScreenView 
mvcModel = MVC.asPipe $ Pipes.takeWhile (/= (EvKey (KChar 'x') [])) >-> MVC.loop mvcModel'
  where mvcModel' :: Event -> Pipes.ListT (State Model) ScreenView
        mvcModel' e = do Just a <- return $ lookup e defaultKeyBindings 
                         lift (modify a >> viewModel <$> get)
                  <|> do EvResize {} <- return e
                         lift (viewModel <$> get)

mvcViewController :: DisplaySkin -> MVC.Managed (MVC.View ScreenView, MVC.Controller Event)
mvcViewController sk = join $ MVC.managed $ \k -> do
     Vty {..} <- mkVty def
     let mvcView :: MVC.View ScreenView
         mvcView = MVC.asSink $ \v -> do
           displayBounds outputIface >>= update . picForImage . displayScreenView sk v 
     x <- k $ do
         mvcController <- do MVC.producer MVC.Single ( do (x,y) <- Pipes.lift (displayBounds outputIface)
                                                          Pipes.yield (EvResize x y)
                                                          MVC.lift nextEvent >~ MVC.cat)
         return (mvcView, mvcController)
     shutdown
     return x

mvcApp :: DisplaySkin -> Model -> IO Model
mvcApp sk m = MVC.runMVC m mvcModel (mvcViewController sk)

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
                 , horizontalLine = '─'
                 , rulesPanelVertPadding = 1
                 , rulesPanelCenterRules = False
                 , vinculumAttr   = defSentenceAttr
                 , vinculumChar   = \x -> if x then ' ' else '─'
                 , vinculumCenterChar   = \x -> if x then '⋮' else '─'
                 , displayTopBar = not
                 , showSchematicDependencies = True
                 , premiseLeftAnnot = onlyInSelL' ( defAttr {attrForeColor=SetTo brightYellow},"←")
                 , premiseRightAnnot = onlyInSelR' ( defAttr {attrForeColor=SetTo brightYellow}, "→")
                 , vincLeftAnnot = onlyInSelL ( defAttr {attrForeColor = SetTo brightGreen}, "←")
                 , vincRightAnnot = onlyInSelR ( defAttr {attrForeColor = SetTo brightGreen}, "→")
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


main :: IO ()
main = do
  getArgs >>= \case
    [] -> putStrLn "No rules files given"
    x:_ -> parse x >>= \case 
      Nothing -> return ()
      Just rules' -> void $ mvcApp defaultSkin $ fromJust $ newModel rules'
      

