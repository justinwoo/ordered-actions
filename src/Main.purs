module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

-- | Class for running ordered actions from a record
class RunEffects (rl :: RL.RowList) (actions :: # Type) (from :: # Type) (to :: # Type)
  | rl -> actions from to where
  runEffectsImpl :: RLProxy rl -> {|actions} -> Effect (Builder {|from} {|to})

instance runEffectsNil :: RunEffects RL.Nil actions () () where
  runEffectsImpl _ _ = pure identity

instance runEffectsCons ::
  ( IsSymbol name
  , Row.Cons name (Effect a) actions' actions
  , Row.Cons name a from' to
  , Row.Lacks name from'
  , RunEffects tail actions from from'
  ) => RunEffects (RL.Cons name (Effect a) tail) actions from to where
  runEffectsImpl _ actions = do
    value <- action
    tail <- runEffectsImpl tailP actions
    let head = Builder.insert nameP value
    pure $ head <<< tail
    where
      nameP = SProxy :: _ name
      action = Record.get nameP actions
      tailP = RLProxy :: _ tail

-- | Run the actions from a record and feed it to a function of the results
runEffects :: forall actions actionsL results return
   . RL.RowToList actions actionsL
  => RunEffects actionsL actions () results
  => {|actions}
  -> ({|results} -> return)
  -> Effect return
runEffects actions fn = do
  builder <- runEffectsImpl actionsLP actions
  let results = Builder.build builder {}
  pure (fn results)
   where
     actionsLP = RLProxy :: _ actionsL

-- | Some random genericEffect that needs to be performed.
genericEffect :: String -> Effect String
genericEffect msg = do
  log $ "Running genericEffect for: " <> msg
  pure msg

-- | Effects we want to run
myEffects ::
  { apple :: Effect String
  , banana :: Effect String
  , kiwi :: Effect String
  }
myEffects =
  { apple: genericEffect "apple"
  , banana: genericEffect "banana"
  , kiwi: genericEffect "kiwi"
  }

main :: Effect Unit
main = do
  return <- runEffects myEffects identity
  log $ show return
