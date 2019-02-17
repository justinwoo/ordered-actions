# Ordered-Actions demo

You can have guaranteed ordering of effects to be run using static row types. This is because row types themselves are unordered, but the RowList structure used to work with row type contents generically are ordered lexicographically.

Preparing effects to be run later is pure:

```purs
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
```

Then if the function to be run with the results of these actions is pure, then the parts users need to be exposed to can be pure.

```purs
runEffects :: forall actions actionsL results return
   . RL.RowToList actions actionsL
  => RunEffects actionsL actions () results
  => {|actions}
  -> ({|results} -> return)
  -> Effect return
runEffects actions fn = do
  -- ...
```

So we can see the results of using this:

```purs
main :: Effect Unit
main = do
  return <- runEffects myEffects identity
  log $ show return
```

```purs
> pulp run
* Building project in /home/justin/Code/ordered-actions
           Src   Lib   All
Warnings   0     0     0
Errors     0     0     0
* Build successful.
Running genericEffect for: apple
Running genericEffect for: banana
Running genericEffect for: kiwi
{ apple: "apple", banana: "banana", kiwi: "kiwi" }
```
