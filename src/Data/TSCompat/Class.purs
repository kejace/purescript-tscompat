module Data.TSCompat.Class where 

import Data.Nullable (Nullable)
import Data.TSCompat (Any, OneOf, OptionRecord, StringConst)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Prim.Row as Row
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Type.Data.Boolean

foreign import data OptionField :: Boolean -> Type -> Type

class TsTypeExists (t :: Type) (rl :: RL.RowList Type) (o :: Boolean) | t rl -> o
instance consCheck :: (IsEq t t2 eq, TsTypeExists t tail tailEq, Or eq tailEq out) 
    => TsTypeExists t (RL.Cons "typed" t2 tail) out
instance nilCheck :: TsTypeExists t RL.Nil False

class ConstainsAll (out :: RL.RowList Type) (b :: Row Type)
instance consAll :: (ConstainsAll tail b, Row.Cons s any btail b) => ConstainsAll (RL.Cons s t tail) b
instance nilAll :: ConstainsAll RL.Nil b 

{-- By using this class we can get pretty good error messages --}
class TSCompatible (s :: Symbol) (a :: Type) (b :: Type) (eq :: Boolean) | a -> b, b -> a
instance onlyTrue :: TSCompatible s a b True
instance sameType :: TSCompatible s a a False

class IsOptional (s :: Symbol) (m :: RL.RowList Type) (b :: Boolean) | s m -> b 
instance consOpt :: IsOptional s (RL.Cons s any tail) False
else instance consOpt2 :: IsOptional s tail b => IsOptional s (RL.Cons s2 any tail) b
instance nilOpt :: IsOptional s RL.Nil True

class IsEqRowList (l :: RL.RowList Type) (b :: Row Type) (m :: RL.RowList Type)
instance consOptEQ :: (Row.Cons s tb tailb b, IsOptional s m o, IsEq ta (OptionField o tb) eq, 
    TSCompatible s ta tb eq, IsEqRowList taila b m) => IsEqRowList (RL.Cons s ta taila) b m
instance nilRLEQ :: IsEqRowList RL.Nil b m

class IsEq (a :: Type) (b :: Type) (eq :: Boolean) | a b -> eq
instance reflTSEq :: IsEq a a True
else instance anyIsEq :: IsEq a Any True
else instance unionIsEq :: (RL.RowToList b rl, TsTypeExists a rl eq) 
    => IsEq a (OneOf b) eq
else instance intIsNumber :: IsEq Int Number True
else instance constString :: IsEq (StringConst s) String True
else instance effectAsFn1 :: IsEq (Effect a) (EffectFn1 e a) True
else instance effectAsFn2 :: IsEq (Effect a) (EffectFn2 e b a) True
else instance effectFn1asFn2 :: IsEq (EffectFn1 e a) (EffectFn2 e b a) True
else instance typeConsEq :: IsEq a b eq => IsEq (m a) (m b) eq
else instance optRecord :: (RL.RowToList a al, RL.RowToList mand ml, 
    IsEqRowList al all ml, ConstainsAll ml a) => IsEq (Record a) (OptionRecord all mand) True
else instance optionalNull :: IsEq a b eq => IsEq (Nullable a) (OptionField True b) eq
else instance optional :: IsEq a b eq => IsEq a (OptionField o b) eq
else instance notEq :: IsEq a b False
 
class IsTSEq (a :: Type) (b :: Type)
instance anyTSEq :: IsEq a b True => IsTSEq a b

asTS :: forall a b. IsTSEq a b => a -> b 
asTS = unsafeCoerce