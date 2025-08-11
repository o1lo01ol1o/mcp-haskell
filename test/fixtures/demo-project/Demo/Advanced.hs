{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module for testing advanced HLS features: GADTs, Template Haskell, advanced types
-- Tests: gadt_conversion, expand_th_splice, advanced hover info, complex completions
module Demo.Advanced where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Simple GADT for testing GADT conversion and advanced type inference
data SimpleGADT a where
  IntValue :: Int -> SimpleGADT Int
  StringValue :: String -> SimpleGADT String
  BoolValue :: Bool -> SimpleGADT Bool
  ListValue :: [a] -> SimpleGADT [a]

deriving instance Show a => Show (SimpleGADT a)

-- | Pattern matching on GADTs - tests advanced hover info
processSimpleGADT :: SimpleGADT a -> String
processSimpleGADT (IntValue i) = "Integer: " ++ show i
processSimpleGADT (StringValue s) = "String: " ++ s  
processSimpleGADT (BoolValue b) = "Boolean: " ++ show b
processSimpleGADT (ListValue xs) = "List with " ++ show (length xs) ++ " elements"

-- | More complex GADT with type-level programming
data TypedExpr a where
  LitInt :: Int -> TypedExpr Int
  LitBool :: Bool -> TypedExpr Bool
  Add :: TypedExpr Int -> TypedExpr Int -> TypedExpr Int
  Eq :: Eq a => TypedExpr a -> TypedExpr a -> TypedExpr Bool
  If :: TypedExpr Bool -> TypedExpr a -> TypedExpr a -> TypedExpr a

deriving instance Show a => Show (TypedExpr a)

-- | GADT evaluator - tests type-level computation understanding
evalTypedExpr :: TypedExpr a -> a
evalTypedExpr (LitInt i) = i
evalTypedExpr (LitBool b) = b  
evalTypedExpr (Add e1 e2) = evalTypedExpr e1 + evalTypedExpr e2
evalTypedExpr (Eq e1 e2) = evalTypedExpr e1 == evalTypedExpr e2
evalTypedExpr (If cond t f) = if evalTypedExpr cond then evalTypedExpr t else evalTypedExpr f

-- | Template Haskell function generator - tests expand_th_splice
generateGetter :: String -> Q [Dec]
generateGetter fieldName = do
  let funcName = mkName ("get" ++ fieldName)
  let fieldName' = mkName fieldName
  [d| $(return (VarP funcName)) r = $(return (VarE fieldName')) r |]

-- | Template Haskell splice that should be expandable
$(generateGetter "Name")

-- | Test record for TH-generated getters
data TestRecord = TestRecord 
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Show)

-- | QuasiQuoter test - complex TH feature
simpleQuoter :: QuasiQuoter
simpleQuoter = QuasiQuoter
  { quoteExp = \s -> [| "Quoted: " ++ $(litE (StringL s)) |]
  , quotePat = undefined
  , quoteType = undefined  
  , quoteDec = undefined
  }

-- | Usage of custom quasi-quoter - should support expand_th_splice
quotedString :: String
quotedString = [simpleQuoter|Hello World|]

-- | Type families for advanced type-level programming
type family ElementType (f :: * -> *) :: *
type instance ElementType [] = ()
type instance ElementType Maybe = ()

-- | Constraint kinds and advanced types
class Collection f where
  empty :: f a
  insert :: a -> f a -> f a
  member :: Eq a => a -> f a -> Bool

instance Collection [] where
  empty = []
  insert = (:)
  member = elem

-- | Function using type families and constraints - tests advanced hover
processCollection :: (Collection f, Eq a) => [a] -> f a -> f a
processCollection items container = foldr insert container items

-- | Data type promotion for type-level programming
data Nat = Zero | Succ Nat

-- | GADT with promoted types
data Vec (n :: Nat) a where
  VNil :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec 'Succ n a

deriving instance Show a => Show (Vec n a)

-- | Type-safe vector operations
vhead :: Vec ('Succ n) a -> a
vhead (VCons x _) = x

vtail :: Vec ('Succ n) a -> Vec n a
vtail (VCons _ xs) = xs

-- | Template Haskell for generating boilerplate
makeLenses :: Name -> Q [Dec]
makeLenses typeName = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify typeName
  concat <$> mapM makeLens fields
  where
    makeLens (fieldName, _, fieldType) = do
      let lensName = mkName (nameBase fieldName ++ "Lens")
      [d| $(return (VarP lensName)) f r = 
            fmap (\x -> r { $(return (VarE fieldName)) = x }) 
                 (f ($(return (VarE fieldName)) r)) |]

-- | More complex TH splice
$(makeLenses ''TestRecord)

-- | Phantom types for type-safety
data Tagged (tag :: *) a = Tagged a
  deriving (Show, Eq)

-- | Type-level tags
data Meters
data Feet  
data Seconds

-- | Type-safe operations with phantom types
distance :: Tagged Meters Double
distance = Tagged 100.0

time :: Tagged Seconds Double  
time = Tagged 5.0

-- | This should cause a type error if we try to add distance and time
-- speed = distance + time  -- Should not compile

-- | Safe operations on tagged types
addSameUnit :: Num a => Tagged tag a -> Tagged tag a -> Tagged tag a
addSameUnit (Tagged x) (Tagged y) = Tagged (x + y)

-- | Convert between units
metersToFeet :: Tagged Meters Double -> Tagged Feet Double
metersToFeet (Tagged m) = Tagged (m * 3.28084)

-- | Higher-kinded types and rank-N polymorphism  
type Forall f = forall a. f a -> String

showAny :: Forall f -> f Int -> String
showAny f x = f x

-- | Existential types
data SomeShow where
  SomeShow :: Show a => a -> SomeShow

instance Show SomeShow where
  show (SomeShow x) = show x

-- | Function creating existential types
makeSomeShow :: String -> SomeShow
makeSomeShow s = SomeShow s