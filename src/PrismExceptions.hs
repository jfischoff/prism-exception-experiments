{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module PrismExceptions where

import           Control.Lens
import           Control.Monad.Except

--------------------------------------------------------------------------------
--- BEGIN Open Union Kit
--------------------------------------------------------------------------------

data Nat = NZ | NS !Nat

type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = NZ
  RIndex r (s ': rs) = NS (RIndex r rs)

data Variant :: [*] -> * where
    Z :: x -> Variant (x ': xs)
    S :: Variant xs -> Variant (x ': xs)

_S :: Prism' (Variant (x ': xs)) (Variant xs)
_S = prism' S $ \case
        S v -> Just v
        _ -> Nothing

class i ~ RIndex x xs => Has xs x i where
    inj :: Prism' (Variant xs) x

instance Has (x ': xs) x NZ where
    inj = prism' Z $ \case
            Z x -> Just x
            _ -> Nothing

instance (RIndex x (a ': xs) ~ NS i, Has xs x i) => Has (a ': xs) x (NS i) where
    inj = _S . inj

throwE :: (Has xs e i, MonadError (Variant xs) m) => e -> m a
throwE = throwError . review inj

--------------------------------------------------------------------------------
--- END Open Union Kit
--------------------------------------------------------------------------------

data ZeroDivisor = ZeroDivisor

safeDivsion :: (Has xs ZeroDivisor i, MonadError (Variant xs) m) => Double -> Double -> m Double
safeDivsion x y
    | y == 0 = throwE ZeroDivisor
    | otherwise = return $ x / y

newtype IdNotFound = IdNotFound Int

lookupId :: (Has xs IdNotFound i, MonadError (Variant xs) m) => Int -> m String
lookupId theId
    | theId == 1 = return "Foo"
    | otherwise = throwE $ IdNotFound theId

combine :: ( Has xs IdNotFound i
           , Has xs ZeroDivisor i'
           , MonadError (Variant xs) m
           )
        => m ()
combine = do
    safeDivsion 1 0
    lookupId 1
    return ()

test :: IO ()
test = do
    case runExcept combine of
        Right () -> putStrLn "success"
        -- I have to pick an order for this to work, but the order is arbitrary
        -- The order does effect how the patterns work (whether it is S (..) or
        -- just Z).
        -- The explicit type annotation is a downside but it is better than
        -- making a bunch of one off types
        Left v -> case v :: Variant [IdNotFound, ZeroDivisor] of
            S (Z ZeroDivisor) -> putStrLn "zero divisor"
            Z (IdNotFound i)  -> putStrLn $ "id not found " ++ show i
