module MonadicLogicalConnectives (
    negProp,
    (<&&>),
    (<||>),
    imp,
    iff)
where

import Control.Monad


negProp :: Monad m => (t -> m Bool) -> (t -> m Bool)
negProp f = (\z -> f z >>= return . not)


(<&&>) :: Monad m => (t -> m Bool) -> (t -> m Bool) -> t -> m Bool
(<&&>) x y = (\z -> liftM2 (&&) (x z) (y z))


(<||>) :: Monad m => (t -> m Bool) -> (t -> m Bool) -> t -> m Bool
(<||>) x y = (\z -> liftM2 (||) (x z) (y z))


imp :: Monad m => (t -> m Bool) -> (t -> m Bool) -> t -> m Bool
x `imp` y = (\z -> liftM2 (||) (x z >>= return . not) (y z))


iff :: Monad m => (t -> m Bool) -> (t -> m Bool) -> t -> m Bool
x `iff` y = (x <&&> y) <||> ((negProp x) <&&> (negProp y))



infixr 9 `iff`
infixr 8 <&&>
infixr 7 <||>
infixr 6 `imp`