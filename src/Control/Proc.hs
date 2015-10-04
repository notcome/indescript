module Control.Proc where

import Control.Monad.Except

tryBool :: (MonadError e m) => Bool -> e -> m ()
tryBool cond res = if cond then throwError res
               else return ()

tryMaybe :: (MonadError e m) => Maybe a -> (a -> e) -> m ()
tryMaybe (Just x) f = throwError $ f x
tryMaybe Nothing  _ = return ()

runProc :: Except a e -> Either e a
runProc proc = case runExcept proc of
  (Left  res) -> Right res
  (Right err) -> Left  err
