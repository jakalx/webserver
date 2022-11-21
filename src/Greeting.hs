module Greeting (
    greet,
) where

import Control.Monad.IO.Class (
    MonadIO,
    liftIO,
 )

greet :: MonadIO m => String -> m ()
greet = liftIO <$> putStrLn
