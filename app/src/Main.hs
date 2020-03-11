module Main where

import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Control.Wire
import Data.Time
import FRP.Netwire
import Prelude hiding ((.), id, until)
import Control.Wire.Unsafe.Event
import Control.Monad.IO.Class

import Structure
import Rendering
import Rasterizer

param = Paramaters 1000 500

main = do
  -- print $ take 205 (findWorldUpdateInfo startingWorld)
  run clockSession_ (test 0)

myTime :: (HasTime t s, Monad m) => Wire s () m a Double
myTime = integral 0 . pure 1

startingWorld = World (0.5) (0.0)

printList :: [(Double, IO ())]
printList = fmap (\x -> (fst x,print (snd x))) $ findWorldUpdateInfo startingWorld

-- eventStream :: (HasTime t s, Monad m, Fractional t) => Wire s () m a (Event (IO ()))
-- eventStream = loop $ fmap (\x -> (convert (fst x), snd x)) $ printList
--   where
--      loop :: (HasTime t s, Monad m) => [(t, IO ())] -> Wire s () m a (Event (IO ()))
--      loop x = createOutput (head x) <& loop (tail x)
--      createOutput :: (HasTime t s, Monad m) => (t, IO ()) -> Wire s () m a (Event (IO ()))
--      createOutput x = at (fst x) . pure (snd x)
--      convert = fromRational . toRational

test :: (HasTime t s, Monad m, Fractional t) => Int -> Wire s () m a (Event (IO ()))
test i = createOutput $ (fmap (\x -> (convert (fst x), snd x)) $ printList) !! i
  where
     createOutput :: (HasTime t s, Monad m) => (t, IO ()) -> Wire s () m a (Event (IO ()))
     createOutput x = at (fst x) . pure (snd x)
     convert = fromRational . toRational

mergeTest :: (HasTime t s, Monad m, Fractional t) => Wire s () m a (Event (IO ()))
mergeTest = (test 0) <$ (test 1)

generateWorld :: (HasTime t s, Monad m) => Wire s () m a World
generateWorld = fmap (getWorld) myTime

generateRenderer :: (HasTime t s, Monad m) => Wire s () m a (IO ())
generateRenderer = fmap (sketchWith param) generateWorld

program :: (HasTime t s, Monad m, Fractional t) => Wire s () m () (Event (IO ()))
program = periodic 1 . generateRenderer


run :: (HasTime t s, MonadIO m) => Session m s -> Wire s e m () (Event (IO ())) -> m e
run = go
  where
    go session wire = do
      (dt, session') <- stepSession session
      (wt', wire') <- stepWire wire dt (Right ())
      case wt' of
        Left a -> return a
        Right bEvent -> do
          case bEvent of
            Event b -> liftIO $ b
            _ -> return ()
          go session' wire'
