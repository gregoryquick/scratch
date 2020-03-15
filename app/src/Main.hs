module Main where

import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Control.Wire
import Data.Time
import FRP.Netwire
import Prelude hiding ((.), id, until)
import Control.Wire.Unsafe.Event
import Control.Monad.IO.Class
import Data.Fixed

import Structure
import Rendering
import Rasterizer

param = Paramaters 1000 500

main = do
  -- print $ take 205 (findWorldUpdateInfo startingWorld)
  run clockSession_ eventStream

myTime :: (HasTime t s, Monad m) => Wire s e m a Double
myTime = integral 0 . pure 1

startingWorld = World (0.5) (0.0)

printList :: [(Double, IO ())]
printList = fmap (\x -> (fst x,print (snd x))) $ findWorldUpdateInfo startingWorld

-- timeModified :: [(Double, IO ())]
-- timeModified = (head printList) : fmap (\x -> ((fst x)/10,snd x)) (tail printList)

eventStream :: (HasTime t s, Monad m, Fractional t, Show t) => Wire s e m a (Event (IO ()))
eventStream = createEvents $ fmap (\x -> (convert $ fst x,(snd x) >> (print $ show $ fst x))) $ printList--foldl1 (<&) $ fmap createOutput $ fmap (\x -> (convert (fst x), snd x)) $ printList
  where
     createOutput :: (HasTime t s, Monad m) => (t, IO ()) -> Wire s e m a (Event (IO ()))
     createOutput x = at (fst x) . pure (snd x)
     convert = fromRational . toRational


createEvents :: (HasTime t s) => [(t,b)] -> Wire s e m a (Event b)
createEvents [] = never
createEvents (x:xs) = mkSFN $ \_ -> (Event (snd x), loop (fst x) xs)
    where
    loop _ [] = never
    loop 0 xs = loop (fst x) xs
    loop t' xs0@(x:xs) =
        mkSF $ \ds _ ->
            let t = t' - dtime ds
            in if t <= 0
                 then (Event (snd x), loop (mod' t (fst x)) xs)
                 else (NoEvent, loop t xs0)

-- test :: (HasTime t s, Monad m, Fractional t) => Int -> Wire s e m a (Event (IO ()))
-- test i = createOutput $ (fmap (\x -> (convert (fst x), snd x)) $ printList) !! i
  -- where
     -- createOutput :: (HasTime t s, Monad m) => (t, IO ()) -> Wire s e m a (Event (IO ()))
     -- createOutput x = at (fst x) . pure (snd x)
     -- convert = fromRational . toRational

-- mergeTest :: (HasTime t s, Monad m, Fractional t) => Wire s e m a (Event (IO ()))
-- mergeTest = (test 0) <& (test 1)

generateWorld :: (HasTime t s, Monad m) => Wire s e m a World
generateWorld = fmap (getWorld) myTime

generateRenderer :: (HasTime t s, Monad m) => Wire s e m a (IO ())
generateRenderer = fmap (sketchWith param) generateWorld

program :: (HasTime t s, Monad m, Fractional t) => Wire s e m () (Event (IO ()))
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
