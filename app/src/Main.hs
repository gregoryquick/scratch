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

import Data.List

param = Paramaters 1000 500

main = do
  -- print $ take 18 $ worlds startingWorld
  run clockSession_ eventStream

startingWorld = World (0.5) (0.0)

printList :: [(Double, IO ())]
printList = fmap (\x -> (fst x, print (snd x))) $ worlds startingWorld


eventStream :: (HasTime t s, Monad m, Fractional t, Show t) => Wire s e m a (Event (IO ()))
eventStream = createEvents $ fmap (\x -> (convert $ fst x, snd x)) printList
  where
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
