module Main where

import Graphics.Rendering.Cairo
import Control.Monad.Reader
import Control.Wire
import Data.Time
import Prelude hiding ((.), id, until)
import Control.Wire.Unsafe.Event
import Control.Monad.IO.Class
import Data.Fixed
import Data.List

import Structure
import Rendering
import Rasterizer
import Data.VectorSpace


param = Paramaters 1000 500

main = do
  createNewBlank param
  run clockSession_ eventStream

convertList :: [(Double,World)] -> [(Double, IO ())]
convertList myList = fmap (\x -> (fst x,(>>) (print x) $ sketchWith param $ snd x)) myList

worldStream :: (HasTime t s, Monad m, Fractional t) => Wire s () m a (IO ())
worldStream = asSoonAs . eventStream

eventStream :: (HasTime t s, Monad m, Fractional t) => Wire s e m a (Event (IO ()))
eventStream = createEvents $ fmap (\x -> (convert $ fst x, snd x)) $ convertList $ worlds startingWorld
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
