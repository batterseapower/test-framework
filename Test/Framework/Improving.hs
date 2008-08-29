module Test.Framework.Improving (
        (:~>)(..), 
        ImprovingIO, yieldImprovement, runImprovingIO
    ) where

import Control.Concurrent.Chan
import Control.Monad


data i :~> f = Finished f
             | Improving i (i :~> f)

instance Functor ((:~>) i) where
    fmap f (Finished x)    = Finished (f x)
    fmap f (Improving x i) = Improving x (fmap f i)


newtype ImprovingIO i f a = IIO { unIIO :: Chan (Either i f) -> IO a }

instance Monad (ImprovingIO i f) where
    return x = IIO (const $ return x)
    ma >>= f = IIO $ \chan -> do
                    a <- unIIO ma chan
                    unIIO (f a) chan

yieldImprovement :: i -> ImprovingIO i f ()
yieldImprovement improvement = IIO $ \chan -> writeChan chan (Left improvement)

runImprovingIO :: ImprovingIO i f f -> IO (i :~> f, IO ())
runImprovingIO iio = do
    chan <- newChan
    let action = do
            result <- unIIO iio chan
            writeChan chan (Right result)
    improving_value <- fmap reifyListToImproving $ getChanContents chan
    return (improving_value, action)

reifyListToImproving :: [Either i f] -> (i :~> f)
reifyListToImproving (Left improvement:rest) = Improving improvement (reifyListToImproving rest)
reifyListToImproving (Right final:_)         = Finished final
reifyListToImproving []                      = error "reifyListToImproving: list finished before a final value arrived"