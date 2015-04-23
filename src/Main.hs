{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception.Enclosed
import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Data.Maybe
import           Data.Monoid
import           Data.String.Class          (toString, fromString)
import           Network.HTTP.Client        (Manager)
import qualified Network.HTTP.Client        as HTTPClient
import           Network.Wreq
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.Timeout             (timeout)

main :: IO ()
main = do
  mgr <- HTTPClient.newManager HTTPClient.defaultManagerSettings
  quickHttpServe (site mgr)

site :: Manager -> Snap ()
site mgr =
    ifTop (writeBS "hello world") <|>
    route [ ("worker", workerHandler mgr)
          , ("slowserve", slowserveHandler)
          ] <|>
    dir "static" (serveDirectory ".")

workerHandler :: Manager -> Snap ()
workerHandler mgr = do
    let ps = [Provider1, Provider2]
    rs <- doo `seq` liftIO $ fmap catMaybes $ mapConcurrently fetch ps
    -- liftIO $ putStrLn ("Got results! " ++ show rs)
    doo `seq` writeBS (fromString (show rs))
  where
    fetch p = ctch p $ tm p $ do
      case p of
        Provider1 -> do
          -- putStrLn "Fetching Provider1"
          let opts = defaults & manager .~ (Right mgr)
          r <- getWith opts "http://localhost:8001/slowserve"
          -- liftIO $ putStrLn ("Got response: " <> toString (r ^. responseBody))
          return (doo `seq` (Just (Provider1Response (r ^. responseBody))))
        Provider2 -> do
          -- putStrLn "Provider2 is pure"
          return (doo `seq` (Just Provider2Response))
    tm p f = do
        res <- timeout (100000 * 5) f
        case res of
          Nothing -> do
            case p of
              Provider2 -> putStrLn ("Got timeout while requesting " <> show p)
              _ -> return ()
            return Nothing
          Just r -> return r
    ctch p f =
        catchAny f (\e -> do
          putStrLn ("Exception while fetching " <> show p <> ": " <> show e)
          return Nothing)

slowserveHandler :: Snap ()
slowserveHandler = do
    liftIO (threadDelay (sec * 2))
    writeBS "Your content is delivered"

data Provider = Provider1
              | Provider2
    deriving (Show, Eq)

data ProviderResponse a = Provider1Response a
                        | Provider2Response
    deriving (Show, Eq)

sec :: Int
sec = 10^(6::Int)

-- | Unintentionally slow fibonacci
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) * n

doo = fib 20000
