#!/usr/bin/env stack
-- stack --resolver lts-12.12 script --package optparse-applicative --package aeson --package mtl
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
module Config
    ( Config
    , Config'(..)
    , resolve
    , main
    )
where

import           Prelude                   hiding (lookup)

import           Control.Applicative       (optional, (<**>), (<|>))
import           Control.Monad.Error.Class (MonadError, liftEither)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Aeson                as Aeson
import           Data.Functor.Identity     (Identity)
import           GHC.Generics              (Generic)
import qualified Options.Applicative       as Opt
import           System.Environment        (lookupEnv)


type family Setting (f :: * -> *) a where
    Setting Identity a = a
    Setting Maybe    a = Maybe a


data Config' f = Config
    { foo :: Setting f String
    , bar :: Setting f String
    } deriving Generic


type Config = Config' Identity
deriving instance Show Config


type PartialConfig = Config' Maybe
deriving instance Show PartialConfig
deriving instance Aeson.FromJSON PartialConfig  -- customise json deserialization here

instance Semigroup PartialConfig where
    -- logic for merging (partial) configuration values goes here
    c1 <> c2 = Config
        { foo = foo c1 <|> foo c2
        , bar = bar c1 <|> bar c2
        }

instance Monoid PartialConfig where
    -- The default (empty) configuration
    mempty = Config mempty mempty


-- Retrieve a partial configuration from command line arguments. May also return
-- a config file path.
fromArgs :: forall m . MonadIO m => m (PartialConfig, Maybe FilePath)
fromArgs = liftIO (Opt.execParser args)
  where
    args :: Opt.ParserInfo (PartialConfig, Maybe FilePath)
    args = Opt.info (parser <**> Opt.helper) $ mconcat

        [ Opt.fullDesc
        , Opt.header "https://github.com/jmackie/haskell-config-example"
        ]

    parser :: Opt.Parser (PartialConfig, Maybe FilePath)
    parser = (,) <$> parseConfig <*> parseConfigFilePath

    parseConfig :: Opt.Parser PartialConfig
    parseConfig =
        Config
            <$> optional
                    ( Opt.strOption
                    $ mconcat
                          [ Opt.long "foo"
                          , Opt.metavar "FOO"
                          , Opt.help "Pointless string"
                          ]
                    )
            <*> optional
                    ( Opt.strOption
                    $ mconcat
                          [ Opt.long "bar"
                          , Opt.metavar "BAR"
                          , Opt.help "Another pointless string"
                          ]
                    )

    parseConfigFilePath :: Opt.Parser (Maybe FilePath)
    parseConfigFilePath = optional
        (Opt.strOption $ mconcat
            [ Opt.long "config"
            , Opt.metavar "FILE"
            , Opt.help "Path to a config file (json)"
            ]
        )


-- Retrieve a partial configuration from environment variables.
fromEnv :: forall m . MonadIO m => m PartialConfig
fromEnv = do
    -- logic for mapping settings to environment variables lives here
    foo <- lookup "FOO"
    bar <- lookup "BAR"
    pure Config {foo , bar }
  where
    lookup :: String -> m (Maybe String)
    lookup = liftIO . lookupEnv


-- Retrieve a partial configuration from a config file.
fromFile :: (MonadIO m, MonadError String m) => FilePath -> m PartialConfig
fromFile configFilePath = do
    -- NOTE: could also case on the file extension and use json|yaml|toml
    -- formats as necessary
    result <- liftIO (Aeson.eitherDecodeFileStrict configFilePath)
    liftEither result


-- Fill in a partial config, maybe throwing errors (e.g. if there are empty
-- settings for which there is no sensible default).
finalize :: forall m . MonadError String m => PartialConfig -> m Config
finalize partial = do
    foo <- withDefault "default" (foo partial)
    bar <- withDefault "default" (bar partial)
    pure Config {foo , bar }
  where
    withDefault :: a -> Maybe a -> m a
    withDefault def = liftEither . maybe (Right def) Right


-- Run the whole config resolution dance.
resolve :: (MonadIO m, MonadError String m) => m Config
resolve = do
    (configArgs, configFilePath) <- fromArgs
    configEnv                    <- fromEnv
    configFile                   <- maybe (pure mempty) fromFile configFilePath
    finalize (configArgs <> configFile <> configEnv)


main :: IO ()
main = runExceptT resolve >>= print
