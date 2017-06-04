{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.App where

import           Control.Monad.Reader
import           Data.Config.Config

type AppConfig = MonadReader Config

newtype App a = App {
    runApp :: ReaderT Config IO a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO)

