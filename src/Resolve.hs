{-# LANGUAGE OverloadedStrings #-}
module Resolve where

import           Data.IP              (IPv4)
import           Data.List            (foldl')
import qualified Data.Map.Strict      as Map
import           Network.DNS          (Domain, defaultResolvConf,
                                       makeResolvSeed, withResolver)
import           Network.DNS.Lookup   (lookupA)
import           Network.DNS.Resolver (Resolver)

type IPMap = Map.Map IPv4 Bool
type ResolutionTries = Int

fullyResolve :: Domain -> ResolutionTries -> IO [IPv4]
fullyResolve domain maxTries = do
    rs <- makeResolvSeed defaultResolvConf
    names <- withResolver rs $ \resolver -> getName resolver maxTries Map.empty
    return $ Map.keys names
  where
    addName :: (Bool, IPMap) -> IPv4 -> (Bool, IPMap)
    addName (added, nameMap) name
        | Map.member name nameMap = (added, nameMap)
        | otherwise               = (True, Map.insert name True nameMap)
    getName :: Resolver -> Int -> IPMap -> IO IPMap
    getName _ 0 resNames = return resNames
    getName resolver count resNames = do
        result <- lookupA resolver domain
        case result of
            Left _      -> return resNames
            Right names -> do
                let (added, resNames') = foldl' addName (False, resNames) names
                if added then getName resolver maxTries resNames'
                         else getName resolver (count-1) resNames'
