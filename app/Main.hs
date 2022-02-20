{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import ProjectRepository (getAllProject)
import Rel8
import Database (runqry)
import AuthorRepository (getAllAuthor)
import Classes
import Data.Maybe (fromMaybe)

isActionAuthorized :: String -> Bool 
isActionAuthorized "show authors"    = True   
isActionAuthorized "show projects"   = True
isActionAuthorized "create author"   = True   
isActionAuthorized "create project"  = True
isActionAuthorized "update author"   = True 
isActionAuthorized "update project"  = True
isActionAuthorized "delete author"   = True 
isActionAuthorized "delete project"  = True
isActionAuthorized _                 = False    
 
 
main = do
  putStr "Agrgument : "
  action <- getLine
  if isActionAuthorized action then 
    case action of
      "show authors" -> return $ Just getAllAuthor 
      _             -> undefined
  else
    putStrLn "Argument invalid"