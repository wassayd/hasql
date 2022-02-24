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

type Action = String 

isActionAuthorized :: Action -> Bool 
isActionAuthorized "show authors"    = True   
isActionAuthorized "show projects"   = True
isActionAuthorized "create author"   = True   
isActionAuthorized "create project"  = True
isActionAuthorized "update author"   = True 
isActionAuthorized "update project"  = True
isActionAuthorized "delete author"   = True 
isActionAuthorized "delete project"  = True
isActionAuthorized _                 = False    
 
main :: IO ()
main = do
  putStr "Agrgument : "
  action <- getLine
  if isActionAuthorized action then 
    case action of
      "show authors"  -> runqry "List of all Authors" getAllAuthor
      "show projects" -> runqry "List of all Projects" getAllProject 
      _             -> undefined
  else
    putStrLn "Argument invalid"