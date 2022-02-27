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
import ProjectRepository (getAllProject, createProject, deleteProject)
import Rel8
import Database (runqry)
import AuthorRepository (getAllAuthor, createAuthor)
import Classes
import Data.Maybe (fromMaybe)
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)

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
  putStr "Argument : "
  action <- getLine
  if isActionAuthorized action then 
    case action of
      "show authors"    -> runqry "List of all Authors" getAllAuthor
      "show projects"   -> runqry "List of all Projects" getAllProject
      "create author"   -> createAuthorAction
      "create project"  -> createAuthorAction
      _               -> undefined
  else
    putStrLn "Argument invalid"


createAuthorAction :: IO ()
createAuthorAction = do
  idstr <- inputAction "Author Id"
  let id = read idstr
  name <- inputAction "Author name" 
  url <- inputAction "Author url" 
  createAuthor id name url

createProjectAction :: IO ()
createProjectAction = do
  idstr <- inputAction "Author Id"
  let authorId = read idstr
  name <- inputAction "Project Name" 
  createProject authorId name

 
deleteProjectAction :: IO ()
deleteProjectAction = do
  idstr <- inputAction "Project Id"
  deleteProject (read idstr) 

inputAction :: String -> IO String
inputAction msg = do
  putStrLn msg
  getLine