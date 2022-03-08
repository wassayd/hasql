{-# language BlockArguments #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import ProjectRepository (getAllProject, createProject, deleteProject, updateProject)
import Database (runqry)
import AuthorRepository (getAllAuthor, createAuthor, deleteAuthor, updateAuthor)
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
      "delete project"  -> deleteProjectAction
      "delete author"   -> deleteProjectAction
      "update project"  -> updateProjectAction
      "update author"  -> updateProjectAction
      _               -> putStrLn "Unreachable"
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

updateProjectAction :: IO ()
updateProjectAction = do
  idstr <- inputAction "Project Id"
  updateProject (read idstr)

updateAuthorAction :: IO ()
updateAuthorAction = do
  idstr <- inputAction "Author Id"
  updateAuthor (read idstr)


deleteProjectAction :: IO ()
deleteProjectAction = do
  idstr <- inputAction "Project Id"
  deleteProject (read idstr) 


deleteAuthorAction :: IO ()
deleteAuthorAction = do
  idstr <- inputAction "Project Id"
  deleteAuthor (read idstr) 

inputAction :: String -> IO String
inputAction msg = do
  putStrLn msg
  getLine