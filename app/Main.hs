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
 
main :: Rel8able a => IO (Maybe (Query (a Expr)))
main = do
  putStr "Agrgument : "
  action <- getLine
  if isActionAuthorized action then 
    case action of
      "show autors" -> return  $  Maybe getAllAuthor 
      _             -> undefined
  else
    putStrLn "Argument invalid"