module Main where
import ProjectRepository (getAllProject)
import Rel8
import Database (runqry)
import AuthorRepository (getAllAuthor)
import Classes

data Action f = Action{
    nameAction :: String
  , fun :: f
}
 
availableAction :: Rel8able a => [Action (Query (a Expr))]
availableAction = [ 
  Action { nameAction="createPrj", fun = getAllProject }
  ,Action { nameAction="s", fun = getAllAuthor  } ]
  

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

main :: IO ()
main = do
  putStr "Agrgument : "
  action <- getLine
  if isActionAuthorized action then 
    putStrLn "Argument valid"
  else
    putStrLn "Argument invalid"