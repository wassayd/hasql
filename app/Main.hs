module Main where
import ProjectRepository (getAllProject)
import Rel8
import Database (runqry)
import AuthorRepository (getAllAuthor)

 
main = do
  runqry "Get All Projects :" getAllProject
  runqry "Get All Authors :" getAllAuthor
  putStrLn "Finished"