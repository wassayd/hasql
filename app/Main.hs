module Main where
import ProjectRepository (getAllProject)
import Rel8
import Database (runqry)

 
main = do
  runqry "Get All Projects :" getAllProject
  putStrLn "Finished"