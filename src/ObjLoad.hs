{-# LANGUAGE DataKinds, OverloadedLabels, TypeOperators, TypeApplications #-}

module ObjLoad where

import Graphics.Rendering.OpenGL hiding (Face, TexCoord)
import Linear (V2(..), V3(..))
import Data.List.Split
import Data.Array

-- Position/Texture Coordinate/Normals
type IGroup = (Int,Int,Int)

data Model_ = Model_ { vertexCoords_ :: [V3 GLfloat]
                   , vertexNorms_ :: [V3 GLfloat]
                   , texCoords_ :: [V2 GLfloat]
                   , faces_ :: [[IGroup]] }

data Model = Model { vertexCoords :: Array Int (V3 GLfloat)
                     , vertexNorms :: Array Int (V3 GLfloat)
                     , texCoords :: Array Int (V2 GLfloat)
                     , faces :: [IGroup] }

empty :: Model_
empty = Model_ { vertexCoords_ = []
              , vertexNorms_ = []
              , texCoords_ = []
              , faces_ = [] }

parseVCoord :: Model_ -> [GLfloat] -> Model_
parseVCoord m [x,y,z] = m { vertexCoords_ = newCoord:coords }
  where newCoord = V3 x y z
        coords = vertexCoords_ m

parseVNorm :: Model_ -> [GLfloat] -> Model_
parseVNorm m [x,y,z] = m { vertexNorms_ = newCoord:coords }
  where newCoord = V3 x y z
        coords = vertexNorms_ m

parseTCoord :: Model_ -> [GLfloat] -> Model_
parseTCoord m [x,y] = m { texCoords_ = newCoord:coords }
  where newCoord = V2 x y
        coords = texCoords_ m

parseFace :: Model_ -> [String] -> Model_
parseFace m igs = m { faces_ = newFace:faces__ }
  where newFace = map parseIGroup igs
        faces__ = faces_ m

parseIGroup :: String -> IGroup
parseIGroup s =
  case map toInt $ splitOn "/" s of
    [p,t,n] -> (p,t,n)
    _ -> ((-1),(-1),(-1))
  where toInt s = read s :: Int
        
parseLine :: Model_ -> String -> Model_
parseLine m s =
  case words s of
    "v":xs -> parseVCoord m $ map toGLf xs
    "vt":xs -> parseTCoord m $ map toGLf xs
    "vn":xs -> parseVNorm m $ map toGLf xs
    "f":xs -> parseFace m xs
    _ -> m
  where toGLf s = read s :: GLfloat

reverseModel :: Model_ -> Model_
reverseModel m = m { vertexCoords_ = reverse $ vertexCoords_ m
                   , vertexNorms_ = reverse $ vertexNorms_ m
                   , texCoords_ = reverse $ texCoords_ m
                   , faces_ = reverse $ faces_ m }

makeArrayModel :: Model_ -> Model
makeArrayModel m = Model { vertexCoords = toArr $ vertexCoords_ m
                         , vertexNorms = toArr $ vertexNorms_ m
                         , texCoords = toArr $ texCoords_ m
                         , faces = concat $ faces_ m }
  where toArr lst = listArray (1,length lst) lst

parseOBJ :: String -> IO Model
parseOBJ file = do
  contents <- readFile file
  return . makeArrayModel . reverseModel . aux empty $ lines contents
  where aux m [] = m
        aux m (x:xs) = aux (parseLine m x) xs
