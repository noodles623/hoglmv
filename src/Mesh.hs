{-# LANGUAGE DataKinds, OverloadedLabels, TypeOperators, TypeApplications #-}

module Mesh where

import Control.Applicative
import Control.Lens ((+~), (^.), contains)
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Linear (V1(..), V2(..), V3(..), V4(..), _x, M44, (!*!))
import System.FilePath ((</>))
import Control.Monad (join)
import If
import Graphics.GL as R
import Data.Array hiding (indices)

import ObjLoad

type Viewport = '("viewport", V2 GLsizei)

type AppInfo = FieldRec '[ '("cam", M44 GLfloat)
                         , '("proj", M44 GLfloat)
                         , '("trans", M44 GLfloat)
                         , Viewport ]

type CamInfo = '[ '("proj",  M44 GLfloat)
                , '("cam",   M44 GLfloat) 
                , '("trans", M44 GLfloat) ]


type Pos = "vertexCoord" ::: V3 GLfloat
type Tex = "texCoord" ::: V2 GLfloat
type Norm = "vertexNorm" ::: V3 GLfloat
type Spec = "specular" ::: V4 GLfloat
type Shin = "shininess" ::: V1 GLfloat

startState :: (Int, Int) -> Float -> AppInfo
startState (w,h) _ = xrec
                     ( mCam
                     , mProj
                     , mCam
                     , V2 (fromIntegral w) (fromIntegral h) )               
  where mProj = projectionMatrix (deg2rad 45) (wf/hf) 0.5 100.0
        mCam = camMatrix fpsCamera
        wf = fromIntegral w :: Float
        hf = fromIntegral h :: Float

loadTextures :: [FilePath] -> IO [TextureObject]
loadTextures = fmap (either error id . sequence) . mapM aux
  where aux f = do
          img <- readTexture ("resources" </> f)
          traverse_ (const texFilter) img
          return img
        texFilter = do
          textureFilter Texture2D $= ((Linear', Nothing), Linear')
          texture2DWrap $= (Repeated, ClampToEdge)

m44_ :: [V4 GLfloat] -> M44 GLfloat
m44_ [r1,r2,r3,r4] = V4 r1 r2 r3 r4
  
rotateY :: Float -> M44 GLfloat
rotateY deg = m44_ [ V4 (cos deg)     0 (sin deg) 0
                   , V4 0             1         0 0
                   , V4 (- (sin deg)) 0 (cos deg) 0 
                   , V4 0             0         0 1 ]

rotateX :: Float -> M44 GLfloat
rotateX deg = m44_ [ V4 1         0           0 0
                   , V4 0 (cos deg) (- sin deg) 0 
                   , V4 0 (sin deg) (cos deg)   0
                   , V4 0         0         0   1 ]

id4 :: M44 GLfloat
id4 = m44_ [ V4 1 0 0 0
           , V4 0 1 0 0
           , V4 0 0 1 0
           , V4 0 0 0 1 ]

trans deg = foldr (!*!) id4 [ rotateY deg ]

animate :: AppInfo -> Float -> IO AppInfo
animate info ct = return $
  rputf #trans (trans ct) $
  rputf #cam newCam info
  where deg = sin ct * 20
        newCam = camMatrix $
                 tilt (-40) $
                 dolly (V3 0 7.0 6.5) $ fpsCamera

makeVertice ::  Model -> IGroup -> FieldRec '[Pos,Tex,Norm,Spec,Shin]
makeVertice m (pi,ti,ni) = (#vertexCoord =:= (vc ! pi)) <+>
                           (#texCoord =:= (tc ! ti)) <+>
                           (#vertexNorm =:= (nc ! ni)) <+>
                           (#specular =:= V4 0 0 0 0) <+>
                           (#shininess =:= V1 (0 :: GLfloat))
  where vc = vertexCoords m
        tc = texCoords m
        nc = vertexNorms m

loadVertices :: Model -> [FieldRec '[Pos,Tex,Norm,Spec,Shin]]
loadVertices m = map (makeVertice m) $ faces m

indices :: Model -> [Word32]
indices m = take (length $ faces m) [0,1..]

drawModel :: String -> [String] -> IO (AppInfo -> IO ())
drawModel modelFile textureFiles = do
  m <- parseOBJ ("resources"</>modelFile)
  vb <- bufferVertices $ loadVertices m
  eb <- makeBuffer ElementArrayBuffer $ indices m
  ts <- loadTextures textureFiles
  s <- simpleShaderProgram ("shaders"</>"phong.vert")
                           ("shaders"</>"phong.frag")
  vao <- makeVAO $ do currentProgram $= Just (program s)
                      setUniforms s (#tex =:= map V1 [0,1::GLint])
                      enableVertices' s vb
                      bindVertices vb
                      bindBuffer ElementArrayBuffer $= Just eb
  let tris = fromIntegral $ (length . faces) m `div` 3
  let ss = setUniforms s
  return $ \appInfo -> withVAO vao . withTextures2D ts $ 
    do currentProgram $= Just (program s)
       ss (rcast appInfo :: FieldRec CamInfo)
       drawIndexedTris tris

setup :: String -> [String] -> Float -> Float -> IO (AppInfo -> IO ())
setup model texs w h = do
--  R.glFrontFace GL_CW
  clearColor $= Color4 0.8 0.8 0.8 1.0
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  draw <- drawModel model texs
  return $ \x -> vp x (draw x)
  where vp = withViewport (Position px py)
           . (\(V2 w h) -> Size w h) . (subtract vPos)
           . rvalf #viewport
        vPos@(V2 px py) = V2 0 0

render :: (AppInfo -> IO ()) -> AppInfo -> IO ()
render draw r = do
  clear [ColorBuffer, DepthBuffer]
  draw r
