module LOGL.Objects
(
    simpleCube, cubeWithTexture, cubeWithNormals, cubeWithNormalsAndTexture, cubeMesh
)
where

import Graphics.Rendering.OpenGL.GL as GL hiding (normal, position, Vertex)
import LOGL.Mesh
import Linear.V2
import Linear.V3

cubeMesh :: [Texture] -> IO Mesh
cubeMesh texts = createMesh vertices indices texts
    where
        indices = [0..35]
        vertices = nextVertex cubeWithNormalsAndTexture

nextVertex :: [GLfloat] -> [Vertex]
nextVertex [] = []
nextVertex (x:y:z:nx:ny:nz:tx:ty:rest) = v : nextVertex rest
    where
        v =  Vertex { position = V3 x y z, normal = V3 nx ny nz, texCoords = V2 tx ty}

cubeWithNormalsAndTexture :: [GLfloat]
cubeWithNormalsAndTexture = [
    -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  0.0, 0.0,
    0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  1.0, 0.0,
    0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  1.0, 1.0,
    0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  1.0, 1.0,
   -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,  0.0, 1.0,
   -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,  0.0, 0.0,

   -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,   0.0, 0.0,
    0.5, -0.5,  0.5,  0.0,  0.0, 1.0,   1.0, 0.0,
    0.5,  0.5,  0.5,  0.0,  0.0, 1.0,   1.0, 1.0,
    0.5,  0.5,  0.5,  0.0,  0.0, 1.0,   1.0, 1.0,
   -0.5,  0.5,  0.5,  0.0,  0.0, 1.0,   0.0, 1.0,
   -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,   0.0, 0.0,

   -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,  1.0, 0.0,
   -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,  1.0, 1.0,
   -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,  0.0, 1.0,
   -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,  0.0, 1.0,
   -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,  0.0, 0.0,
   -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,  1.0, 0.0,

    0.5,  0.5,  0.5,  1.0,  0.0,  0.0,  1.0, 0.0,
    0.5,  0.5, -0.5,  1.0,  0.0,  0.0,  1.0, 1.0,
    0.5, -0.5, -0.5,  1.0,  0.0,  0.0,  0.0, 1.0,
    0.5, -0.5, -0.5,  1.0,  0.0,  0.0,  0.0, 1.0,
    0.5, -0.5,  0.5,  1.0,  0.0,  0.0,  0.0, 0.0,
    0.5,  0.5,  0.5,  1.0,  0.0,  0.0,  1.0, 0.0,

   -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  0.0, 1.0,
    0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  1.0, 1.0,
    0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  1.0, 0.0,
    0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  1.0, 0.0,
   -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,  0.0, 0.0,
   -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,  0.0, 1.0,

   -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  0.0, 1.0,
    0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  1.0, 1.0,
    0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  1.0, 0.0,
    0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  1.0, 0.0,
   -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,  0.0, 0.0,
   -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,  0.0, 1.0]


cubeWithTexture :: [GLfloat]
cubeWithTexture = [
    -0.5, -0.5, -0.5,  0.0, 0.0,
    0.5, -0.5, -0.5,  1.0, 0.0,
    0.5,  0.5, -0.5,  1.0, 1.0,
    0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5,  0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 0.0,

    -0.5, -0.5,  0.5,  0.0, 0.0,
    0.5, -0.5,  0.5,  1.0, 0.0,
    0.5,  0.5,  0.5,  1.0, 1.0,
    0.5,  0.5,  0.5,  1.0, 1.0,
    -0.5,  0.5,  0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,

    -0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5, -0.5,  1.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5,  0.5,  1.0, 0.0,

    0.5,  0.5,  0.5,  1.0, 0.0,
    0.5,  0.5, -0.5,  1.0, 1.0,
    0.5, -0.5, -0.5,  0.0, 1.0,
    0.5, -0.5, -0.5,  0.0, 1.0,
    0.5, -0.5,  0.5,  0.0, 0.0,
    0.5,  0.5,  0.5,  1.0, 0.0,

    -0.5, -0.5, -0.5,  0.0, 1.0,
    0.5, -0.5, -0.5,  1.0, 1.0,
    0.5, -0.5,  0.5,  1.0, 0.0,
    0.5, -0.5,  0.5,  1.0, 0.0,
    -0.5, -0.5,  0.5,  0.0, 0.0,
    -0.5, -0.5, -0.5,  0.0, 1.0,

    -0.5,  0.5, -0.5,  0.0, 1.0,
    0.5,  0.5, -0.5,  1.0, 1.0,
    0.5,  0.5,  0.5,  1.0, 0.0,
    0.5,  0.5,  0.5,  1.0, 0.0,
    -0.5,  0.5,  0.5,  0.0, 0.0,
    -0.5,  0.5, -0.5,  0.0, 1.0]

cubeWithNormals :: [GLfloat]
cubeWithNormals = [
    -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
     0.5, -0.5, -0.5,  0.0,  0.0, -1.0,
     0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
     0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
    -0.5,  0.5, -0.5,  0.0,  0.0, -1.0,
    -0.5, -0.5, -0.5,  0.0,  0.0, -1.0,

    -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
     0.5, -0.5,  0.5,  0.0,  0.0, 1.0,
     0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
     0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
    -0.5,  0.5,  0.5,  0.0,  0.0, 1.0,
    -0.5, -0.5,  0.5,  0.0,  0.0, 1.0,

    -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,
    -0.5,  0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5, -0.5, -1.0,  0.0,  0.0,
    -0.5, -0.5,  0.5, -1.0,  0.0,  0.0,
    -0.5,  0.5,  0.5, -1.0,  0.0,  0.0,

     0.5,  0.5,  0.5,  1.0,  0.0,  0.0,
     0.5,  0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5, -0.5,  1.0,  0.0,  0.0,
     0.5, -0.5,  0.5,  1.0,  0.0,  0.0,
     0.5,  0.5,  0.5,  1.0,  0.0,  0.0,

    -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
     0.5, -0.5, -0.5,  0.0, -1.0,  0.0,
     0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
     0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
    -0.5, -0.5,  0.5,  0.0, -1.0,  0.0,
    -0.5, -0.5, -0.5,  0.0, -1.0,  0.0,

    -0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
     0.5,  0.5, -0.5,  0.0,  1.0,  0.0,
     0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
     0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
    -0.5,  0.5,  0.5,  0.0,  1.0,  0.0,
    -0.5,  0.5, -0.5,  0.0,  1.0,  0.0]

simpleCube :: [GLfloat]
simpleCube = [
    -0.5, -0.5, -0.5,
    0.5, -0.5, -0.5,
    0.5,  0.5, -0.5,
    0.5,  0.5, -0.5,
    -0.5,  0.5, -0.5,
    -0.5, -0.5, -0.5,

    -0.5, -0.5,  0.5,
    0.5, -0.5,  0.5,
    0.5,  0.5,  0.5,
    0.5,  0.5,  0.5,
    -0.5,  0.5,  0.5,
    -0.5, -0.5,  0.5,

    -0.5,  0.5,  0.5,
    -0.5,  0.5, -0.5,
    -0.5, -0.5, -0.5,
    -0.5, -0.5, -0.5,
    -0.5, -0.5,  0.5,
    -0.5,  0.5,  0.5,

    0.5,  0.5,  0.5,
    0.5,  0.5, -0.5,
    0.5, -0.5, -0.5,
    0.5, -0.5, -0.5,
    0.5, -0.5,  0.5,
    0.5,  0.5,  0.5,

    -0.5, -0.5, -0.5,
    0.5, -0.5, -0.5,
    0.5, -0.5,  0.5,
    0.5, -0.5,  0.5,
    -0.5, -0.5,  0.5,
    -0.5, -0.5, -0.5,

    -0.5,  0.5, -0.5,
    0.5,  0.5, -0.5,
    0.5,  0.5,  0.5,
    0.5,  0.5,  0.5,
    -0.5,  0.5,  0.5,
    -0.5,  0.5, -0.5]
