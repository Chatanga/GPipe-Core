{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Graphics.GPipe.VertexInput where

import Graphics.GPipe.Buffer
import Graphics.GPipe.Shader
import Control.Arrow (Kleisli(..))
import Control.Monad.IO.Class (liftIO)

class BufferFormat a => VertexInput a where
    type ShaderInput a  
    toVertex :: Shader os a (ShaderInput a)
    
instance VertexInput BFloat where
    type ShaderInput BFloat = VFloat
    toVertex = Shader (Kleisli shader) (Kleisli setup)
        where
            shader _ = do attrId <- getNextGlobal
                          let attr = 'a' : show attrId
                          tellGlobalDecl $ "in float " ++ attr 
                          return $ S $return attr
            setup (B name off) =  do attrId <- getNextGlobal
                                     liftIO $ glSetAttribute attrId name off
                                     return undefined
            setup (BConst a) =  do attrId <- getNextGlobal
                                   liftIO $ glSetGenericAttribute attrId a
                                   return undefined

glSetAttribute :: t -> t1 -> t2 -> IO ()
glSetAttribute _ _ _ = undefined                                       

glSetGenericAttribute :: t -> t1 -> IO ()
glSetGenericAttribute _ _ = undefined   