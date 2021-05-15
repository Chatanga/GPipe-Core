{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Arrows, GeneralizedNewtypeDeriving, GADTs, MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.GPipe.Internal.TransformFeedback where

import Graphics.GPipe.Internal.Compiler
import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.GeometryStream
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Debug

import Graphics.GL.Core45
import Graphics.GL.Types

import Data.IORef
import Data.IntMap.Lazy (insert)
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- | Defines how the size of the feedback content must be etablished.
data FeedbackBufferSizing
    = Fixed -- ^ The feedback content is expected to fit exactly in the buffer size.
    | Queried -- ^ Using an explicit GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN query.
    | Stored -- ^ Rely on a transform feedback object to store it.

drawNothing :: forall p a s c ds os f. (PrimitiveTopology p, VertexInput a, GeometryExplosive (VertexFormat a))
    => Window os c ds
    -- Output feedback buffers should remain black boxes until synchronized
    -- which won't be necessary when using glDrawTransformFeedback (add a flag
    -- for it?).
    -> (s -> Buffer os a)
    -> FeedbackBufferSizing
    -- maxVertices
    -> Int
    -- We should use a primitive (vertex) stream too, but the way we deal
    -- currently with modular stages is not flexible enough and we stick with
    -- geometry stream.
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> Shader os s ()
drawNothing w getTransformFeedbackBuffer sizing maxVertices gs = Shader $ tellDrawcalls w gs getTransformFeedbackBuffer sizing maxVertices

tellDrawcalls :: forall p a s c ds os. (PrimitiveTopology p, VertexInput a, GeometryExplosive (VertexFormat a))
    => Window os c ds
    -> GeometryStream (GGenerativeGeometry p (VertexFormat a))
    -> (s -> Buffer os a)
    -> FeedbackBufferSizing
    -> Int
    -> ShaderM s ()
tellDrawcalls w (GeometryStream xs) getTransformFeedbackBuffer sizing maxVertices =  mapM_ f xs where
    f (x, gsd@(GeometryStreamData n layoutName _)) = do

        let shaderDeclarations = evalState (declareGeometry (undefined :: VertexFormat a)) 0
            varyings = evalState (enumerateVaryings (undefined :: VertexFormat a)) 0
            varyingCount = length varyings
            bufferMode = GL_INTERLEAVED_ATTRIBS
            io s pName = do
                names <- mapM newCString varyings
                withArray names $ \a -> do
                    glTransformFeedbackVaryings pName (fromIntegral varyingCount) a bufferMode
                mapM_ free names
            topology = toGeometryShaderOutputTopology (undefined :: p)

        tellDrawcall $ makeDrawcall w getTransformFeedbackBuffer sizing topology gsd shaderDeclarations $ do
            declareGeometryLayout layoutName (toLayoutOut (undefined :: p)) maxVertices
            x' <- unS x
            return ()

        modifyRenderIO (\s -> s { transformFeedbackToRenderIO = insert n io (transformFeedbackToRenderIO s) } )

makeDrawcall :: forall a s c ds os. (VertexInput a)
    => Window os c ds
    -> (s -> Buffer os a)
    -> FeedbackBufferSizing
    -> GLuint
    -> GeometryStreamData
    -> GlobDeclM ()
    -> ExprM ()
    -> IO (Drawcall s)
makeDrawcall w getTransformFeedbackBuffer sizing topology (GeometryStreamData geoN _ (PrimitiveStreamData primN ubuff)) shaderDeclarations shader = do
    (gsource, gunis, gsamps, _, prevShaderDeclarations, prevShader) <- runExprM shaderDeclarations shader
    (vsource, vunis, vsamps, vinps, _, _) <- runExprM prevShaderDeclarations prevShader

    let withFeedback env doAsync drawIO = do
            let buffer = getTransformFeedbackBuffer env
            bName <- readIORef (bufName buffer)
            let tfRef = bufTransformFeedback buffer
            tf <- readIORef tfRef
            r <- case tf of
                Just names -> return names
                Nothing -> do
                    r <- case sizing of
                        Fixed -> return (Nothing, Nothing)
                        Stored -> do
                            tfName <- alloca $ \ptr -> do
                                glGenTransformFeedbacks 1 ptr
                                peek ptr
                            mkWeakIORef tfRef (doAsync $ with tfName (\n -> putStrLn ("glDeleteTransformFeedbacks " ++ show n) >> glDeleteTransformFeedbacks 1 n))
                            return (Just tfName, Nothing)
                        Queried -> do
                            tfqName <- alloca $ \ptr -> do
                                glGenQueries 1 ptr
                                peek ptr
                            mkWeakIORef tfRef (doAsync $ with tfqName (\n -> putStrLn ("glDeleteQueries (TF) " ++ show n) >> glDeleteQueries 1 n))
                            return (Nothing, Just tfqName)
                    writeIORef tfRef (Just r)
                    return r
            let capturedDrawIO = do
                    glBindBufferBase GL_TRANSFORM_FEEDBACK_BUFFER 0 bName
                    glBeginTransformFeedback topology
                    glEnable GL_RASTERIZER_DISCARD
                    {-
                    Unsupported rendering feedback loop without this barrier.
                    It doesn't appear to be mandatory on my machine with Linux,
                    but it does on the same machine with Windows 10.
                    -}
                    glTextureBarrier
                    drawIO
                    glDisable GL_RASTERIZER_DISCARD
                    glEndTransformFeedback
            case r of
                (Nothing, Nothing) -> capturedDrawIO
                (Just tfName, Nothing) -> do
                    glBindTransformFeedback GL_TRANSFORM_FEEDBACK tfName
                    capturedDrawIO
                (Nothing, Just tfqName) -> do
                    glBeginQuery GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN tfqName
                    capturedDrawIO
                    glEndQuery GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN

    return $ Drawcall
        (const (Left (getWinName w), return ()))
        (Just withFeedback)
        primN
        (Just geoN)
        vsource (Just gsource) Nothing
        vinps
        vunis vsamps
        gunis gsamps
        [] []
        ubuff
