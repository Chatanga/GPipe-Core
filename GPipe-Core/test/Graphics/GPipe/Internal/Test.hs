{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Graphics.GPipe.Internal.Test where

import Data.Function ((&))

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Category
import Control.Arrow
import Control.Monad (void)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.IntMap.Lazy (insert)
import Prelude hiding (length, id, (.))
import System.IO

import Graphics.GPipe.Internal.Context
import Graphics.GPipe.Internal.Format
import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.PrimitiveArray
import Graphics.GPipe.Internal.PrimitiveStream
import Graphics.GPipe.Internal.FragmentStream
import Graphics.GPipe.Internal.FrameBuffer
import Graphics.GPipe.Internal.GeometryStream
import Graphics.GPipe.Internal.Shader
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Uniform
import Graphics.GPipe.Internal.Texture
import Graphics.GPipe.Internal.Orphans
import Linear
import Data.Boolean

import Data.List
import qualified Debug.Trace as Trace


traceIt :: Show a => String -> a -> a
traceIt t a = Trace.trace (t ++ " = " ++ show a) a
-- traceIt _ = id

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as
--traceList _ = id


data MyContext = MockContext

data MyContextHandlerParameters = MockContextHandlerParameters

data MyContextWindow = MockContextWindow

data MyWindowParameters = MockWindowParameters

-- | Class implementing a window handler that can create openGL contexts, such as GLFW or GLUT
instance ContextHandler MyContext where

    -- | Implementation specific context handler parameters, eg error handling and event processing policies
    data ContextHandlerParameters MyContext = MyContextHandlerParameters

    -- | Implementation specific window type
    type ContextWindow MyContext = MyContextWindow

    -- | Implementation specific window parameters, eg initial size and border decoration
    type WindowParameters MyContext = MyWindowParameters

    -- | Create a context handler. Called from the main thread
    -- contextHandlerCreate :: ContextHandlerParameters ctx -> IO ctx
    contextHandlerCreate _ = return MockContext

    -- | Delete the context handler. All contexts created from this handler will be deleted using contextDelete prior to calling this.
    -- contextHandlerDelete :: ctx -> IO ()
    contextHandlerDelete _ = return ()

    -- | Create a new context sharing all other contexts created by this ContextHandler. If the parameter is Nothing,
    --   a hidden off-screen context is created, otherwise creates a window with the provided window bits and implementation specific parameters.
    --   Only ever called from the mainthread (i.e. the thread that called contextHandlerCreate).
    -- createContext :: ctx -> Maybe (WindowBits, WindowParameters ctx) -> IO (ContextWindow ctx)
    createContext _ _ = return MockContextWindow

    -- | Run an OpenGL IO action in this context, that doesn't return any value to the caller. This may be run after contextDelete or contextHandlerDelete has been called.
    --   The thread calling this may not be the same creating the context (for finalizers it is most definetly not).
    --   May also be called on previously deleted windows in the case of finalizers.
    -- contextDoAsync :: ctx -> Maybe (ContextWindow ctx) -> IO () -> IO ()
    contextDoAsync _ _ action = action -- => Boom !

    -- | Swap the front and back buffers in the context's default frame buffer.
    --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate').
    --   Never called on deleted windows.
    -- contextSwap :: ctx -> ContextWindow ctx -> IO ()
    contextSwap _ _ = return ()

    -- | Get the current size of the context's default framebuffer (which may change if the window is resized).
    --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate')
    -- contextFrameBufferSize :: ctx -> ContextWindow ctx -> IO (Int, Int)
    contextFrameBufferSize _ _ = return (0, 0)

    -- | Delete a context and close any associated window.
    --   Only ever called from the mainthread (i.e. the thread that called 'contextHandlerCreate'). Only ever called once per window,
    --   and will always be called for each window before the context is deleted with 'contextHandlerDelete'.
    -- contextDelete :: ctx -> ContextWindow ctx -> IO ()
    contextDelete _ _ = return ()

data ShaderEnvironment = ShaderEnvironment
    {   primitives :: PrimitiveArray Triangles (B2 Float)
    ,   rasterOptions :: (Side, ViewPort, DepthRange)
    }

getZ (V4 _ _ z _) = z

exploreIt :: IO ()
exploreIt = runContextT MyContextHandlerParameters $ do
    
    window :: Window os RGBFloat Depth <- newWindow (WindowFormatColorDepth SRGB8 Depth16) MockWindowParameters

    -- Create vertex data buffers
    positions :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]

    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

    -- Create the shader
    shader :: CompiledShader os ShaderEnvironment <- compileShader $ do

        vertices :: PrimitiveStream Triangles (V3 VFloat) <- fmap (\(V2 x y) -> (V3 x y 1)) <$> toPrimitiveStream primitives
        modelViewProj :: V4 (V4 VFloat) <- getUniform (const (uniform, 0))
        let projectedVertices :: PrimitiveStream Triangles (V4 VFloat, ()) = (\(V3 px py pz) -> (modelViewProj !* V4 px py pz 1, ())) <$> vertices

        frags :: FragmentStream () <- rasterize rasterOptions projectedVertices
        let fragsWithDepth :: FragmentStream (V3 (S F Float), FFloat) = withRasterizedInfo (\_ info -> (V3 1 1 1, getZ (rasterizedFragCoord info))) frags

        let colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) fragsWithDepth

    -- Render the frame and present the results
    render $ do
        clearWindowColor window 0
        clearWindowDepth window 1
        cube :: PrimitiveArray Triangles (B2 Float) <- toPrimitiveArray TriangleStrip <$> newVertexArray positions
        shader $ ShaderEnvironment cube (FrontAndBack, ViewPort 320 200, DepthRange 0 1)

    swapWindowBuffers window

exploreIt2 :: IO ()
exploreIt2 = do
    hSetBuffering stdout NoBuffering
    runContextT MyContextHandlerParameters $ do

        window :: Window os RGBFloat Depth <- newWindow (WindowFormatColorDepth SRGB8 Depth16) MockWindowParameters

        uniform :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

        -- Create the shader
        shader :: CompiledShader os ShaderEnvironment <- compileShader $ do

            vertices :: PrimitiveStream Triangles (V3 VFloat) <- fmap (\(V2 x y) -> (V3 x y 1)) <$> toPrimitiveStream primitives
            modelViewProj :: V4 (V4 VFloat) <- getUniform (const (uniform, 0))
            let projectedVertices :: PrimitiveStream Triangles (VPos, V3 VFloat) = (\(V3 px py pz) -> (modelViewProj !* V4 px py pz 1, V3 1 2 3)) <$> vertices

            geometries :: GeometryStream (Geometry Triangles (VPos, V3 VFloat)) <- geometrize projectedVertices
            let
                makePrimitive :: GGenerativeGeometry Triangles (VPos, V3 VFloat) -> Geometry Triangles (VPos, V3 VFloat) -> GGenerativeGeometry Triangles (VPos, V3 VFloat)
                makePrimitive g (Triangle p1 p2 p3) = g
                    & emitVertex (fst p1, V3 4 5 6)
                    & emitVertex (fst p2, V3 1 2 3)
                    & emitVertex (fst p3, V3 7 8 9)
                    & endPrimitive
                expandedGeometries :: GeometryStream (GGenerativeGeometry Triangles (VPos, V3 VFloat)) = makePrimitive generativeTriangleStrip <$> geometries 

            frags :: FragmentStream (FragmentFormat (V3 VFloat)) <- generateAndRasterize rasterOptions 3 expandedGeometries
            
            -- frags :: FragmentStream (FragmentFormat (V3 VFloat)) <- rasterize rasterOptions projectedVertices

            let fragsWithDepth :: FragmentStream (FragColor RGBFloat, FragDepth) = withRasterizedInfo (\(V3 r g b) info -> ((V3 b g r) * 2, getZ (rasterizedFragCoord info))) frags

            let colorOption = ContextColorOption NoBlending (pure True)
                depthOption = DepthOption Less True

            drawWindowColorDepth (const (window, colorOption, depthOption)) fragsWithDepth

        return ()

exploreIt3 :: IO ()
exploreIt3 = do
    hSetBuffering stdout NoBuffering
    runContextT MyContextHandlerParameters $ do

        window :: Window os RGBFloat Depth <- newWindow (WindowFormatColorDepth SRGB8 Depth16) MockWindowParameters

        uniform :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

        -- Create the shader
        shader :: CompiledShader os ShaderEnvironment <- compileShader $ do

            vertices :: PrimitiveStream Triangles (V3 VFloat) <- fmap (\(V2 x y) -> (V3 x y 1)) <$> toPrimitiveStream primitives
            modelViewProj :: V4 (V4 VFloat) <- getUniform (const (uniform, 0))
            let projectedVertices :: PrimitiveStream Triangles VPos = (\(V3 px py pz) -> modelViewProj !* V4 px py pz 1) <$> vertices

            geometries :: GeometryStream (Geometry Triangles VPos) <- geometrize projectedVertices
            let
                makePrimitive :: Int -> Geometry Triangles VPos -> GGenerativeGeometry Triangles (VPos, V3 VFloat) -> GGenerativeGeometry Triangles (VPos, V3 VFloat)
                makePrimitive depth (Triangle p1 p2 p3) g =
                    let depth' = depth - 1
                        p12 = (p1 + p2) / 2
                        p23 = (p2 + p3) / 2
                        p31 = (p3 + p1) / 2
                    in  if depth >* 0
                        then (g
                            & makePrimitive depth' (Triangle p1 p12 p31)
                            & makePrimitive depth' (Triangle p12 p2 p23)
                            & makePrimitive depth' (Triangle p31 p23 p3)
                            & makePrimitive depth' (Triangle p12 p23 p31))
                        else (g
                            & emitVertex (p1, V3 1 0 0)
                            & emitVertex (p2, V3 0 1 0)
                            & emitVertex (p3, V3 0 0 1)
                            & endPrimitive)
                depth = 1
                expandedGeometries :: GeometryStream (GGenerativeGeometry Triangles (VPos, V3 VFloat)) = (\t -> makePrimitive depth t generativeTriangleStrip) <$> geometries 

            frags :: FragmentStream (FragmentFormat (V3 VFloat)) <- generateAndRasterize rasterOptions (3 * 4 ^ depth) expandedGeometries

            let fragsWithDepth :: FragmentStream (FragColor RGBFloat, FragDepth) = withRasterizedInfo (\(V3 r g b) info -> ((V3 b g r) * 2, getZ (rasterizedFragCoord info))) frags

            let colorOption = ContextColorOption NoBlending (pure True)
                depthOption = DepthOption Less True

            drawWindowColorDepth (const (window, colorOption, depthOption)) fragsWithDepth

        return ()

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

testIt = do
    let expr = do
        b <- tellAssignment STypeInt (return "truc")
        let a = scalarS STypeInt (useVInput STypeInt 42)
        let b = scalarS' "33"
        let c = vec4S (STypeVec 4) (return "1234")
        unS $ bin STypeInt "+" a b
        unS $ fun1 STypeInt "abracadara" (fromVec4 c)
        return ()
    (source, unis, samps, inps, decls, ss) <- runExprM (return ()) expr
    putStrLn source

testIt2 = do
    let shader = do
        -- ifThenElse (S $ return "condition" :: VBool) id id (S $ return "i" :: VInt) :: VInt
        -- ifThenElse (S $ return "condition" :: S G Bool) (\i -> i + i) id (S $ return "i") :: S G Int
        let x = scalarS' "33" :: S V Int
        let i = vec4S (STypeVec 4) (return "1234")
        ifThenElse (S $ return "condition" :: S V Bool) (\i -> bin (STypeVec 2) "+" i i) id x -- (vec2S STypeInt (useVInput (STypeVec 2) 42))
    (source, unis, samps, inps, decls, ss) <- runExprM (return ()) (void $ unS shader)
    putStrLn source

-- ifThenElse :: forall a b x. (ShaderType a x, ShaderType b x) => S x Bool -> (a -> b) -> (a -> b) -> a -> b

playWithIt :: IO ()
playWithIt = do
    let
        ToVertex _ (Kleisli mf) _ = toVertex :: ToVertex (B2 Float) (VertexFormat (B2 Float))
        (V2 x y, (n, uSize, offToStype)) = runReader
            (runStateT (mf (error "boom")) (11, 22, mempty))
            (error "kaboom") -- (useUniform (buildUDecl offToStype) 3)
        globalDeclM = return mempty
    (source, unis, samps, inps, decls, ss) <- runExprM globalDeclM (void $ traceIt "ignored" <$> (unS y >> unS x))
    putStrLn "--------"
    putStrLn source
    putStrLn $ show unis
    putStrLn $ show samps
    putStrLn $ show inps
    -- putStrLn $ show decls -- Related to globalDeclM?
    putStrLn "--------"
    putStrLn $ show n
    putStrLn $ show uSize
    putStrLn $ show $ fmap stypeName offToStype
    putStrLn "--------"
