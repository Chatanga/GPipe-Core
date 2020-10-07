{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs, TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, Arrows, MultiParamTypeClasses, AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes, AllowAmbiguousTypes #-}

module Graphics.GPipe.Internal.TestUniform where

import Control.Arrow
import Control.Category
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import qualified Data.IntMap as Map
import Prelude hiding (length, id, (.))
import Linear.V2

import Graphics.GPipe.Internal.Buffer
import Graphics.GPipe.Internal.Expr
import Graphics.GPipe.Internal.Uniform

import Test.Framework

withNewline s = s ++ "\n"

test_buildUDecl = do
    -- All types, no padding
    assertEqual
        (concatMap withNewline
            [ "float u0;"
            , "int u4;"
            , "bool u8;"
            , "uint u12;"
            , "whatever u16;"
            , "mat3x2 u20;"
            , "vec2 u24;"
            , "ivec3 u32;"
            , "uvec4 u44;"
            , "bool u60;"
            ])
        (snd . runWriter . buildUDecl $ Map.fromList
            [ (0, STypeFloat)
            , (4, STypeInt)
            , (8, STypeBool)
            , (12, STypeUInt)
            , (16, STypeDyn "whatever")
            , (20, STypeMat 2 3) -- Just 4?
            , (24, STypeVec 2)
            , (32, STypeIVec 3)
            , (44, STypeUVec 4)
            , (60, STypeGenerativeGeometry)
            ])
    -- With padding
    assertEqual
        (concatMap withNewline
            [ "float pad0;"
            , "int u4;"
            , "float pad8;"
            , "int u12;"
            , "float pad16;"
            , "float pad20;"
            , "int u24;"
            , "float pad28;"
            , "float pad32;"
            , "float pad36;"
            , "int u40;"
            ])
        (snd . runWriter . buildUDecl $ Map.fromList
            [ (4, STypeInt)
            , (12, STypeInt)
            , (24, STypeInt)
            , (40, STypeInt)
            ])

test_toUniform = do
    let ToUniform (Kleisli shaderGenF) = toUniform :: ToUniform V (B2 Float) (UniformFormat (B2 Float) V)

        uniAl = 9 :: UniformAlignment
        blockId = 3 :: Int
        sampleBuffer = makeBuffer undefined undefined uniAl :: Buffer os (Uniform (B2 Float))

        fromBUnifom (Uniform b) = b

        shaderGen :: (Int -> ExprM String) -> (UniformFormat (B2 Float) V, OffsetToSType) -- Int is name of uniform block
        shaderGen = runReader $ runWriterT $ shaderGenF $ fromBUnifom $ bufBElement sampleBuffer $ BInput 0 0

        (u, offToStype) = shaderGen (useUniform (buildUDecl offToStype) blockId)

        decls = tellGlobalLn "// hello"

        shaderExpr = mapM unS u >>= \(V2 s1 s2) -> return ()

    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    let uDecl = snd . runWriter . buildUDecl $ offToStype
    assertEqual "vec2 u0;\n" uDecl

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "layout(std140) uniform uBlock3 {"
            , "vec2 u0;"
            , "} u3;"
            , "void main() {"
            , "}"
            ])
        source

    assertEqual ([3] :: [Int]) unis
    assertEqual ([] :: [Int]) samps
    assertEqual [] inps

    return ()
