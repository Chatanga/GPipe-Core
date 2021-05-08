{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.GPipe.Internal.TestExpr where

import Prelude hiding (length, id, (.), (<*))
import Control.Monad
import Data.Boolean

import Test.Framework
    ( TestSuite, assertEqual_, makeLoc, makeTestSuite, makeUnitTest )

import Graphics.GPipe.Internal.Expr

withNewline s = s ++ "\n"

test_while :: IO ()
test_while = do
    let
        total :: VInt
        total = snd $ while
            (\(i, _) -> i <* 3)
            (\(i, n) -> (i + 1, 10 * i + n))
            (0, 1)

        decls = tellGlobalLn "// hello"

        shaderExpr :: ExprM ()
        shaderExpr = void (unS total)

    -- The last two values aren’t meant to be evaluated (will trip over an
    -- undefined value otherwise) since there is no previous stage here.
    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "void main() {"
            , "int t0;"
            , "int t1;"
            , "t0 = 0;" -- i <- 0
            , "t1 = 1;" -- n <- 1
            , "bool t2 = (0<3);"
            , "bool t3 = t2;"
            , "while(t3){"
            , "int t4 = (t0+1);"
            , "int t5 = (10*t0);"
            , "int t6 = (t5+t1);"
            , "t0 = t4;" -- i = i + 1
            , "t1 = t6;" -- n = 10 * i + n
            , "bool t7 = (t4<3);"
            , "t3 = t7;"
            , "}"
            , "}"
            ])
        source

test_tuple_op :: IO ()
test_tuple_op = do
    let
        total :: VInt
        total = let (x, y) = (\(i, n) -> (i + 1, 2 * i + n)) (scalarS STypeInt (useVInput STypeInt 3), 4) in x + y

        decls = tellGlobalLn "// hello"

        shaderExpr :: ExprM ()
        shaderExpr = void (unS total)

    -- The last two values aren’t meant to be evaluated (will trip over an
    -- undefined value otherwise) since there is no previous stage here.
    (source, unis, samps, inps, _, _) <- runExprM decls shaderExpr

    assertEqual
        (concatMap withNewline
            [ "#version 450"
            , "// hello;"
            , "in int in3;"
            , "void main() {"
            , "int t0 = in3;"
            , "int t1 = (t0+1);"
            , "int t2 = (2*t0);"
            , "int t3 = (t2+4);"
            , "int t4 = (t1+t3);"
            , "}"
            ])
        source
