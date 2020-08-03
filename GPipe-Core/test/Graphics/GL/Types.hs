{-# LANGUAGE CPP #-}

module Graphics.GL.Types (
    GLbitfield
  , GLboolean
  , GLbyte
  , GLchar
  , GLcharARB
  , GLclampd
  , GLclampf
  , GLclampx
  , GLdouble
  , GLeglImageOES
  , GLenum
  , GLfixed
  , GLfloat
  , GLhalf
  , GLhalfARB
  , GLhalfNV
  , GLhandleARB
  , GLint
  , GLint64
  , GLint64EXT
  , GLintptr
  , GLintptrARB
  , GLshort
  , GLsizei
  , GLsizeiptr
  , GLsizeiptrARB
  , GLsync
  , GLubyte
  , GLuint
  , GLuint64
  , GLuint64EXT
  , GLushort
  , GLvdpauSurfaceNV
) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
-- import Numeric.Fixed
-- import Numeric.Half

type Fixed = Int
type Half = Int

type GLbitfield       = Word32
type GLboolean        = Word8
type GLbyte           = Int8
type GLchar           = CChar
type GLcharARB        = CChar
type GLclampd         = Double
type GLclampf         = Float
type GLclampx         = Int32
type GLdouble         = Double
type GLeglImageOES    = Ptr ()
type GLenum           = Word32
type GLfixed          = Fixed
type GLfloat          = Float
type GLhalf           = Half
type GLhalfARB        = Half
type GLhalfNV         = Half
type GLint            = Int32
type GLint64          = Int64
type GLint64EXT       = Int64
type GLintptr         = CPtrdiff
type GLintptrARB      = CPtrdiff
type GLshort          = Int16
type GLsizei          = Int32
type GLsizeiptr       = CPtrdiff
type GLsizeiptrARB    = CPtrdiff
type GLsync           = Ptr ()
type GLubyte          = Word8
type GLuint           = Word32
type GLuint64         = Word64
type GLuint64EXT      = Word64
type GLushort         = Word16
type GLvdpauSurfaceNV = CPtrdiff
type GLhandleARB      = Word32
