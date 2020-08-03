{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}

module Graphics.GL.Ext.EXT.TextureFilterAnisotropic (
  -- * Extension Support
    gl_EXT_texture_filter_anisotropic

  -- * GL_EXT_texture_filter_anisotropic
  , pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
  , pattern GL_TEXTURE_MAX_ANISOTROPY_EXT
) where

import Data.Set

gl_EXT_texture_filter_anisotropic :: Bool
gl_EXT_texture_filter_anisotropic = False

pattern GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FF

pattern GL_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FE
