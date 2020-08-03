module Graphics.GPipe.GeometryStream (
    -- * The data type
    GeometryStream(),
    GenerativeGeometry(..),

    -- * Creating GeometryStream
    geometrize,
    generateAndRasterize,

    -- * Various GeometryStream operations   
    emitVertex,
    endPrimitive,
)
where

import Graphics.GPipe.Internal.GeometryStream
