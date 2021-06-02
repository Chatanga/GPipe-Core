# GPipe 2.3.x

- Lot of simple changes to the code: reformatting, variable renaming, commenting...
- Unit tests to ease the exploration of the code.
- Experimental support for geometry shader.
- Experimental support for transform feedback.
- Shader bitwise operator.
- Instanced rendering in 3D textures.
- Fixed bug in while loop ([#75](https://github.com/tobbebex/GPipe-Core/issues/75)).

If you use [GPipe-GLFW](https://github.com/plredmond/GPipe-GLFW) (of course you do!), you will need the latest version (>= 1.4.2.0) in order to require an OpenGL Core Profile >= 4.5.

In the lack of any user level documentation for the changes introduced,
you can have a look at my toy project [Hadron](https://github.com/Chatanga/Hadron) which uses both geometry shader and transform feedback.

## Known issues

_(In addition to the unresolved bugs from the original code base: [#50](https://github.com/tobbebex/GPipe-Core/issues/50), [#78](https://github.com/tobbebex/GPipe-Core/issues/78) and [#82](https://github.com/tobbebex/GPipe-Core/issues/82).)_

**Unused varying in a GS**: if not every varying of a GS are used to compute the emitted vertex, the shader will fail to compile.

## Future

The work done in this fork is not intended to be published back into the original GPipe project. Some fixes and additions could (provided the GPipe's maintainer gives any sign of life), but the main bulk, GS (Geometry Shader) and TF (Transform Feedback), is not the proper way to do things today. Indeed, in post modern OpenGL, GS and TF are seen as transitory and have been essentially replaced by CS (Compute Shader). A newer "GPipe-Compute" project alongside the current GPipe-Core would be more appropriate.