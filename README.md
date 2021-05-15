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

**Unused varying in a GS** If not every varying of a GS are used to compute the emitted vertex, the shader will fail to compile.

**Problematic finalizers** The release of some temporary objects (like VAO) leads to `Access violation in generated code when writing 0x0` in Windows 10. Keeping you Render actions from being garbage collected is the only workaround for now.
