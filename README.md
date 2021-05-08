# GPipe 2.3.x

- Lot of simple changes to the code: reformatting, variable renaming, commenting...
- Unit tests to ease the exploration of the code.
- Experimental support for geometry shader.
- Experimental support for transform feedback.
- Fixed bug in while loop ([#75](https://github.com/tobbebex/GPipe-Core/issues/75)).

If you use GPipe-GLFW (of course you do!), you will need the latest version (>= 1.4.2.0) in order to require an OpenGL Core Profile >= 4.5.

In the lack of any user level documentation for the changes introduced,
you can have look at my toy project [Hadron](https://github.com/Chatanga/Hadron) which uses both geometry shader and transform feedback.
