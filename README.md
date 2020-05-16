## unfinished port of https://learnopengl.com/ example code to CL

Not sure if it actually runs or not with current versions of the
libraries it uses, bug reports/PRs/suggestions welcomed (though might
take a while to do anything about them.)

The `learnopengl.asd` and files under `factored/` were an attempt to
refactor the code into a form that just contained the changes for a
particular example, not sure if that worked out or not. Rest of the
`.lisp` files should be fairly direct translations of the original C
examples (and possibly random junk, since i checked in a bunch of
stuff without looking at it too closely before pushing).

### libraries used

Uses Glop for window/context creation, sb-cga+mathkit for 3d math
library, Opticl for loading textures, and (after the first few)
3bgl-shaders for cl-like shaders. I can't unconditionally recommend
any of those at the moment, but I had to pick something to use :)

Opticl is the closest to an unconditional recommendation, with the
only caveats being that it is written in CL so might be slower than
FFI-based image loading libraries, and doesn't support some of the
formats useful for 3d like `.dds` or `.htx`, or various HDR image
formats.

3bgl-shaders is what I use, but might not be in a good enough state
for normal users. [Varjo](https://github.com/cbaggers/varjo) (possibly
with [shadow](https://github.com/mfiano/shadow) and
[umbra](https://github.com/mfiano/umbra)) is another alternative if
you want lisp-like shader code, or you can just write normal GLSL like
the first few standalone examples, possibly with
[glsl-toolkit](https://github.com/Shirakumo/glsl-toolkit) if you need
to do more than just load strings from files into the API.

There are also lots of other options for math libs, with more active
maintainers, various combinations of performance/features, and
probably a larger library of useful functions.

Glop works well enough for me, though I plan to rewrite/replace it at
some point. Most of the GL code should work fairly easily with glfw or
sdl2, though, if you prefer those.