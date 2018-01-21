## Demoscene related stuff for GNU/Linux.

tools/fngen.c:
	- Function table generator for import-by-hash code to use.
	
tools/ulink.c:
	- Minimal linker to generate small ELFs.
		- (with some intro-specific constants hardcoded into the program header...)

src/fn_table.txt:
	- List of functions to import by hash, input to fngen.
	
src/intro.s:
	- The actual intro code.

src/shader.frag:
	- Fragment shader used by the intro.

Run make and you should get a 740 byte ELF that looks like this:
![screenshot](/anim.gif)

## TODO:
	- music and stuff
	- learn how to make snazzier shaders
	- compress the shader code somehow?
	- use the shell script self-extract trick?
