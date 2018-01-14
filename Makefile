intro: build/intro.o tools/ulink
	tools/ulink -o $@ $<
	du -b $@

src/fn_table.s: src/fn_table.txt tools/fngen
	tools/fngen < $< > $@

build/intro.o: src/intro.s src/fn_table.s src/shader.frag | build
	as -Isrc --32 $< -o $@

tools/ulink: tools/ulink.c
	gcc $< -o $@
	
tools/fngen: tools/fngen.c
	gcc $< -o $@

build:
	mkdir $@

clean:
	$(RM) tools/fngen tools/ulink src/fn_table.s intro

.PHONY: clean
