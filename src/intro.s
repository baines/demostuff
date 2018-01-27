.include "fn_table.s"
.global _start

.data

shader:
	.incbin "shader.frag"
	.byte 0
	.int dlopen_stub_part1
phdr:

.bss
tmp_space:
	.fill 256, 4

.equ FNTAB_2_SHADER, shader - fn_table
.equ FNTAB_2_PHDR  , phdr   - fn_table

.equ PHDR_VERTS  , 0x18
.equ PHDR_HEIGHT , 0x1f
.equ PHDR_WIDTH  , 0x5b
.equ PHDR_FLOAT  , 0x38
.equ PHDR_GL_FRAG, 0x34

.text

dlopen_stub_part1:
	add $11, -105(%ebx)        # point "dlopen" at func below
	mov 12(%ecx), %ecx         # ecx is our link map, iterate it
	mov 12(%ecx), %eax
	ret

dlopen_stub_part2:
	mov -4(%edi), %eax         # XXX: offset needs to be adjusted based on fn_defs
	mov %eax, -105(%ebx)       # store dlopen symbol
	jmp *%eax                  # call real dlopen

_start:
	xchg %eax, %ecx            # put our link_map* in ecx
	and $-16, %esp
	mov $tmp_space, %ebx
	push %ebx
	mov $fn_loader_data, %esi
	lea FN_LOADER_SIZE(%esi), %edi
	push %edi
	xor %eax, %eax
.Load_lib_loop:
	lodsb                      # eax = hash count
	push %eax                  # num of hashes
	pushl $2                   # RTLD_NOW (dlopen arg 2)
	push %esi                  # lib name (dlopen arg 1)

load_lib:
	call *-105(%ebx)           # dlopen
	mov (%eax), %ebp           # ebp = elf/mem offset
	mov 8(%eax), %esi          # esi = l_ld
.L_dyn_load:
	lodsl                      # eax = dt_tag
	movzxb %al, %ecx           # ecx = dt_tag & 0xff
	lodsl                      # eax = dt_un
	mov %eax, (%ebx, %ecx, 4)  # store dt_un val in tmp table
	inc %ecx                   # silly loop
	loop .L_dyn_load
.L_sym_load:
	mov 24(%ebx), %edx         # edx = sym tab ptr
.L_sym_next:
	mov (%edx), %esi           # esi = name offset
	add 20(%ebx), %esi         # esi = char* name
	mov $0x180f, %cx           # ecx = initial hash val
	xor %eax, %eax
.L_hash_loop:                  # do {
	lodsb                      #   eax = *str++
	imul $-69, %ecx, %ecx      #   ecx = ecx * -69 + *str
	addl %eax, %ecx            #
	test %eax, %eax            # } while (*str != 0)
	jnz .L_hash_loop           #
.L_hash_end:
	add $16, %edx              # edx = next sym tab entry ptr
	xchg %eax, %ecx            # ecx = 0; eax = computed hash
	cmpl %eax, (%edi)          # did we find the hash?
	jnz .L_sym_next            # no  -> loop
	mov -12(%edx), %eax        # yes -> put sym val in eax 
	addl %ebp, %eax            #  add elf/mem offset
	stosl                      #  store in edi, edi += 4
	decl 8(%esp)               # dec hash count
	jnz .L_sym_load            # continue if count != 0

end_load_lib:
	xchg %eax, %ecx            # eax = 0, ecx = hash
	mov %edi, %esi             # esi = fn_table ptr
	pop %edi                   # edi = lib name
	repne scasb                # edi += strlen(lib_name)
	xchg %edi, %esi            # edi = fn_table ptr again, esi = next lib sym count
	pop %ecx                   # free crap on stack (ecx = 2)
	pop %eax                   # eax = still 0
	cmp %al, (%esi)            # if next lib has no syms, exit loop
	jnz .Load_lib_loop

video_init:
#	int $3

	pop %ebp                   # ebp = fn_table
	pop %ebx                   # ebx = tmp_space
	sub $101, %ebx             # ebx = phdr

	push %ecx                  # push $2 = flags (SDL_OPENGL)
	push %eax                  # push $0 = bpp
	pushl PHDR_HEIGHT(%ebx)
	pushl PHDR_WIDTH(%ebx)
	call *_fn_SDL_SetVideoMode(%ebp)

#	push %ebx                  # to re-enable ctrl-c
#	mov $2, %ebx
#	mov $48, %eax
#	xor %ecx, %ecx
#	int $128
#	pop %ebx

	lea FNTAB_2_SHADER(%ebp), %ecx
	push %ecx                  # put shader addr in memory
	push %esp                  # arg2 = char** strings
	push (%ebx)                # arg1 = array count = 1
	pushl PHDR_GL_FRAG(%ebx)   # arg0 = GL_FRAGMENT_SHADER constant
	call *_fn_glCreateShaderProgramv(%ebp)
	xchg %eax, %esi            # esi = shader program

	pop %eax                   # sub esp
	pop %ecx                   # ecx = 1
	lea -8(%esp), %eax         # eax = ptr to store pipeline (top of stack)
	push %eax                  # arg1 = ptr
	push %ecx                  # arg0 = count = 1
	call *_fn_glGenProgramPipelines(%ebp)
	call *_fn_glBindProgramPipeline(%ebp)

	pop %eax                   # eax = pipeline
	pop %edx                   # sub esp
	pop %edx                   # sub esp
	push %esi                  # arg2 = shader program
	pushl $2                   # arg1 = GL_FRAGMENT_SHADER_BIT
	push %eax                  # arg0 = pipeline
	call *_fn_glUseProgramStages(%ebp)

draw_setup:
	lea PHDR_VERTS(%ebx), %eax
	mov %esp, %edi
	stosl
	sub $4, %eax
	stosl
	xor %edi, %edi

draw_loop:
	push %eax
	push %edi
	fld (%esp)
	fadd PHDR_FLOAT(%ebx)
	fstp (%esp)
	mov (%esp), %edi
	push $0      # uniform
	push %esi    # shader program
	call *_fn_glProgramUniform1f(%ebp)
	add $16, %esp
	call *_fn_glRectsv(%ebp)
	call *_fn_SDL_GL_SwapBuffers(%ebp)
#	pushl $16000
#	call *_fn_usleep(%ebp)

	jmp draw_loop	

