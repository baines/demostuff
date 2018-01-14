#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <elf.h>
#include <unistd.h>
#include "stb_sb.h"

////////////////////////////////////////

enum {
	SECT_TEXT,
	SECT_DATA,
	SECT_BSS,

	SECT_COUNT,
};

struct reloc {
	uint32_t sym;
	uint32_t offset;
	uint8_t  type;
};

struct section {
	uint8_t* mem;
	size_t   mem_size;
	sb(struct reloc) relocs;

	uint32_t out_offset;
	uint32_t out_virt;
};

struct objfile {
	struct section  sects[SECT_COUNT];
	char*           sh_strtab;
	char*           strtab;
	Elf32_Sym*      symtab;
	size_t          nsyms;
	Elf32_Shdr*     shdrs;
	struct objfile* next;
};

struct global_sym {
	struct objfile*    file;
	const char*        name;
	int                sect_index;  // -1 undefined
	uint32_t           value;
	struct global_sym* next;
};

////////////////////////////////////////

struct global_sym* global_sym_list;
struct objfile*    objfile_list;

////////////////////////////////////////

static const char out_interp[] = "/lib/ld-linux.so.2\0libc.so.6";


#define BASE_ADDR 0x400000

#define PHDR_COUNT 3

#define EHDR_SIZE     (offsetof(Elf32_Ehdr, e_shentsize))
//#define EHDR_SIZE     (sizeof(Elf32_Ehdr))
#define PHDR_SIZE     (PHDR_COUNT*sizeof(Elf32_Phdr))
#define DHDR_SIZE     (24)
#define DVAL_SIZE     (5)
#define INTERP_SIZE   (19)

#define FILE_OFFSET_TO_ELF        (0)
#define FILE_OFFSET_TO_INTERP     (FILE_OFFSET_TO_ELF      + EHDR_SIZE)
#define FILE_OFFSET_TO_CODE       (FILE_OFFSET_TO_INTERP   + sizeof(out_interp))
#define FILE_OFFSET_TO_PHDR(sz)   (FILE_OFFSET_TO_CODE     + (sz))
#define FILE_OFFSET_TO_DYN(sz)    (FILE_OFFSET_TO_PHDR(sz) + PHDR_SIZE - 12)

#define ELF_CRAP_BETWEEN_DATA_BSS_SIZE (PHDR_SIZE + DVAL_SIZE)

////////////////////////////////////////

void write_elf(int fd, uint32_t start_addr, uint8_t* code, size_t code_size){

	Elf32_Ehdr h = {
		.e_ident = {
			[EI_MAG0]    = ELFMAG0,
			[EI_MAG1]    = ELFMAG1,
			[EI_MAG2]    = ELFMAG2,
			[EI_MAG3]    = ELFMAG3,
			[EI_CLASS]   = ELFCLASS32,
			[EI_DATA]    = ELFDATA2LSB,
			[EI_VERSION] = EV_CURRENT,
		},
		.e_type      = ET_EXEC,
		.e_machine   = EM_386,
		.e_version   = EV_CURRENT,
		.e_entry     = start_addr,
		.e_phoff     = FILE_OFFSET_TO_PHDR(code_size),
		.e_ehsize    = sizeof(h),
		.e_phentsize = sizeof(Elf32_Phdr),
		.e_phnum     = PHDR_COUNT,
	};

	//memcpy((char*)&h.e_ident + 8, "abaines", 8);

	Elf32_Phdr ps[] = {
		{
			.p_type   = PT_LOAD,
			.p_offset = EHDR_SIZE,
			.p_vaddr  = EHDR_SIZE + BASE_ADDR,
			.p_paddr  = EHDR_SIZE + BASE_ADDR,
			.p_filesz = code_size + EHDR_SIZE + PHDR_SIZE + DVAL_SIZE + sizeof(out_interp),
			.p_memsz  = 0x00100010,
			.p_flags  = 0xffffffff,//PF_R | PF_W | PF_X,
			.p_align  = 0x1000,
		},
		{
			.p_type   = PT_INTERP,
			.p_offset = FILE_OFFSET_TO_INTERP,
			.p_vaddr  = FILE_OFFSET_TO_INTERP + BASE_ADDR,
			.p_paddr  = FILE_OFFSET_TO_INTERP + BASE_ADDR,
			.p_filesz = INTERP_SIZE,
			.p_memsz  = 0x8b30,
			.p_flags  = PF_R | PF_W | 0x3c23d70a,
		},
		{
			.p_type   = PT_DYNAMIC,
			.p_offset = FILE_OFFSET_TO_DYN(code_size),
			.p_vaddr  = FILE_OFFSET_TO_DYN(code_size) + BASE_ADDR,
			.p_paddr  = FILE_OFFSET_TO_DYN(code_size) + BASE_ADDR,
			.p_filesz = DHDR_SIZE,
			.p_memsz  = 6,
			.p_flags  = PF_R | PF_W,
			.p_align  = 5,
		},
	};

	uint32_t dyn[] = {
		FILE_OFFSET_TO_INTERP + BASE_ADDR + 19, DT_NEEDED,
	};

#define WRITE(fd, name, sz) ({\
	off_t s = lseek(fd, 0, SEEK_CUR);\
	printf("%02lx - %02lx: " #name, s, s+sz);\
	for(uint8_t* c = (uint8_t*)name; c < (uint8_t*)name + sz; ++c){\
		if(((c - (uint8_t*)name) % 16) == 0) printf("\n ");\
		printf(" %02x", *c);\
	}\
	puts("");\
	write(fd, name, sz); })

	WRITE(fd, &h, EHDR_SIZE);
	WRITE(fd, out_interp, sizeof(out_interp));
	WRITE(fd, code, code_size);
	WRITE(fd, ps, PHDR_SIZE);
	WRITE(fd, dyn, DVAL_SIZE);
}

int sect_name_to_id(const char* name){
	if(strcmp(name, ".text") == 0) return SECT_TEXT;
	if(strcmp(name, ".data") == 0) return SECT_DATA;
	if(strcmp(name, ".bss") == 0) return SECT_BSS;
	return -1;
}

void global_sym_process(struct objfile* obj, Elf32_Sym* sym){
	assert(ELF32_ST_TYPE(sym->st_info) != STT_SECTION);

	const char* name = obj->strtab + sym->st_name;

	if(sym->st_shndx == SHN_UNDEF){
		printf("undef sym [%s]\n", name);
	} else {
		assert(sym->st_shndx != SHN_ABS);

		printf("global sym [%s] [%x]\n", name, sym->st_value);

		struct global_sym* g;
		{
			struct global_sym** list = &global_sym_list;
			while(*list) list = &(*list)->next;
			*list = calloc(1, sizeof(**list));
			g = *list;
		}

		g->file = obj;
		g->name = name;
		g->sect_index = sym->st_shndx;
		g->value = sym->st_value;
	}
}

uint32_t global_sym_get(const char* name){

	for(struct global_sym* sym = global_sym_list; sym; sym = sym->next){
		if(strcmp(name, sym->name) == 0 && sym->sect_index != -1){
			const char* sect_name = sym->file->sh_strtab + sym->file->shdrs[sym->sect_index].sh_name;

			int id;
			if((id = sect_name_to_id(sect_name)) != -1){
				return sym->value + sym->file->sects[id].out_virt;
			} else {
				printf("sect [%s] [%d]\n", sect_name, sym->sect_index);
				assert(!"sym with bogus sect index");
			}
		}
	}

	assert(!"global_sym_get fail");
	return 0;
}

void get_relocs(int fd, struct section* s, Elf32_Shdr* rel_header){
	for(size_t i = 0; i < rel_header->sh_size; i += rel_header->sh_entsize){
		Elf32_Rel rel;
		pread(fd, &rel, sizeof(rel), rel_header->sh_offset + i);

		struct reloc r = {
			.offset = rel.r_offset,
			.type   = ELF32_R_TYPE(rel.r_info),
			.sym    = ELF32_R_SYM(rel.r_info),
		};

		printf("add reloc %d, %d, %d\n", r.offset, r.type, r.sym);
		sb_push(s->relocs, r);
	}
}

void do_reloc(uint8_t* mem, struct objfile* obj, struct section* s, struct reloc* r){
	Elf32_Sym* sym = obj->symtab + r->sym;

	printf("do_reloc: [%ld] [%#x]\n", s - obj->sects, s->out_virt);

	uint32_t val = 0;
	if(ELF32_ST_TYPE(sym->st_info) == STT_SECTION){
		const char* sectname = obj->sh_strtab + obj->shdrs[sym->st_shndx].sh_name;
		int id;

		printf("  reloc sect [%s]\n", sectname);
		if((id = sect_name_to_id(sectname)) != -1){
			val = objfile_list->sects[id].out_virt;
		} else {
			printf("  sect = [%s]\n", sectname);
			assert(!"can't reloc unknown section");
		}
	} else {
		val = global_sym_get(obj->strtab + sym->st_name);
	}

	switch(r->type){
		case R_386_32: {
			uint32_t* p = (uint32_t*)(mem + s->out_offset + r->offset);
			printf("  reloc off: %#x val: [%#x -> %#x]\n", s->out_offset + r->offset, *p, *p + val);
			*p += val;
		} break;

		default: {
			printf("reloc type = %d\n", r->type);
			assert(!"reloc type nyi");
		}
	}
}

void objfile_load(const char* filename){
	int fd = open(filename, O_RDONLY);
	assert(fd != -1);

	struct objfile* obj;
	{
		struct objfile** l = &objfile_list;
		while(*l) l = &(*l)->next;
		*l = calloc(1, sizeof(**l));
		obj = *l;
	}

	Elf32_Ehdr header;
	assert(pread(fd, &header, sizeof(header), 0) == sizeof(header));

	{
		uint8_t* i = header.e_ident;
		assert( strncmp(i, ELFMAG, SELFMAG) == 0 );

		assert( i[EI_CLASS]   == ELFCLASS32  );
		assert( i[EI_DATA]    == ELFDATA2LSB );
		assert( i[EI_VERSION] == EV_CURRENT  );
	}

	assert( header.e_type    == ET_REL     );
	assert( header.e_machine == EM_386     );
	assert( header.e_version == EV_CURRENT );

#if 0
	printf("Info: [%s]\n", filename);
	printf("  Entry point: %#x\n", header.e_entry);
	printf("  Prog offset: %#x\n", header.e_phoff);
	printf("  Sect offset: %#x\n", header.e_shoff);
	printf("        Flags: %#x\n", header.e_flags);
	printf("     Hdr size: %#x\n", header.e_ehsize);
	printf("    PHdr size: %#x\n", header.e_phentsize);
	printf("   PHdr count: %#x\n", header.e_phnum);
	printf("    SHdr size: %#x\n", header.e_shentsize);
	printf("   SHdr count: %#x\n", header.e_shnum);
	printf("   SH Str idx: %#x\n", header.e_shstrndx);
#endif

	// shstrtab
	{
		Elf32_Shdr sect = {};
		pread(fd, &sect, sizeof(sect), header.e_shoff + header.e_shstrndx * sizeof(sect));
		obj->sh_strtab = malloc(sect.sh_size);
		pread(fd, obj->sh_strtab, sect.sh_size, sect.sh_offset);
	}

	obj->shdrs = calloc(header.e_shnum, sizeof(Elf32_Shdr));

	for(int i = 0; i < header.e_shnum; ++i){
		Elf32_Shdr* sect = obj->shdrs + i;
		pread(fd, sect, sizeof(*sect), header.e_shoff + i * sizeof(*sect));
	}

	for(int i = 0; i < header.e_shnum; ++i){
		Elf32_Shdr* sect = obj->shdrs + i;

		const char* name = obj->sh_strtab + sect->sh_name;
		//printf("sect named [%s]\n", name);

		if(sect->sh_type == SHT_REL){
			Elf32_Shdr* who = obj->shdrs + sect->sh_info;

			int id;
			if((id = sect_name_to_id(obj->sh_strtab + who->sh_name)) != -1){
				get_relocs(fd, obj->sects + id, sect);
			}

			continue;
		}

		if(sect->sh_type == SHT_SYMTAB){
			assert(!obj->symtab);
			obj->nsyms = sect->sh_size / 0x10;
			obj->symtab = malloc(sect->sh_size);
			pread(fd, obj->symtab, sect->sh_size, sect->sh_offset);

			continue;
		}

		if(sect->sh_type == SHT_STRTAB && i != header.e_shstrndx){
			assert(!obj->strtab);
			obj->strtab = malloc(sect->sh_size);
			pread(fd, obj->strtab, sect->sh_size, sect->sh_offset);

			continue;
		}

		int id;
		if((id = sect_name_to_id(obj->sh_strtab + sect->sh_name)) != -1){
			struct section* s = obj->sects + id;

			s->mem_size = sect->sh_size;

			if(sect->sh_type != SHT_NOBITS){
				s->mem = malloc(s->mem_size);
				pread(fd, s->mem, s->mem_size, sect->sh_offset);
			}
		}
	}

	// global syms
	for(size_t i = 0; i < obj->nsyms; ++i){
		Elf32_Sym* sym = obj->symtab + i;

		if(ELF32_ST_BIND(sym->st_info) == STB_GLOBAL){
			global_sym_process(obj, sym);
		}
	}

	close(fd);
}

int main(int argc, char** argv){
	const char* outname = "output.elf";

	int opt;
	while((opt = getopt(argc, argv, "o:")) != -1){
		if(opt != 'o') continue;
		outname = optarg;
	}

	if(optind >= argc){
		fprintf(stderr, "Usage: %s [object files...]\n", argv[0]);
		return 1;
	}

	argc -= (optind-1);
	argv += (optind-1);

	for(int i = 1; i < argc; ++i){
		objfile_load(argv[i]);
	}
	
	uint32_t virt = BASE_ADDR + FILE_OFFSET_TO_CODE;
	printf("o2c: %#lx, virt: %#x\n", FILE_OFFSET_TO_CODE, virt);

	sb(char) out_mem = NULL;

	for(int i = SECT_TEXT; i < SECT_COUNT; ++i){

		if(i == SECT_BSS){
			virt += ELF_CRAP_BETWEEN_DATA_BSS_SIZE;
		}

		for(struct objfile* obj = objfile_list; obj; obj = obj->next){
			struct section* s = obj->sects + i;

			s->out_offset = sb_count(out_mem);
			s->out_virt   = virt;

			virt += s->mem_size;

			if(s->mem){
				memcpy(sb_add(out_mem, s->mem_size), s->mem, s->mem_size);
			}
		}
	}

	// process relocs

	for(int i = SECT_TEXT; i < SECT_COUNT; ++i){
		for(struct objfile* obj = objfile_list; obj; obj = obj->next){
			struct section* s = obj->sects + i;

			sb_each(r, s->relocs){
				do_reloc(out_mem, obj, s, r);
			}
		}
	}

	uint32_t start = global_sym_get("_start");
	printf("_start @ %#x\n", start);

	// write out file
	int fd = open(outname, O_WRONLY | O_TRUNC | O_CREAT, 00777);
	if(fd == -1){
		perror("open");
		return -1;
	}

	write_elf(fd, start, out_mem, sb_count(out_mem));

	close(fd);

	return 0;
}
