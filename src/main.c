#include "stdint.h"
#include "stdlib.h"
#include "tty.h"
#include "int.h"
#include "lisp.h"
#include "bios.h"

#define LISPIX_VERSION_MINOR 1
#define LISPIX_VERSION_MAJOR 0

void test_malloc() {
  int i;
  char *p, *q;
  for(i = 0; i <  60; i++) {
	p = malloc(0x100000);
	q = malloc(0x100000);
	if(!p || !q) {
	  printf("%d; malloc is broken!\n", i);
	  for(;;);
	}
	free(p);
	free(q);
  }
}


void test_ints() {
  int_bios_regs->al = 'X';
  int_bios_regs->ah = 0x0E;
  int_bios_regs->bx = 0x07;
  int_bios(0x10);
}

void test_disk() {
  uint8_t *p = malloc(0x10000);
  char *t = "abcd";
  disk_select(DISK_HD0);
  disk_write(0, t, 4);
  disk_read(p, 0, 4096);
  print_mem(p, 256);
  //abort();
}

void main() {
  tty_init();

  clear_screen();
  //printf("%d\n", in(0x21));
  int_init(); // init interrupts
  init_stdlib();

  //test_ints();
  test_disk();


  printf("Lispix-v%d.%d ready\n",
		 LISPIX_VERSION_MAJOR, LISPIX_VERSION_MINOR);
  printf("%dMb RAM available\n\n", get_memory_size());

  lisp();

  //print_mem(0x100000, 0x110);

  //printf("in: ");
  //printf("echo: %s\n", gets(b));
  printf("goodbay!\n");
  abort();
}
