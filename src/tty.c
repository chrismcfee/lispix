#define VIDEO_WIDTH 80
#define VIDEO_HEIGHT 25
#define VIDEO_RAM 0xb8000

#include <stdarg.h>
#include "stdlib.h"
#include "tty.h"
#include "ports.h"

int tty_cursor;
int tty_attribute;

// Miscellaneous Output Register
#define VGA_MOR_R_PORT  0x3c2
#define VGA_MOR_W_PORT  0x3cc

void tty_set_cursor(int p);

static int vga_crt_in(int index) {
  out(0x03D4, index);
  return in(0x03D5);
}


static void vga_crt_out(int index, int data) {
  out(0x03D4, index);
  out(0x03D5, data);
}

void tty_set_text_color(unsigned int c) {
	tty_attribute = c;
}

void clear_screen() {
	char *video = (char*) VIDEO_RAM;
	int i;
	for (i = 0; i < VIDEO_HEIGHT*VIDEO_WIDTH; i++) {
		*(video + i*2) = ' ';
		*(video + i*2 + 1) = 0x07;
	}
	tty_cursor = 0;
}

void putchar(char c) {
	char *video = (char*) VIDEO_RAM;
	int i;
	switch (c) {
	case '\n':
		tty_cursor+=VIDEO_WIDTH;
		tty_cursor-=tty_cursor%VIDEO_WIDTH;
		break;
	default:
		*(video + tty_cursor*2) = c;
		*(video + tty_cursor*2+1) = tty_attribute;
		tty_cursor++;
		break;
	}
	// do we need to shift screen horizontally?
	if (tty_cursor>VIDEO_WIDTH*VIDEO_HEIGHT) {
		for (i=VIDEO_WIDTH*2;i<=VIDEO_WIDTH*VIDEO_HEIGHT*2+VIDEO_WIDTH*2;i++) {
			*(video+i-VIDEO_WIDTH*2)=*(video+i);
		}
		tty_cursor-=VIDEO_WIDTH;
	}
	tty_set_cursor(tty_cursor);
}

void puts(const char *s) {
	while(*s) putchar(*s++);
}

void printf(const char *fmt, ...) {
  char b[512];
  va_list args;
  va_start(args, fmt);

  tty_set_text_color(0xf);

  vsprintf_helper(b, fmt, args);

  va_end(args);
  puts(b);
}

#define IGNORE 0
#define READ_STRING 1
#define READ_CHAR 2

int input_state;
char *input_buffer;
int input_ptr;
int input_echo;


void tty_input(int key) {
  if(input_state == IGNORE) return;

  if(key == 8) { //backspace
	if(input_ptr) {
	  char *video = (char*) VIDEO_RAM;
	  tty_cursor--;
	  input_ptr--;
	  *(video + tty_cursor*2) = ' ';
	  tty_set_cursor(tty_cursor);
	}
    return;
  }

  if(input_echo)
	putchar(key); // echo it to screen

  if(input_state == READ_CHAR) {
	input_buffer[input_ptr++] = key;
	input_state = IGNORE;
  } else if(key == '\n') {
	input_buffer[input_ptr++] = 0;
	input_state = IGNORE;
  } else {
	input_buffer[input_ptr++] = key;
  }
}

int tty_set_echo(int new_state) {
  int r = input_echo;
  input_echo = new_state;
  return r;
}

char *gets(char *s) {
  input_ptr = 0;
  input_buffer = s;
  input_state = READ_STRING;
  while(input_state);
  return s;
}

void tty_enable_color_adapter() {
  int mor = in(VGA_MOR_R_PORT);
  mor |= 1; // set I/OAS bit
  out(VGA_MOR_W_PORT, mor);
}

void tty_set_cursor(int p) {
  vga_crt_out(0xE, (p>>8)&0xff);
  vga_crt_out(0xF, p&0xff);  
}

void tty_enable_cursor() {
  int csr = vga_crt_in(0xA);
  csr &= ~020;
  vga_crt_out(0xA, csr);
}

void tty_disable_cursor() {
  int csr = vga_crt_in(0xA);
  csr |= 020;
  vga_crt_out(0xA, csr);
}

void tty_init() {
  // We don't need monochreome compat
  tty_enable_color_adapter();

  // Cursor usually left enabled by BIOS...
  //tty_disable_cursor();

  input_state = IGNORE;
  tty_set_echo(1);
  tty_cursor = 0;
  tty_attribute = 10;

  tty_set_cursor(tty_cursor);

}
