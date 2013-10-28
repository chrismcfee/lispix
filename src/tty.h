#include "common.h"

// user interface
void clear_screen();
void putchar(char c);
void puts(const char *s);
void printf(const char *fmt, ...);

char *gets(char *s);

// internal interface
void tty_init();
void tty_input(int key);
int tty_set_echo(int new_state);
void tty_set_text_color(unsigned int c);

extern int tty_cursor;
extern int tty_attribute;
