#ifndef STDLIB_H
#define STDLIB_H

#include <stdarg.h>
#include "common.h"

void abort();

#define assert(c) \
  if(!(c)) { \
	printf("Assertion failled: %s\n", #c); \
	abort(); \
}

void *memset(void *s, int c, size_t n);
void *memcpy(void *dest, const void *src, size_t n);
char *strcpy(char *dst, const char *src);
char* strncpy(char *dest, const char *src, size_t n);
char *strdup(const char *s);
int strlen(const char *p);
int strcmp(const char *a, const char *b);
int atoi(const char *s);

void vsprintf(char *b, const char *fmt, ...);
void vprintf(const char *format, va_list ap);
void vsprintf_helper(char *b, const char *fmt, va_list args);

void print_mem(void *pmem, int size);

int isspace(int c);
int isdigit(int c);
int isalpha(int c);
int isalnum(int c);

void *malloc(size_t size);
void free(void *blk);
void *realloc(void *blk, size_t size);

typedef struct FILE FILE;

FILE *fopen(const char *path, const char *mode);
int fclose(FILE *f);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb,
			  FILE *stream);
int fgetc(FILE *stream);

/* End of file character.
   Some things throughout the library rely on this being -1.  */
#define EOF (-1)

/* The possibilities for the third argument to `fseek'.
   These values should not be changed.  */
#define SEEK_SET        0       /* Seek from beginning of file.  */
#define SEEK_CUR        1       /* Seek from current position.  */
#define SEEK_END        2       /* Seek from end of file.  */
extern FILE *stdin;
extern FILE *stdout;

int init_stdlib();
int get_memory_size();


#endif

