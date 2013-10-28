#include "tty.h"
#include "stdlib.h"

void abort() {
	for(;;);
}

static int mem_size;

void probe_mem() {
  uint32_t i;
  volatile uint8_t *p;
  uint8_t t;
  for(i = MEM_HEAP; i < 0xFEC00000; i += 0x100000) {
	p = (volatile uint8_t*)i;
	t = *p;
	*p = ~t;
	if(*p == t) break;
  }
  mem_size = (int)(i / 0x100000);
}

int get_memory_size() {
	return mem_size;
}


static uint8_t *heap;
static uint32_t heap_size;

void heap_init() {
  heap = (uint8_t*)MEM_HEAP;
  heap_size = (uint32_t)get_memory_size()*0x100000;
}


#define HEAP_SIZE heap_size
#define	MALLOC_MAGIC	0x6D92	/* must be < 0x8000 */

typedef struct _malloc		/* Turbo C	DJGPP */
{
	size_t size;		/* 2 bytes	 4 bytes */
	struct _malloc *next;	/* 2 bytes	 4 bytes */
	unsigned magic : 15;	/* 2 bytes total 4 bytes total */
	unsigned used : 1;
} malloc_t;		/* total   6 bytes	12 bytes */

static uint8_t *g_heap_bot=0, *g_kbrk=0, *g_heap_top=0;

/*****************************************************************************
*****************************************************************************/
/*****************************************************************************
POSIX sbrk() looks like this
	void *sbrk(int incr);
Mine is a bit different so I can signal the calling function
if more memory than desired was allocated (e.g. in a system with paging)
If your kbrk()/sbrk() always allocates the amount of memory you ask for,
this code can be easily changed.

			int brk(	void *sbrk(		void *kbrk(
function		 void *adr);	 int delta);		 int *delta);
----------------------	------------	------------		-------------
POSIX?			yes		yes			NO
return value if error	-1		-1			NULL
get break value		.		sbrk(0)			int x=0; kbrk(&x);
set break value to X	brk(X)		sbrk(X - sbrk(0))	int x=X, y=0; kbrk(&x) - kbrk(&y);
enlarge heap by N bytes	.		sbrk(+N)		int x=N; kbrk(&x);
shrink heap by N bytes	.		sbrk(-N)		int x=-N; kbrk(&x);
can you tell if you're
  given more memory
  than you wanted?	no		no			yes
*****************************************************************************/
static void *kbrk(int *delta)
{
	uint8_t *new_brk, *old_brk;

	/* heap doesn't exist yet */
	if(g_heap_bot == NULL)
	{
		g_heap_bot = g_kbrk = heap;
		g_heap_top = g_heap_bot + HEAP_SIZE;
	}
	new_brk = g_kbrk + (*delta);
	/* too low: return NULL */
	if(new_brk < g_heap_bot)
		return NULL;
	/* too high: return NULL */
	if(new_brk >= g_heap_top)
		return NULL;
	/* success: adjust brk value... */
	old_brk = g_kbrk;
	g_kbrk = new_brk;
	/* ...return actual delta... (for this sbrk(), they are the same)
	   (*delta) = (*delta); */
	/* ...return old brk value */
	return old_brk;
}

/*****************************************************************************
malloc() and free() use g_heap_bot, but not g_kbrk nor g_heap_top
*****************************************************************************/
void *malloc(size_t size)
{
  void *r;
  unsigned total_size;
  malloc_t *m, *n;
  int delta;

  if(size&3) size = (size&~3) + 4;

  if(size == 0)
	return NULL;
  total_size = size + sizeof(malloc_t);
  /* search heap for free block (FIRST FIT) */
  m = (malloc_t *)g_heap_bot;
  /* g_heap_bot == 0 == NULL if heap does not yet exist */
  if(m != NULL) {
	if(m->magic != MALLOC_MAGIC)
	  //			panic("kernel heap is corrupt in malloc()");
	  {
		printf("*** kernel heap is corrupt in malloc()\n");
		return NULL;
	  }
	for(;; m = m->next) {
	  if(m->used) {
		if(m->next) continue;
		break;
	  }
	  /* size == m->size is a perfect fit */
	  if(size == m->size) {
		m->used = 1;
	  } else {
		/* otherwise, we need an extra sizeof(malloc_t) bytes for the header
		   of a second, free block */
		if(total_size > m->size) {
		  if(m->next) continue;
		  break;
		}
		/* create a new, smaller free block after this one */
		n = (malloc_t *)((char *)m + total_size);
		n->size = m->size - total_size;
		n->next = m->next;
		n->magic = MALLOC_MAGIC;
		n->used = 0;
		/* reduce the size of this block and mark it used */
		m->size = size;
		m->next = n;
		m->used = 1;
	  }
	  r = (char *)m + sizeof(malloc_t);
	  goto end;
	}
  }
  /* use kbrk() to enlarge (or create!) heap */
  delta = total_size;
  n = kbrk(&delta);
  /* uh-oh */
  if(n == NULL)
	return NULL;
  if(m != NULL)
	m->next = n;
  n->size = size;
  n->magic = MALLOC_MAGIC;
  n->used = 1;
  /* did kbrk() return the exact amount of memory we wanted?
	 cast to make "gcc -Wall -W ..." shut the hell up */
  if((int)total_size == delta) {
	n->next = NULL;
  } else {
	/* it returned more than we wanted (it will never return less):
	   create a new, free block */
	m = (malloc_t *)((char *)n + total_size);
	m->size = delta - total_size - sizeof(malloc_t);
	m->next = NULL;
	m->magic = MALLOC_MAGIC;
	m->used = 0;

	n->next = m;
  }

  r = (char *)n + sizeof(malloc_t);

 end:
  return memset(r, 0, size);
}
/*****************************************************************************
*****************************************************************************/
void free(void *blk) {
  malloc_t *m, *n;

  if(!blk) return;

  /* get address of header */
  m = (malloc_t *)((char *)blk - sizeof(malloc_t));
  if(m->magic != MALLOC_MAGIC)
	//		panic("attempt to free() block at 0x%p "
	//			"with bad magic value", blk);
	{
	  printf("*** attempt to free() block at 0x%x "
			 "with bad magic value\n", blk);
	  return;
	}
  /* find this block in the heap */
  n = (malloc_t *)g_heap_bot;
  if(n->magic != MALLOC_MAGIC)
	//		panic("kernel heap is corrupt in free()");
	{
	  printf("*** kernel heap is corrupt in free()\n");
	  return;
	}
  for(; n != NULL; n = n->next) {
	if(n == m)
	  break;
  }
  /* not found? bad pointer or no heap or something else? */
  if(n == NULL)
	//		panic("attempt to free() block at 0x%p "
	//			"that is not in the heap", blk);
	{
	  printf("*** attempt to free() block at 0x%x "
			 "that is not in the heap\n", blk);
	  return;
	}
  /* free the block */
  m->used = 0;
  /* coalesce adjacent free blocks
	 Hard to spell, hard to do */
  for(m = (malloc_t *)g_heap_bot; m != NULL; m = m->next) {
	while(!m->used && m->next != NULL && !m->next->used) {
	  /* resize this block */
	  m->size += sizeof(malloc_t) + m->next->size;
	  /* merge with next block */
	  m->next = m->next->next;
	}
  }
}
/*****************************************************************************
*****************************************************************************/
void *realloc(void *blk, size_t size)
{
	void *new_blk;
	malloc_t *m;

/* size == 0: free block */
	if(size == 0)
	{
		if(blk != NULL)
			free(blk);
		new_blk = NULL;
	}
	else
	{
/* allocate new block */
		new_blk = malloc(size);
/* if allocation OK, and if old block exists, copy old block to new */
		if(new_blk != NULL && blk != NULL)
		{
			m = (malloc_t *)((char *)blk - sizeof(malloc_t));
			if(m->magic != MALLOC_MAGIC)
//				panic("attempt to krealloc() block at "
//					"0x%p with bad magic value", blk);
			{
				printf("*** attempt to realloc() block at "
					"0x%p with bad magic value\n", blk);
				return NULL;
			}
/* copy minimum of old and new block sizes */
			if(size > m->size)
				size = m->size;
			memcpy(new_blk, blk, size);
/* free the old block */
			free(blk);
		}
	}
	return new_blk;
}



void *memset(void *s, int c, size_t n) {
  uint8_t *p = s;
  int i;
  for(i = 0; i < n; i++)
	p[i] = (uint8_t)c;

  return s;
}

void *memcpy(void *dst, const void *src, size_t n) {
  uint8_t *d = dst;
  const uint8_t *s = src;
  while(n--) *d++ = *s++;
  return dst;
}

char *strcpy(char *dst, const char *src) {
  char *r = dst;
  while((*dst++ = *src++));
  return r;
}

char* strncpy(char *dest, const char *src, size_t n) {
  size_t i;

  for(i = 0 ; i < n && src[i] != '\0'; i++) dest[i] = src[i];
  for( ; i < n ; i++) dest[i] = '\0';

  return dest;
}

int strlen(const char *p) {
  const char *q = p;
  while(*p) p++;
  return p - q;
}

char *strdup(const char *s) {
  return strcpy(malloc(strlen(s)+1), s);
}

int strcmp(const char *a, const char *b) {
  while(*a == *b && *a) {
	a++;
	b++;
  }
  return (int)(uint8_t)*a - (int)(uint8_t)*b;
}

int atoi(const char *s) {
  int r = 0;
  int neg;

  if(*s == '-') {
	neg = 1;
	s++;
  } else {
	neg = 0;
  }

  while(isdigit(*s)) {
	r = r*10 + (*s - '0');
	s++;
  }

  if(neg) r = -r;

  return r;
}

static void reverse(char *s) {
	char *p = s;
	while(*p) p++;
	while(s < --p) {
		int t = *s;
		*s++ = *p;
		*p = t;
	}
}

static char *puthex_digit(char *b, unsigned char digit) {
	char table[]="0123456789ABCDEF";
	*b++ = table[digit];
	return b;
}

static char *puthex_tetra(char *b, unsigned char byte) {
	b = puthex_digit(b, byte >> 4);
	b = puthex_digit(b, byte & 0x0F);
	return b;
}

static char *puthex(char *b, unsigned int dword) {
	b = puthex_tetra(b, (dword & 0xFF000000) >>24);
	b = puthex_tetra(b, (dword & 0x00FF0000) >>16);
	b = puthex_tetra(b, (dword & 0x0000FF00) >>8);
	b = puthex_tetra(b, (dword & 0x000000FF));
	return b;
}

static char *int2string(char *b, int v, int base, int unsign) {
	char *p = b;
	unsigned int x;
	int sign = 0;

	if(v < 0 && !unsign) {
		sign = 1;
		x = -v;
		*p++ = '-';
	} else {
		if(v < 0) x = -v;
		else x = v;
	}

	do {
		unsigned int c = x % base;
		x /= base;
		if(c < 10) c += '0';
		else {
			c -= 10;
			c += 'A';
		}
		*p++ = c;
	} while(x);
	*p = 0;
	reverse(b+sign);
	return p;
}

void vsprintf_helper(char *b, const char *fmt, va_list args) {
	for( ; *fmt; fmt++) {
		if(*fmt != '%') {
			*b++ = *fmt;
			continue;
		}
		switch(*++fmt) {
		case 's': {
			char *p = va_arg(args, char *);
			while((*b++ = *p++));
			b--;
			break;}

		case 'c':
			*b++ = va_arg(args, unsigned int);
			break;

		case 'i': case 'd':
			b = int2string(b, va_arg(args, unsigned int), 10, 0);
			break;
	
		case 'u':
			b = int2string(b, va_arg(args, unsigned int), 10, 1);

		case 'x': case 'X':
			b = puthex(b, va_arg(args, unsigned int));
			break;

		case 'z':
		  tty_set_text_color(va_arg(args,unsigned int));
		  break;
		}
	}
	*b = 0;
}

static char d2c[] = "0123456789ABCDEF";
void print_mem(void *q, int s) {
  int i = 0;
  uint8_t *p = q;

  while(i < s) {
	if(!(i&0xf)) {
	  if(i) printf("\n");
	  printf("%x: ", i);
	}

	printf("%c%c ", d2c[*p>>4], d2c[*p & 0xf]);
	p++;
	i++;
  }
  printf("\n");
}

void vsprintf(char *b, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vsprintf_helper(b, fmt, args);
	va_end(args);
}

void vprintf(const char *fmt, va_list ap) {
  char b[256];
  vsprintf_helper(b, fmt, ap);
  printf(b);
}

int isspace(int c) {
  return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

int isdigit(int c) {
  return '0' <= c && c <= '9';
}

int isalpha(int c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}

int isalnum(int c) {
  return isalpha(c) || isdigit(c);
}


#define FBUFSZ 512

#define F_INVALID 0
#define F_KEYBOARD 1

struct FILE {
  int type;
  char buf[FBUFSZ]; // buffer
  int bp; // buffer pointer
  int bt; // buufer top
};

FILE *stdin;
FILE *stdout;

FILE *fopen(const char *path, const char *mode) {
  return 0;
}

int fclose(FILE *f) {
  return 0;
}


static int read_kb(void *dst, int s, FILE *f) {
  uint8_t *p = dst;
  while(s) {
	int x = f->bt - f->bp;
	if(!x) {
	  gets(f->buf);
	  f->bp = 0;
	  f->bt = strlen(f->buf) + 1;
	  f->buf[f->bt-1] = '\n';
	  continue;
	} else {
	  if(x > s) x = s;
	  s -= x;
	  while(x--) *p++ = f->buf[f->bp++];
	}
  }
  return p - (uint8_t*)dst;
}

size_t fread(void *p, size_t size, size_t nmemb, FILE *f) {
  if(f == stdin) return read_kb(p, size*nmemb, f);

  return 0;
}


size_t fwrite(const void *p, size_t size, size_t nmemb, FILE *f) {
  return 0;
}

int fgetc(FILE *f) {
  uint8_t r;
  fread(&r, 1, 1, f);
  return r;
}


static void finit() {
  stdin = malloc(sizeof(FILE));
  stdout = malloc(sizeof(FILE));
}

int init_stdlib() {
  probe_mem();
  heap_init();
  finit();
  return 0;
}

