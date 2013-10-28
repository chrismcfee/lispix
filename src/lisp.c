/* #include <stdio.h> */
/* #include <stdlib.h> */
/* #include <stdint.h> */
/* #include <stdarg.h> */
/* #include <string.h> */
/* #include <ctype.h> */
/* #include <assert.h> */

#include <stdarg.h>
#include "stdint.h"
#include "stdlib.h"
#include "tty.h"

#include "lambda.h"


typedef int8_t s1;
typedef int16_t s2;
typedef int32_t s4;
typedef int64_t s8;

typedef uint8_t u1;
typedef uint16_t u2;
typedef uint32_t u4;
typedef uint64_t u8;

typedef uintptr_t uiptr;

typedef unsigned int uint;

typedef enum {false, true} bool;

typedef void *tp; // typed-pointer

tp fixnum(int n) {
  return (tp)((n) < 0 ?
	      ((uiptr)-(n) << 2) | 3 :
	      ((uiptr)(n) << 2) | 1);
}

int fixnum2int(tp n) {
  return (int)((uiptr)(n) & 2 ?
	       -(intptr_t)((uiptr)(n) >> 2) :
	       (intptr_t)((uiptr)(n) >> 2));
}

#define FIXNUM_MAX (intptr_t)(~(uiptr)3 >> 2)
#define FIXNUM_MIN -(intptr_t)(~(uiptr)3 >> 2)


enum {
  BOUND_SYMBOL,     // these two...
  UNBOUND_SYMBOL,   // should go first.
  PAIR,
  FUNCT,
  AREF,             // on-stack argument reference
  STRING,
  VECTOR,
  HASH,
  STREAM,
  QUOTED,
  QUASIQUOTED,
  EVALED,
  EVALED_LIST,
  FIXNUM
};

typedef struct {
  tp car, cdr;
} Pair;

typedef struct {
  tp value;
} Typed;

typedef struct {
  int offset;
  tp name;
} Aref;

typedef struct {
  tp name, value, type;
} Symbol;

typedef struct {
  int len;
  char *ptr;
} String;

typedef struct {
  int len;
  tp *ptr;
} Vector;

typedef struct {
  bool string_stream;
  String *string;
  int pos;
  FILE *strm;
  int pb_sp, pb_stack[50]; // pushback stack
} Stream;

typedef struct {
  bool native;
  bool macro;
  int nargs;
  int nreq; // number of required args
  bool rest;
  tp name;
  tp env;
  union {
    tp body;
    tp (*ptr)(tp);
  };
} Funct;


#define INVALID_NREF 0xffffff

int get_nref(tp p) {return *((u4*)p-1) & 0xffffff;}
int get_type(tp p) {return *((u4*)p-1) >> 24;}

tp set_nref(tp p, u4 new) {
  *((u4*)p-1) = (u4)new | (*((u4*)p-1) & ~0xffffff);
  return p;
}

tp set_type(tp p, u4 new) {
  *((u4*)p-1) = get_nref(p) | new<<24;
  return p;
}

#define MAX_OBJECTS 30000
int objects_in_use;
tp alloc_bytes(int type, int size) {
  if(objects_in_use == MAX_OBJECTS) {
    printf("Out of memory\n");
    abort();
  }
  ++objects_in_use;
  u4 *p = malloc(size+sizeof(u4));
  *p = (u4)type << 24;
  return p+1;
}

#define alloc(type, type_struct) \
  (type_struct*)alloc_bytes(type, sizeof(type_struct))

void reclaim(void *p) {
  --objects_in_use;
  free((u4*)p - 1);
}

enum {
  CMD_EVAL,
  CMD_PREPARE_FUNCALL,
  CMD_FUNCALL,
  CMD_SAVE,
  CMD_SAVE_EXPANDED,
  CMD_SETQ,
  CMD_IF,
  CMD_FRAME,
  CMD_POP_ARGS,
  CMD_ERROR,
  CMD_ERROR_HANDLER, // error handler
  CMD_END
};


// special symbols
u1 nil[sizeof(Symbol)+sizeof(u4)];
#define nil ((tp)((u4*)nil+1))
tp sym_t, eof;
tp stdin_stream, stdout_stream;

tp Required, Optional, Rest, Invalid, Lambda;

tp eval_last;

// lexical environment
tp env;

// reader state
tp reader_source;
char *reader_data;
int reader_pos;
int reader_end;
bool reader_bq; // inside back-quote
int (*read_char_cb)();
int last_ch;
int next_ch;
int tok;
#define MAX_LEX (512)
char lex[MAX_LEX];
int lex_len;
tp reader_last; // last readed elem

tp cons(tp car, tp cdr);
tp atom(tp type, void *p);
tp atom_dat(tp type, tp dat);
tp preserve(tp p);
void release(tp p);
bool atom_p(tp p);
bool pair_p(tp p);
bool symbol_p(tp p);
tp string(char *str);
tp eval(tp p);
int type(tp p);

void pp(tp p);
void fatal(char *msg, ...);
void err(char *msg, ...);

#define vdef(x) tp x;
#define let(x,v,body) macr tp x = preserve(v); tp let_ret = macr body endm; release(x); let_ret; endm
#define set(x,v) macr tp _t = preserve(v); release(x); x = _t; endm

#define iter(p) ((p) = cdr(p))
#define loopr(elem,list, body) macr			  \
  tp loop_ret = nil, elem, loop_ptr = (list);		  \
  for(; pair_p(loop_ptr); iter(loop_ptr))		  \
    {elem = car(loop_ptr); loop_ret = macr body endm; }	  \
  loop_ret; endm

#define loop(elem,list, body) do {			  \
    tp elem, loop_ptr = (list);				  \
    for(; pair_p(loop_ptr); iter(loop_ptr))		  \
      {elem = car(loop_ptr); { body } }			  \
  } while(0)

#define choice(v, body) macr tp case_r; switch(v) {body} case_r; endm
#define incase(c, body) case c: case_r = macr body endm; break;
#define otherwise(body) default: case_r = macr body endm; break;

#define push(elem, list) set((list),cons((elem),(list)))
#define pop(list) set(list, cdr(list))

#define with(x,y) {tp *x_ptr = &(x), saved_x = preserve(x); set(x,y);{
#define endw }set(*x_ptr,saved_x); release(saved_x);}


// Jenkins One-at-a-time hash
u4 hash(void *data, int data_len) {
  u1 *p = (u1*)data;
  u4 hash = 0;
  int i;
 
  for (i = 0; i < data_len; i++) {
    hash += p[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}
u4 hash_cstr(char *p) {
  return hash(p, strlen(p));
}


bool fixnum_p(tp p) {return (uiptr)p & 1;}
bool datum_p(tp p)  {return !fixnum_p(p);}
int type(tp p)      {return fixnum_p(p) ? FIXNUM : get_type(p);}
bool pair_p(tp p)   {return type(p) == PAIR;}
bool atom_p(tp p)   {return type(p) != PAIR;}
bool null_p(tp p)   {return p == nil;}

int nref(tp p) {return fixnum_p(p) ? INVALID_NREF : get_nref(p);}
tp car(Pair *p) {return pair_p(p) ? p->car : nil;}
tp cdr(Pair *p) {return pair_p(p) ? p->cdr : nil;}

tp preserve(tp p) {
  int nr = nref(p);
  if(nr != INVALID_NREF) set_nref(p, nr+1);
  return p;
}

tp cons(tp car, tp cdr) {
  Pair *p = alloc(PAIR, Pair);
  p->car = preserve(car);
  p->cdr = preserve(cdr);
  return p;
}

tp rplaca(Pair *p, tp new) {
  if(pair_p(p)) set(p->car, new);
  return p;
}

tp rplacd(Pair *p, tp new) {
  if(pair_p(p)) set(p->cdr, new);
  return p;
}

tp typed(int type, tp value) {
  Typed *p = alloc(type, Typed);
  p->value = preserve(value);
  return p;
}

tp typed_value(Typed *p) {
  return p->value;;
}

int length(tp l) {
  int r = 0;
  while(l) r++, iter(l);
  return r;
}

tp reverse(tp l) {
  return loopr(e, l, cons(e, loop_ret););
}

tp list(int dummy, ...) {
  va_list ap;
  tp arg, p = nil, r = nil;
  va_start(ap,dummy);
  while((arg = va_arg(ap, tp)) != Invalid) {
    if(null_p(r)) r = p = cons(arg, nil);
    else p = cdr(rplacd(p, cons(arg, nil)));
  }
  va_end(ap);
  return r;
}

#define list(...) list(0, __VA_ARGS__, Invalid)

tp symbol(tp name) {
  Symbol *s = alloc(UNBOUND_SYMBOL, Symbol);
  s->name = preserve(name);
  s->value = nil;
  return s;
}
void release_symbol(Symbol *s) {
  release(s->name);
  release(s->value);
  reclaim(s);
}

bool symbol_p(tp p) { return type(p) <= UNBOUND_SYMBOL; }
tp symbol_name(Symbol *s) { return s->name; }
tp symbol_value(Symbol *s) { return s->value; }
tp symbol_type(Symbol *s) { return s->type; }
tp symbol_set_type(Symbol *s, tp t) {s->type = t;return s;}
tp symbol_set_value(Symbol *s, tp v) {
  set_type(s, BOUND_SYMBOL);
  set(s->value, v);
  return s;
}

tp funct_name(Funct *f) { return f->name; }
void funct_set_name(Funct *f, tp new) { set(f->name, new); }
bool funct_macro(Funct *f) { return f->macro; }
bool funct_native(Funct *f) { return f->native; }
tp funct_ptr(Funct *f) { return f->ptr; }
void *funct_body(Funct *f) { return f->body; }

tp quote(tp d) { return typed(QUOTED, d); }
tp quasiquote(tp d) { return typed(QUASIQUOTED, d); }

tp string(char *str) {
  String *s = alloc(STRING, String);
  s->len = strlen(str);
  s->ptr = strdup(str);

  return s;
}
void string_release(String *s) {
  free(s->ptr);
  reclaim(s);
}
int string_len(String *s) { return s->len; }
char *string_ptr(String *s) { return s->ptr; }
u4 string_hash(tp s) { return hash(string_ptr(s), string_len(s)); }
bool string_eql(tp a, tp b) {return !strcmp(string_ptr(a), string_ptr(b)); }

tp vector(int nelems) {
  int i;
  Vector *v = alloc(VECTOR, Vector);
  v->len = nelems;
  v->ptr = malloc(nelems*sizeof(tp));
  for(i = 0; i < v->len; i++) v->ptr[i] = nil;
  return v;
}
void vector_release(Vector *v) {
  int i;
  for(i = 0; i < v->len; i++) release(v->ptr[i]);
  free(v->ptr);
  reclaim(v);
}
int vector_len(Vector *p) { return p->len; }
tp vector_elt(Vector *p, int index) {
  return index < vector_len(p) ? p->ptr[index] : nil;
}
tp vector_set(Vector *p, int index, tp new) {
  if(index < vector_len(p)) set(p->ptr[index], new);
  return p;
}

bool atoms_eql(tp a, tp b) {
  if(type(a) != type(b)) return false;

  switch(type(a)) {
  case STRING:   return string_eql(a, b);
  }
  return a == b;
}

bool number_p(tp n) {
  return type(n) == FIXNUM;
}

tp aref(tp name, int offset) {
  Aref *r = alloc(AREF, Aref);
  r->name = preserve(name);
  r->offset = offset;
  return r;
}

tp aref_name(Aref *ar) {
  return ar->name;
}

int aref_offset(Aref *ar) {
  return ar->offset;
}

tp stream(FILE *f) {
  Stream *s;
  if(!f) return nil;
  s = alloc(STREAM, Stream);
  s->strm = f;
  s->string_stream = false;
  s->pb_sp = 0;
  return s;
}

void stream_release(Stream *s) {
  if(s->string_stream) release(s->string);
  else if(s->strm != stdin && s->strm != stdout) fclose(s->strm);
  reclaim(s);
}

tp string_stream(tp string) {
  Stream *s = alloc(STREAM, Stream);
  s->string = preserve(string);
  s->string_stream = true;
  return s;
}

int stream_read_byte(tp p) {
  Stream *s = p;
  if(s->pb_sp) return s->pb_stack[--s->pb_sp];
  if(s->string_stream) {
    if(s->pos == string_len(s->string)) return EOF;
    return string_ptr(s->string)[s->pos++];
  }
  return fgetc(s->strm);
}

void stream_push_back(tp p, int ch) {
  Stream *s = p;
  s->pb_stack[s->pb_sp++] = ch;
}


void release(tp p) {
  int nr = nref(p);
  if(nr == INVALID_NREF) return;
  if(nr > 1) {
    set_nref(p, nr-1);
    return;
  }
  set_nref(p, INVALID_NREF); // so we wont mess loop-ed graphs

  switch(type(p)) {
  case PAIR: {
    Pair *pair = p;
    release(pair->car);
    release(pair->cdr);
    reclaim(pair);
    break;}

  case BOUND_SYMBOL:
  case UNBOUND_SYMBOL:
    release_symbol(p);
    break;

  case QUOTED:      case QUASIQUOTED: case EVALED:
  case EVALED_LIST: {
    Typed *e = p;
    release(e->value);
    reclaim(e);
    break;}

  case FUNCT: {
    Funct *f = p;
    release(f->name);
    if(!f->native) {
      release(f->body);
      release(f->env);
    }
    reclaim(f);
    break;}

  case STREAM: stream_release(p); break;
  case STRING: string_release(p); break;
  case VECTOR: vector_release(p); break;

  case AREF: {
    Aref *r = p;
    release(r->name);
    reclaim(r);
    break;}
  }
}


/// ****************************************************************
/// Symbol Table Stuff  ********************************************
/// ****************************************************************
#define STBL_INIT_SIZE 509
tp stbl;
void stbl_init() {
  stbl = vector(STBL_INIT_SIZE);
}

tp stbl_get_nil_diff(char *name) {
  loop(e, vector_elt(stbl, hash_cstr(name)%vector_len(stbl)),
       if(!strcmp(string_ptr(symbol_name(e)), name)) return e;
  );
  return 0;
}

tp stbl_get(char *name) {
  tp r = stbl_get_nil_diff(name);
  return r ? r : nil;
}

tp stbl_add(char *name) {
  u4 index = hash_cstr(name) % vector_len(stbl);
  tp s = symbol(string(name));
  vector_set(stbl, index, cons(s, vector_elt(stbl, index)));
  return s;
}

tp stbl_get_or_add(char *name) {
  tp s = stbl_get_nil_diff(name);
  return s ? s : stbl_add(name);
}

tp stbl_set(char *name, tp value) {
  return symbol_set_value(stbl_get_or_add(name), value);
}

tp stbl_add_sym(tp s) {
  u4 index = string_hash(symbol_name(s)) % vector_len(stbl);
  vector_set(stbl, index, cons(s, vector_elt(stbl, index)));
  return s;

}

void stbl_clear() {
  int i, end =  vector_len(stbl);
  for(i = 0; i < end; i++)
    loop(e, vector_elt(stbl, i),
		 symbol_set_value(e, nil););
}


int read_char() {
  last_ch = next_ch;
  if(last_ch == EOF) return last_ch;
  next_ch = stream_read_byte(reader_source);
  return last_ch;
}

enum {
  TOK_SYMBOL,
  TOK_STRING,
  TOK_NUMBER,
  TOK_EOF,
  TOK_ERROR
};

int advance_token() {
  lex_len = 0;

 start:
  while(isspace(next_ch)) read_char();

  switch(next_ch) {
  case ';':
    do { read_char(); } while(next_ch != '\n' && next_ch != EOF);
    goto start;

  case '\"':
    tok = TOK_STRING;
    read_char();
    while(read_char() != '\"') {
      if(last_ch == '\\') read_char();
      if(last_ch == EOF) {
		err("EOF in string");
		return TOK_ERROR;
      }
      lex[lex_len++] = last_ch;
    }
    break;

  case ',':
    tok = read_char();
    if(next_ch == '@') tok = read_char();
    break;

  case EOF:
    tok = TOK_EOF;
    break;

  case '(': case ')': case '\'': case '`': case '.':
    tok = read_char();
    break;

  default:
    if(isdigit(next_ch)) {
      tok = TOK_NUMBER;
      do { lex[lex_len++] = read_char(); } while(isalnum(next_ch));
      break;
    }

    tok = TOK_SYMBOL;
    for(;;) {
      switch(next_ch) {
      case '\\':
		read_char();
		if(next_ch == EOF) {
		  err("EOF in symbol");
		  return TOK_ERROR;
		}
		break;

      case '|':
		read_char();
		while(read_char() != '|') {
		  if(last_ch == EOF) {
			err("EOF in \"|...|\" bars");
			return TOK_ERROR;
		  }
		  lex[lex_len++] = last_ch;
		}
		goto break_loop;

      case ';': case '(': case ')': case '\'': case EOF:
		goto break_loop;

      default:
		if(isspace(next_ch)) 
		  goto break_loop;
      }
      lex[lex_len++] = read_char();
    }
  break_loop:;

  }
  lex[lex_len] = 0;
  return tok;
}

tp read_list();

tp read_elem() {
  tp r;

  switch(tok) {
  case ')':
    err("Unexpected: \')\'");
    return nil;

  case '(':
    advance_token();
    r = read_list();
    assert(tok == ')');
    break;

  case '\'':
    advance_token();
    return quote(read_elem());

  case '`': {
    bool bq_save = reader_bq;
    reader_bq = true;
    advance_token();
    r = quasiquote(read_elem());
    reader_bq = bq_save;
    break;
  }

  case '@':
    if(!reader_bq) {
      err("coma not inside a quasiquote");
      return nil;
    }
    advance_token();
    return typed(EVALED_LIST, read_elem());

  case ',':
    if(!reader_bq) {
      err("coma not inside a quasiquote");
      return nil;
    }
    advance_token();
    return typed(EVALED, read_elem());

  case TOK_SYMBOL: r = stbl_get_or_add(lex); break;
  case TOK_STRING: r = string(lex); break;
  case TOK_NUMBER: r = fixnum(atoi(lex)); break;
  case TOK_EOF: r = eof; break;
  case TOK_ERROR: return nil;

  default:
    assert(false);
  }
  return r;
}

tp read_list() {
  tp elem;

  if(tok == ')' || tok == TOK_EOF || tok == TOK_ERROR) return nil;

  elem = read_elem();
  advance_token();
  return cons(elem, read_list());
}

tp read(tp strm) {
  reader_source = strm;
  next_ch = 0;
  read_char(); // get look-ahead
  advance_token();
  set(reader_last, read_elem());
  stream_push_back(reader_source, next_ch); // save look-ahead char
  return reader_last;
}

void print_pair(tp c);
void print_atom(tp a);

void print(tp p) {
  if(atom_p(p)) print_atom(p);
  else {
    printf("(");
    print_pair(p);
    printf(")");
  }
}

void print_atom(tp a) {
  switch(type(a)) {
  case BOUND_SYMBOL:
  case UNBOUND_SYMBOL:
    printf("%s", string_ptr(symbol_name(a)));
    break;

  case AREF:
    printf("%s", string_ptr(aref_name(a)));
    break;

  case FUNCT:
    if(funct_macro(a)) printf("#<macro:");
    else printf("#<function:");
    if(null_p(funct_name(a))) printf(" anonymous") ;
    else print(funct_name(a));
    if(funct_native(a)) printf(" #x%x", (uiptr)funct_ptr(a));
    else printf(" #x%x", (uiptr)funct_body(a));
    printf(">");
    break;

  case QUOTED:
    printf("\'");
    print(((Typed*)a)->value);
    break;

  case QUASIQUOTED:
    printf("`");
    print(((Typed*)a)->value);
    break;

  case EVALED:
    printf(",");
    print(((Typed*)a)->value);
    break;

  case EVALED_LIST:
    printf(",@");
    print(((Typed*)a)->value);
    break;

  case STRING:
    printf("\"%s\"", string_ptr(a));
    break;

  case FIXNUM:
    printf("%d", fixnum2int(a));
    break;

  case VECTOR:
    printf("#<vector>");
    break;

  case STREAM:
    printf("#<stream>");
    break;

  default:
    printf("#<invalid>");
  }
}

void print_pair(tp p) {
  for(;;) {
    print(car(p));
    if(atom_p(cdr(p))) { // dotted pair?
      if(!null_p(cdr(p))) {
	printf(" . ");
	print(cdr(p));
      }
      break;
    }
    printf(" ");
    iter(p);
  }
}

#define ARG_STACK_SIZE 1000
tp args1st;
tp args2nd[ARG_STACK_SIZE];
int args2nd_sp;

#define CMD_STACK_SIZE 1000
struct {
  int type;
  tp arg;
  int prev_sp;
  tp env;
} cmd_stack[1000];
int cmd_sp;

void cmd_push(int type, tp arg) {
  cmd_stack[cmd_sp].type = type;
  cmd_stack[cmd_sp].arg = preserve(arg);
  ++cmd_sp;
}

void cmd_push_frame(tp arg, int sp, tp env) {
  cmd_push(CMD_FRAME, arg);
  cmd_stack[cmd_sp-1].prev_sp = sp;
  cmd_stack[cmd_sp-1].env = preserve(env);
}

void cmd_push_pop_args(int sp) {
  cmd_push(CMD_POP_ARGS, nil);
  cmd_stack[cmd_sp-1].prev_sp = sp;
}

void cmd_save(tp v) {
  rplaca(args1st, cons(v, car(args1st)));
}

void cmd_save_expanded(tp l) {
  if(!pair_p(l)) return;
  cmd_save_expanded(cdr(l));
  cmd_save(car(l));
}

void fatal(char *msg, ...) {
  va_list ap;
  va_start(ap, msg);
  printf("Fatal: "); vprintf(msg, ap); printf("\n");
  va_end(ap);
  abort();
}

void err(char *msg, ...) {
  va_list ap;
  va_start(ap, msg);
  printf("Error: "); vprintf(msg, ap); printf("\n");
  va_end(ap);
  cmd_push(CMD_ERROR, nil);
}

void push_arg(tp a) {
  if(args2nd_sp == ARG_STACK_SIZE) fatal("Argument stack overflow");
  args2nd[args2nd_sp++] = preserve(a);
}

void progn(tp prog) {
  if(pair_p(prog)) {
    progn(cdr(prog));
    cmd_push(CMD_EVAL, car(prog));
  }
}

void pop_args(int prev_sp) {
  while(args2nd_sp > prev_sp)
    release(args2nd[--args2nd_sp]);
}

void prepare_funcall(Funct *f, tp args) {
  int npassed = 0;

  if(type(f) != FUNCT) {
    err("Illegal function call");
    return;
  }

  if(f->native) {
    cmd_push_frame(f, args2nd_sp, env);
    cmd_push(CMD_FUNCALL, f);
  } else {
    int i;
    // kludge!
    for(i = 1; ; i++) {
      if(cmd_stack[cmd_sp-i].type != CMD_FRAME) {
		cmd_push_frame(f, args2nd_sp, env);
		cmd_push(CMD_FUNCALL, f);
		break;
      }
      if(cmd_stack[cmd_sp-i].arg == f) { // tail-call?
		int new_cmd_sp = cmd_sp-i+1;
		int prev_sp = cmd_stack[cmd_sp-i].prev_sp;
		while(cmd_sp != new_cmd_sp) {
		  --cmd_sp;
		  pop_args(cmd_stack[cmd_sp].prev_sp);
		  set(env, cmd_stack[cmd_sp].env);
		  release(cmd_stack[cmd_sp].env);
		  release(cmd_stack[cmd_sp].arg);
		}
		cmd_push(CMD_FUNCALL, f);
		cmd_push_pop_args(prev_sp);
		break;
      }
    }
  }

  if(f->macro) {
    tp q;
    push(args, args1st); // pass as is
    for(q = args; !null_p(q); iter(q)) {
      if(!pair_p(q)) {
		err("Invalid atom in argument list cdr's place");
		return;
      }
      npassed++;
    }
  } else {
    push(nil, args1st);
    for(; !null_p(args); iter(args)) {
      if(!pair_p(args)) {
		err("Invalid atom in argument list cdr's place");
		return;
      }
      cmd_push(CMD_SAVE, nil);
      cmd_push(CMD_EVAL, car(args));
      npassed++;
    }
  }

  if(npassed < f->nreq || (npassed > f->nreq && !f->rest))
    err("Wrong number of arguments to function");
}

tp funcall(Funct *f, tp args) {
  tp r, p, q;

  if(f->native) {
    r = f->ptr(args);
  } else {
    set(env, f->env);

    // push args
    push(nil, env);
    for(p = car(cdr(env)), q = args;  !null_p(p);  iter(p), iter(q)) {
      tp e = car(p);
      tp t = symbol_type(e);
      tp arg;
      if(t == Rest) {
	arg = q;
      } else if(t == Optional) {
	arg = car(q); // shouldn't we use default value?
      } else {
	arg = car(q);
      }
      rplaca(env, cons(symbol_set_value(symbol(symbol_name(e)), arg), car(env)));
      push_arg(arg);
    }

    progn(f->body);
    r = nil;
  }

  return r;
}

tp eval(tp a) {
  switch(type(a)) {
  default: return a; // self-evaluating

  case BOUND_SYMBOL: return symbol_value(a);

  case PAIR:
    cmd_push(CMD_PREPARE_FUNCALL, cdr(a));
    cmd_push(CMD_EVAL, car(a));
    return nil;

  case AREF: return args2nd[args2nd_sp + aref_offset(a)];
  case QUOTED: return typed_value(a);

  case QUASIQUOTED:
    push(nil, args1st);
    cmd_push(CMD_FUNCALL, symbol_value(stbl_get("list")));
    loop(e, typed_value(a),
	 if(type(e) == EVALED_LIST) {
	   cmd_push(CMD_SAVE_EXPANDED, nil);
	   cmd_push(CMD_EVAL, typed_value(e));
	 } else {
	   cmd_push(CMD_SAVE, nil);
	   if(type(e) == EVALED) cmd_push(CMD_EVAL, typed_value(e));
	   else cmd_push(CMD_EVAL, quote(e));
	 }
    );
    return nil;

  case UNBOUND_SYMBOL:
    err("Symbol |%s| is unbound", string_ptr(symbol_name(a)));
    return nil;
  }
  return nil;
}

void cmd_error() {
  int frame = 0, done = 0;
  printf("Trace:\n");
  while(!done && cmd_sp--) {
    int type = cmd_stack[cmd_sp].type;
    tp arg = cmd_stack[cmd_sp].arg;

    if(type == CMD_ERROR_HANDLER) {
      cmd_push(CMD_EVAL, arg);
      done = 1;
    } else if(type == CMD_FUNCALL) pop(args1st);
    else if(type == CMD_POP_ARGS) pop_args(cmd_stack[cmd_sp].prev_sp);
    else if(type == CMD_FRAME) {
      printf("  %d: ", frame++); pp(cmd_stack[cmd_sp].arg);
      pop_args(cmd_stack[cmd_sp].prev_sp);
      set(env, cmd_stack[cmd_sp].env);
      release(cmd_stack[cmd_sp].env);
    } else if(type == CMD_END) {
      done = 1;
    }
    release(arg);
  }
}

tp cmd_loop(tp p) {
  int cycle = 0;
  cmd_sp = 0;
  cmd_push(CMD_END, nil);
  cmd_push(CMD_EVAL, p);
  while(cmd_sp--) {
    int type = cmd_stack[cmd_sp].type;
    tp arg = cmd_stack[cmd_sp].arg;

    switch(type) {
    case CMD_EVAL: set(eval_last, eval(arg)); break;
    case CMD_PREPARE_FUNCALL: prepare_funcall(eval_last, arg); break;

    case CMD_FUNCALL:
      set(eval_last, funcall(arg, car(args1st)));
      pop(args1st);
      break;

    case CMD_SAVE: cmd_save(eval_last); break;
    case CMD_SAVE_EXPANDED: cmd_save_expanded(eval_last); break;
    case CMD_SETQ: symbol_set_value(arg, eval_last); break;

    case CMD_IF:
      cmd_push(CMD_EVAL, !null_p(eval_last) ? car(arg) : car(cdr(arg)));
      break;
      
    case CMD_FRAME:
      pop_args(cmd_stack[cmd_sp].prev_sp);
      set(env, cmd_stack[cmd_sp].env);
      release(cmd_stack[cmd_sp].env);
      break;

    case CMD_POP_ARGS:  pop_args(cmd_stack[cmd_sp].prev_sp); break;
    case CMD_ERROR: cmd_error(); break;
    case CMD_ERROR_HANDLER: break;
      //default: err("Invalid command");
    }

    release(arg);
    cycle++;
  }


  return eval_last;
}


tp nm_progn(tp args) {
  progn(args);
  return nil;
}

tp nm_setq(tp args) {
  while(pair_p(args)) {
    if(!pair_p(args) || !pair_p(cdr(args)) || !symbol_p(car(args))) {
      err("Invalid args to SETQ");
    }

    cmd_push(CMD_SETQ, car(args));
    cmd_push(CMD_EVAL, car(cdr(args)));
    args = cdr(cdr(args));
  }

  return nil;
}

tp nm_quote(tp args) {
  return car(args);
}

tp nm_if(tp args) {
  cmd_push(CMD_IF, cdr(args));
  cmd_push(CMD_EVAL, car(args));
  return nil;
}

tp even_elems(tp form) {
  return pair_p(form) ? cons(car(form), even_elems(cdr(cdr(form)))) : nil;
}

// for now, define it through lambda.
tp nm_let(tp args) {
  tp l = cons(stbl_get("lambda"), cons(even_elems(car(args)), cdr(args)));
  cmd_push(CMD_EVAL, cons(l, even_elems(cdr(car(args)))));
  return nil;
}

tp compile_lookup(tp sym) {
  loop(env_2nd, env,
       loop(e, env_2nd,
	    if(type(e) == AREF) {
	      if(aref_name(e) == symbol_name(sym)) return e;
	    } else if(symbol_name(e) == symbol_name(sym)) {
	      return e;
	    }
       );
  );

  return sym;
}

tp compile(tp d);

tp compile_quasiquote(tp qq) {
  tp r = car(qq);
  if(!pair_p(qq)) return nil;

  if(type(r) == EVALED) r = typed(EVALED, compile(typed_value(r)));
  else if(type(r) == EVALED_LIST) r = typed(EVALED_LIST, compile(typed_value(r)));
  return cons(r, compile_quasiquote(cdr(qq)));
}

tp compile_pair(tp d) {
  if(null_p(d)) return nil;
  if(pair_p(d)) return cons(compile(car(d)), compile_pair(cdr(d)));
  return compile_lookup(d);
}

tp compile(tp d) {
  if(null_p(d)) return nil;
  if(pair_p(d)) {
    if(car(d) == Lambda) return d;
    return compile_pair(d);
  }
  if(!symbol_p(d)) {
    if(type(d) == QUASIQUOTED)
      return quasiquote(compile_quasiquote(typed_value(d)));
    return d;
  }
  return compile_lookup(d);
}

tp lambda(bool macro, tp def) {
  Funct *f;
  bool rest = false;
  int nreq=0, nargs=0;
  tp q, p=nil, args = nil, env_frame = nil, arg_type = Required;


  for(q = car(def); pair_p(q); iter(q)) { // create local symbols for args
    tp e = car(q);
    if(e == Rest) {
      arg_type = e;
      rest = true;
    } else if(e == Optional) {
      arg_type = e;
    } else {
      tp s, c;
      if(!symbol_p(e)) {
	release(env_frame);
	err("Invalid argument list to LAMBDA");
	return nil;
      }
      s = symbol_set_type(symbol(symbol_name(e)), arg_type);
      c = cons(s, nil);
      if(null_p(env_frame)) env_frame = c;
      else rplacd(p, c);
      p = c;
      if(arg_type == Required) nreq++;
      nargs++;
    }
  }

  f = alloc(FUNCT, Funct);
  f->native = false;
  f->macro = macro;
  f->nargs = nargs;
  f->nreq = nreq;
  f->rest = rest;
  f->name = nil;
  f->env = preserve(cons(env_frame, env));

  // set arguments' offsets from top of the stack
  loop(e, env_frame,
       push(aref(symbol_name(e), -(nargs--)), args););

  with(env, cons(args, env))
    f->body = compile(cdr(def));
  endw;

  release(args);

  return f;
}

tp nm_lambda(tp def) {
  return lambda(false, def);
}

tp nm_lambda_m(tp def) {
  tp t = list(car(def), list(stbl_get("eval"), car(cdr(def))));
  tp r = lambda(true, t);
  release(t);
  return r;
}

tp nf_eval(tp args) {
  cmd_push(CMD_EVAL, car(args));
  return nil;
}

tp nf_read(tp args) {
  if(null_p(args)) return read(stdin_stream);
  if(type(car(args)) != STREAM) {
    err("Argument isn't stream");
    return nil;
  }
  return read(car(args));
}

tp nf_list(tp args) {
  return args;
}

tp nf_print(tp args) {
  print(car(args));
  printf("\n");
  return nil;
}

tp nf_disassemble(tp args) {
  Funct *f = car(args);
  if(type(f) != FUNCT) {
    err("Non-function argument to disassemble");
    return nil;
  }
  if(f->native) {
   if(f->macro) printf("#<native macro #x%x>\n", (uiptr)f->ptr);
   else printf("#<native function #x%x>\n", (uiptr)f->ptr);
  } else {
    if(f->macro) printf("(lambda-m ");
    else printf("(lambda ");
    print(car(f->env));
    printf(" ");
    print_pair(f->body);
    printf(")\n");
  }
  return nil;
}

tp nf_cons(tp args) {
  return cons(car(args), car(cdr(args)));
}

tp nf_car(tp args) {
  return car(car(args));
}

tp nf_cdr(tp args) {
  return cdr(car(args));
}

tp nf_symbol_name(tp args) {
  if(!symbol_p(car(args))) {
    err("invalid arguments");
    return nil;
  }
  return symbol_name(car(args));
}

tp nf_name_lambda(tp args) {
  tp l = car(args), n = car(cdr(args));
  if(type(l) != FUNCT || type(n) != STRING) {
    err("invalid arguments");
    return nil;
  }
  funct_set_name(l, n);

  return l;
}

tp nf_add(tp args) {
  int total = 0;
  loop(e, args,
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       total += fixnum2int(e);
  );
  return fixnum(total);
}

tp nf_sub(tp args) {
  int total;
  if(null_p(args)) return fixnum(0);
  if(!number_p(car(args))) {
    err("Argument isn't a number");
    return nil;
  }
  total = fixnum2int(car(args));
  if(!cdr(args)) return fixnum(-total);

  loop(e, cdr(args),
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       total -= fixnum2int(e);
  );
  return fixnum(total);
}

tp nf_mul(tp args) {
  int total = 1;
  loop(e, args,
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       total *= fixnum2int(e);
  );
  return fixnum(total);
}

tp nf_div(tp args) {
  int total, n;
  if(null_p(args)) return fixnum(1);
  if(!number_p(car(args))) {
    err("Argument isn't a number");
    return nil;
  }
  total = fixnum2int(car(args));
  if(!cdr(args)) return fixnum(1/total);
	
  loop(e, cdr(args),
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       n = fixnum2int(e);
       if(!n) {
	 err("Division by zero");
	 return nil;
       }
       total /= n;
  );
  return fixnum(total);
}


tp nf_mod(tp args) {
  int total, n;
 
  if(!number_p(car(args))) {
    err("Invalid arguments");
    return nil;
  }
  total = fixnum2int(car(args));

  loop(e, cdr(args),
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       n = fixnum2int(e);
       if(!n) {
	 err("Division by zero");
	 return nil;
       }
       total %= n;
  );
  return fixnum(total);
}

tp nf_lt(tp args) {
  int prev, n;
  if(!number_p(car(args))) {
    err("Argument isn't a number");
    return nil;
  }
  prev = fixnum2int(car(args));
  loop(e, cdr(args),
       if(!number_p(e)) {
	 err("Argument isn't a number");
	 return nil;
       }
       n = fixnum2int(e);
       if(prev >= n) return nil;
       prev = n;
  );
  return sym_t;
}

tp nf_eql(tp args) {
  tp prev = car(args);
  loop(e, cdr(args),
       if(!atoms_eql(prev, e)) return nil;);
  return sym_t;
}

struct {
  bool macro; int nargs; bool rest; char *name; tp (*fp)(tp);
} native_funs[] = {
  {1, 0, 1, "lambda", nm_lambda},
  {1, 0, 1, "lambda-m", nm_lambda_m},
  {1, 1, 0, "quote", nm_quote},
  {1, 1, 1, "let", nm_let},
  {1, 0, 1, "progn", nm_progn},
  {1, 0, 1, "setq", nm_setq},
  {1, 0, 1, "if", nm_if},
  {1, 1, 0, "quote", nm_quote},

  {0, 1, 0, "eval", nf_eval},
  {0, 0, 1, "read", nf_read},
  {0, 0, 1, "list", nf_list},
  {0, 1, 0, "print", nf_print},
  {0, 1, 0, "disassemble", nf_disassemble},
  {0, 2, 0, "cons", nf_cons},
  {0, 1, 0, "car", nf_car},
  {0, 1, 0, "cdr", nf_cdr},
  {0, 1, 0, "symbol-name", nf_symbol_name},
  {0, 2, 0, "name-lambda", nf_name_lambda},

  {0, 0, 1, "+", nf_add},
  {0, 0, 1, "-", nf_sub},
  {0, 0, 1, "*", nf_mul},
  {0, 0, 1, "/", nf_div},
  {0, 0, 1, "mod", nf_mod},
  {0, 2, 1, "<", nf_lt},
  {0, 0, 1, "eql", nf_eql},

  {0, 0, 0, 0, 0}
};

void init() {
  int i;


  set_type(nil, BOUND_SYMBOL);
  set_nref(nil, INVALID_NREF);
  ((Symbol*)nil)->value = nil;
  ((Symbol*)nil)->name = string("nil");


  sym_t = symbol(string("t"));
  set_nref(sym_t, INVALID_NREF);
  symbol_set_value(sym_t, sym_t);

  stbl_init();
  stbl_add_sym(nil);
  stbl_add_sym(sym_t);

  for(i = 0; native_funs[i].fp; i++) {
    Funct *f = alloc(FUNCT, Funct);
    f->native = true;
    f->macro = native_funs[i].macro;
    f->nreq = f->nargs = native_funs[i].nargs;
    f->rest = native_funs[i].rest;
    f->ptr = native_funs[i].fp;
    f->name = preserve(symbol_name(stbl_set(native_funs[i].name, f)));
  }

  Invalid = stbl_add("&invalid");
  Rest = stbl_add("&rest");
  Optional = stbl_add("&optional");
  Required = stbl_add("&required");
  Lambda = stbl_get_or_add("lambda");

  eof = stbl_add("*eof*");
  stdin_stream = symbol_value(stbl_set("*stdin*", stream(stdin)));
  stdout_stream = symbol_value(stbl_set("*stdout*", stream(stdout)));

  env = nil;
  args1st = nil;

  reader_last = nil;
  eval_last = nil;
}

void deinit() {
  release(reader_last);
  release(eval_last);
  stbl_clear();
  release(stbl);
  printf("objects in use:  %d\n", objects_in_use-3);
}

/*int file_size(FILE *file) {
  int len;
  int saved_pos = (int)ftell(file);
  fseek(file, 0, SEEK_END);
  len = (int)ftell(file);
  fseek(file, saved_pos, SEEK_SET);
  return len;
}*/

// pretty-printer
void pp(tp p) {
  print(p); printf("\n");
}

//int lisp(int argc, char **argv) {
void lisp() {
  int elem = 0;
  tp input = nil;

  init();

  input = stdin_stream;

  //input = preserve(argc > 1 ? stream(fopen(argv[1], "r")) : stdin_stream);
  //if(input == nil) fatal("Couldn't open \"%s\"", argv[1]);


  for(;;) {
    printf("[%d] ", elem++);
    if(read(input) == eof) {
      printf("\n");
      break;
    }

    cmd_loop(reader_last);
    printf("-> "); pp(eval_last);
  }

  release(input);

  deinit();

  //return 0;
}
