/*
 * Iris Programming Language - Embedding API
 *
 * A Lua-inspired C API for embedding Iris in host applications.
 */

#ifndef IRIS_H
#define IRIS_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque state handle */
typedef struct IrisState IrisState;

/* Status codes */
typedef enum {
  IRIS_OK = 0,  /* No error */
  IRIS_ERRRUN,  /* Runtime error */
  IRIS_ERRMEM,  /* Memory allocation error */
  IRIS_ERRTYPE, /* Type error */
} IrisStatus;

/* Type identifiers */
typedef enum {
  IRIS_TNIL = 0, /* Nil/null value */
  IRIS_TINT,     /* Integer */
  IRIS_TSTRING,  /* String */
  IRIS_TBOOL,    /* Boolean */
  IRIS_TSTRUCT,  /* Struct instance */
  IRIS_TOBJECT,  /* Class instance (object) */
  IRIS_TLIST,
  IRIS_TFUTURE,
} IrisType;

/* Native function signature: returns number of results pushed */
typedef int (*IrisCFunction)(IrisState *I);
/* Create a new Iris state */
IrisState *iris_open(void);
/* Close and free an Iris state */
void iris_close(IrisState *I);
/* Load and compile Iris source code */
IrisStatus iris_loadstring(IrisState *I, const char *source);
/* Load and compile from file */
IrisStatus iris_loadfile(IrisState *I, const char *path);
/* Execute the loaded program */
IrisStatus iris_run(IrisState *I);
/* Protected call with error handling */
IrisStatus iris_pcall(IrisState *I, int nargs, int nresults);
/* Get the current stack top (number of elements) */
int iris_gettop(IrisState *I);
/* Set the stack top, adjusting size as needed */
void iris_settop(IrisState *I, int index);
/* Pop n values from the stack */
void iris_pop(IrisState *I, int n);
/* Push a copy of value at index */
void iris_pushvalue(IrisState *I, int index);
/* Push nil (represented as 0) */
void iris_pushnil(IrisState *I);
/* Push an integer */
void iris_pushinteger(IrisState *I, int64_t n);
/* Push a boolean */
void iris_pushboolean(IrisState *I, bool b);
/* Push a string (copies the string) */
void iris_pushstring(IrisState *I, const char *s);
/* Push a string with explicit length */
void iris_pushlstring(IrisState *I, const char *s, size_t len);
/* Check if value at index is of specific type */
bool iris_isinteger(IrisState *I, int index);
bool iris_isstring(IrisState *I, int index);
bool iris_isboolean(IrisState *I, int index);
bool iris_isstruct(IrisState *I, int index);
bool iris_isobject(IrisState *I, int index);
bool iris_isnil(IrisState *I, int index);
/* Get the type of value at index */
IrisType iris_type(IrisState *I, int index);
/* Get type name as string */
const char *iris_typename(IrisState *I, IrisType t);
/* Convert to integer (returns 0 if not convertible) */
int64_t iris_tointeger(IrisState *I, int index);
/* Convert to boolean */
bool iris_toboolean(IrisState *I, int index);
/* Get string value (returns NULL if not a string) */
const char *iris_tostring(IrisState *I, int index);
/* Get string/object length */
size_t iris_rawlen(IrisState *I, int index);
/* Check and return integer argument (sets error if wrong type) */
int64_t iris_checkinteger(IrisState *I, int arg);
/* Check and return string argument (sets error if wrong type) */
const char *iris_checkstring(IrisState *I, int arg);
/* Check and return boolean argument (sets error if wrong type) */
bool iris_checkboolean(IrisState *I, int arg);
/* Check arbitrary condition, set error if false */
void iris_argcheck(IrisState *I, bool cond, int arg, const char *msg);
/* Get global variable, pushes to stack. Returns type. */
IrisType iris_getglobal(IrisState *I, const char *name);
/* Set global variable, pops from stack */
void iris_setglobal(IrisState *I, const char *name);
/* Register a C function as a global Iris function
 * nparams: expected parameter count (-1 for variadic) */
void iris_register(IrisState *I, const char *name, IrisCFunction fn,
                   int nparams);
/* Get the last error message */
const char *iris_error(IrisState *I);
/* Set error message (for native functions) */
void iris_seterror(IrisState *I, const char *fmt, ...);
/* Get field from struct/object at index, pushes to stack */
void iris_getfield(IrisState *I, int index, const char *field);
/* Set field in struct/object at index, pops value from stack */
void iris_setfield(IrisState *I, int index, const char *field);
#ifdef __cplusplus
}
#endif
#endif /* IRIS_H */

#define IRIS_IMPLEMENTATION

#ifdef IRIS_IMPLEMENTATION
/* Expose POSIX and BSD/XSI extensions so functions like `usleep` and
 * `strdup` are declared on platforms (glibc, musl) that hide them unless
 * feature test macros are defined before including system headers.
 *
 * Use a conservative POSIX version that provides `usleep`/`strdup`.
 */
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

// clang-format off
typedef enum {
  TOK_NUMBER, TOK_IDENT, TOK_FN, TOK_RETURN, TOK_FOR, TOK_WHILE,
  TOK_IF, TOK_ELSE, TOK_MUT, TOK_CONST, TOK_IN, TOK_STRUCT, TOK_CLASS,
  TOK_NEW, TOK_TRY, TOK_CATCH, TOK_THROW, TOK_IMPORT, TOK_EXPORT,
  TOK_FROM, TOK_RANGE, TOK_LPAREN, TOK_RPAREN, TOK_LBRACE, TOK_RBRACE,
  TOK_COMMA, TOK_SEMICOLON, TOK_DOT, TOK_COLON, TOK_PLUS, TOK_MINUS,
  TOK_STAR, TOK_SLASH, TOK_EQ_EQ, TOK_NOT_EQ, TOK_LT, TOK_GT,
  TOK_LT_EQ, TOK_GT_EQ, TOK_ASSIGN, TOK_STRING, TOK_FSTRING,
  TOK_TRUE, TOK_FALSE,
  // Bitwise and modulo tokens
  TOK_PERCENT, TOK_AMP, TOK_PIPE, TOK_CARET, TOK_TILDE, TOK_SHL, TOK_SHR,
  // Compound assignment tokens
  TOK_PLUS_EQ, TOK_MINUS_EQ, TOK_STAR_EQ, TOK_SLASH_EQ, TOK_PERCENT_EQ,
  TOK_AMP_EQ, TOK_PIPE_EQ, TOK_CARET_EQ, TOK_SHL_EQ, TOK_SHR_EQ,
  // Loop control tokens
  TOK_BREAK, TOK_CONTINUE,
  // Array tokens
  TOK_LBRACKET, TOK_RBRACKET,
  // Async tokens
  TOK_ASYNC, TOK_AWAIT,
  TOK_EOF
} IrisTokenType;

// Token type to human-readable string
static const char *const token_type_names[] = {
  [TOK_NUMBER]     = "number",
  [TOK_IDENT]      = "identifier",
  [TOK_FN]         = "'function'",
  [TOK_RETURN]     = "'return'",
  [TOK_FOR]        = "'for'",
  [TOK_WHILE]      = "'while'",
  [TOK_IF]         = "'if'",
  [TOK_ELSE]       = "'else'",
  [TOK_MUT]        = "'mut'",
  [TOK_CONST]      = "'const'",
  [TOK_IN]         = "'in'",
  [TOK_STRUCT]     = "'struct'",
  [TOK_CLASS]      = "'class'",
  [TOK_NEW]        = "'new'",
  [TOK_TRY]        = "'try'",
  [TOK_CATCH]      = "'catch'",
  [TOK_THROW]      = "'throw'",
  [TOK_IMPORT]     = "'import'",
  [TOK_EXPORT]     = "'export'",
  [TOK_FROM]       = "'from'",
  [TOK_RANGE]      = "'..'",
  [TOK_LPAREN]     = "'('",
  [TOK_RPAREN]     = "')'",
  [TOK_LBRACE]     = "'{'",
  [TOK_RBRACE]     = "'}'",
  [TOK_COMMA]      = "','",
  [TOK_SEMICOLON]  = "';'",
  [TOK_DOT]        = "'.'",
  [TOK_COLON]      = "':'",
  [TOK_PLUS]       = "'+'",
  [TOK_MINUS]      = "'-'",
  [TOK_STAR]       = "'*'",
  [TOK_SLASH]      = "'/'",
  [TOK_EQ_EQ]      = "'=='",
  [TOK_NOT_EQ]     = "'!='",
  [TOK_LT]         = "'<'",
  [TOK_GT]         = "'>'",
  [TOK_LT_EQ]      = "'<='",
  [TOK_GT_EQ]      = "'>='",
  [TOK_ASSIGN]     = "'='",
  [TOK_STRING]     = "string",
  [TOK_FSTRING]    = "f-string",
  [TOK_TRUE]       = "'true'",
  [TOK_FALSE]      = "'false'",
  [TOK_PERCENT]    = "'%'",
  [TOK_AMP]        = "'&'",
  [TOK_PIPE]       = "'|'",
  [TOK_CARET]      = "'^'",
  [TOK_TILDE]      = "'~'",
  [TOK_SHL]        = "'<<'",
  [TOK_SHR]        = "'>>'",
  [TOK_PLUS_EQ]    = "'+='",
  [TOK_MINUS_EQ]   = "'-='",
  [TOK_STAR_EQ]    = "'*='",
  [TOK_SLASH_EQ]   = "'/='",
  [TOK_PERCENT_EQ] = "'%='",
  [TOK_AMP_EQ]     = "'&='",
  [TOK_PIPE_EQ]    = "'|='",
  [TOK_CARET_EQ]   = "'^='",
  [TOK_SHL_EQ]     = "'<<='",
  [TOK_SHR_EQ]     = "'>>='",
  [TOK_BREAK]      = "'break'",
  [TOK_CONTINUE]   = "'continue'",
  [TOK_LBRACKET]   = "'['",
  [TOK_RBRACKET]   = "']'",
  [TOK_ASYNC]      = "'async'",
  [TOK_AWAIT]      = "'await'",
  [TOK_EOF]        = "end of file",
};

const char *token_type_str(IrisTokenType type) {
  if (type < 0 || type >= (int)(sizeof token_type_names / sizeof *token_type_names) ||
      !token_type_names[type]) {
    return "unknown";
  }
  return token_type_names[type];
}

typedef enum {
  OP_PUSH, OP_LOAD_LOCAL, OP_STORE_LOCAL, OP_ADD, OP_SUB, OP_MUL,
  OP_DIV, OP_EQ, OP_NOT_EQ, OP_LT, OP_GT,
  OP_LT_EQ, OP_GT_EQ, OP_JMP, OP_JMP_IF_FALSE, OP_CALL,
  OP_RETURN, OP_POP, OP_HALT, OP_PUSH_STRING, OP_STRING_CONCAT,
  OP_PUSH_BOOL, OP_TO_STRING, OP_NEW_STRUCT, OP_GET_FIELD, OP_SET_FIELD,
  OP_NEW_CLASS, OP_CALL_METHOD, OP_TRY_BEGIN, OP_TRY_END, OP_THROW,
  // Bitwise and modulo opcodes
  OP_MOD, OP_BITAND, OP_BITOR, OP_BITXOR, OP_BITNOT, OP_SHL, OP_SHR, OP_NEGATE,
  // Stack manipulation
  OP_DUP, OP_DUP2,
  // Array opcodes
  OP_NEW_LIST, OP_LIST_GET, OP_LIST_SET, OP_LIST_PUSH, OP_LIST_POP, OP_LIST_LEN, OP_LIST_PEEK,
  // Async opcodes
  OP_AWAIT, OP_ASYNC_CALL, OP_TASK_RETURN,
  // Function reference
  OP_PUSH_FUNC,
} IrisOpCode;
// clang-format on

typedef struct IrisString IrisString;
typedef struct IrisStruct IrisStruct;
typedef struct IrisClass IrisClass;
typedef struct IrisList IrisList;
typedef struct IrisFuture IrisFuture;
typedef struct IrisTask IrisTask;

typedef struct {
  char name[128];
  char fields[16][128];
  int field_count;
} StructType;

typedef struct {
  char name[128];
  int method_idx; // Index in funcs array
} ClassMethod;

typedef struct {
  char name[128];
  char fields[16][128];
  int field_count;
  ClassMethod methods[16];
  int method_count;
  int ctor_idx; // Constructor function index (-1 if none)
  int dtor_idx; // Destructor function index (-1 if none)
} ClassType;

typedef enum { IMPORT_FN, IMPORT_STRUCT, IMPORT_CLASS } ImportType;

typedef struct {
  char name[128];
  ImportType type;
  int index;
} Export;

struct IrisString {
  char *data;
  int length;
  int refcount;
};

typedef enum {
  VAL_INT,
  VAL_STRING,
  VAL_BOOL,
  VAL_STRUCT,
  VAL_CLASS,
  VAL_LIST,
  VAL_FUTURE,
  VAL_FUNC
} ValueType;

typedef struct {
  ValueType type;
  union {
    int64_t num;
    IrisString *str;
    bool boolean;
    IrisStruct *instance;
    IrisClass *object;
    IrisList *list;
    IrisFuture *future;
    int func_idx; // Index into prog->funcs for VAL_FUNC
  } as;
} IrisValue;

struct IrisList {
  IrisValue *items;
  int length;
  int capacity;
  int refcount;
};

struct IrisStruct {
  int type_id;
  int refcount;
  int field_count;
  IrisValue *fields;
};

struct IrisClass {
  int class_id;
  int refcount;
  int field_count;
  IrisValue *fields;
};

/* exception handler entry */
typedef struct {
  int catch_addr; /* addr of catch block */
  int sp,         /* stack ptr */
      bp,         /* base ptr to restore */
      csp;        /* call stack ptr to restore */
} ExceptionHandler;

/* call frame info for stack traces */
typedef struct {
  int func_idx;  /* idx of function being invoked */
  int call_line; /* line num of call site */
  int return_addr;
} CallFrame;

typedef struct {
  IrisValue stack[256];
  int sp;
  IrisValue locals[256];
  int bp;
  int call_stack[64]; /* return addresses (kept for compatibility) */
  int bp_stack[64];
  int sp_stack[64];
  CallFrame frames[64]; /* call frame info */
  int csp;
  int ip;

  /** exception handling */
  ExceptionHandler handlers[32];
  int handler_count;
  IrisValue exception; /* current exception value */
  bool has_exception;  /* if the exception is active */
} VM;

typedef enum { FUTURE_PENDING, FUTURE_RESOLVED, FUTURE_REJECTED } FutureState;

/* callback entry for tasks waiting on a future */
typedef struct FutureCallback {
  int task_id;
  struct FutureCallback *next;
} FutureCallback;

struct IrisFuture {
  int refcount;
  FutureState state;
  IrisValue result;          // Resolved value or rejection reason
  FutureCallback *callbacks; // Tasks waiting on this future

  // For .then/.catch/.finally chaining
  struct IrisFuture *chained; // Next future in chain
  int on_resolve_func; // Function index for .then() callback (-1 if none)
  int on_reject_func;  // Function index for .catch() callback (-1 if none)
  int on_finally_func; // Function index for .finally() callback (-1 if none)

  // For all/race combinators
  void *combinator;    // AllCombinator* for tracking
  int combinator_type; // 0 = none, 1 = all, 2 = race
};

typedef enum {
  TASK_READY,
  TASK_RUNNING,
  TASK_SUSPENDED,
  TASK_COMPLETED,
  TASK_FAILED
} TaskState;

struct IrisTask {
  int id;
  int refcount;
  TaskState state;

  // Saved VM state for suspension/resumption
  int ip, bp, sp, csp;
  IrisValue stack[256];
  IrisValue locals[256];
  int call_stack[64];
  int bp_stack[64];
  int sp_stack[64];
  CallFrame frames[64];
  ExceptionHandler handlers[32];
  int handler_count;

  IrisFuture *future;   // Future this task returns
  IrisFuture *awaiting; // Future we're waiting on (if suspended)
};

// Timer entry for sleep() implementation
typedef struct TimerEntry {
  int64_t trigger_time_ms;
  IrisFuture *future;
  struct TimerEntry *next;
} TimerEntry;

// Event loop for async scheduling
typedef struct {
  IrisTask **ready_queue;
  int ready_count;
  int ready_capacity;

  IrisTask **all_tasks; // All tracked tasks
  int task_count;
  int task_capacity;

  TimerEntry *timers;     // Sorted linked list of pending timers
  IrisTask *current_task; // Currently executing task (NULL if main)
  int next_task_id;
  bool running;
} EventLoop;

// Global event loop (initialized before execution)
static EventLoop *g_event_loop = NULL;

// ============================================================================
// Iris Embedding API Types
// ============================================================================

// Forward declarations
typedef struct IrisState IrisState;
typedef struct IrisProgram IrisProgram;

// Native function for embedding API: returns number of results pushed
typedef int (*IrisCFunction)(IrisState *I);

// Internal global registry entry for C functions
typedef struct {
  char name[128];
  IrisCFunction fn;
  int nparams;
} IrisCFuncEntry;

// The main Iris state - combines VM, program, and API state
struct IrisState {
  VM vm;
  IrisProgram *prog;

  // API stack (separate from VM execution stack)
  IrisValue api_stack[256];
  int api_top;

  // Globals registry (name -> value mapping)
  struct {
    char name[128];
    IrisValue value;
  } globals[128];
  int global_count;

  // Error state
  char error_msg[512];
  IrisStatus last_status;

  // C function registry
  IrisCFuncEntry cfuncs[64];
  int cfunc_count;
};

// Global pointer to current state (for native function bridging)
static IrisState *g_current_iris_state = NULL;

// Current native function name being called (for bridging)
static const char *g_current_native_func_name = NULL;

// Forward declaration of bridge function
static void iris_cfunc_bridge(VM *vm, int argc);

typedef void (*NativeFunc)(VM *vm, int argc);

// Loop context for break/continue support
typedef struct {
  int break_patches[16]; // Addresses to patch for break jumps
  int break_count;
  int continue_patches[16]; // Addresses to patch for continue jumps
  int continue_count;
  int continue_target; // Address where continue should jump to
} LoopContext;

typedef struct {
  char name[128];
  int addr;
  int is_native;
  NativeFunc native_fn;
  int param_count;
  int returns_void;
  int is_async; // Whether this is an async function
  char params[8][128];
  char locals[64][128];
  int local_count;
  int is_mutable[64];
  int scope_depth[64];
  int current_scope;
  // Loop context for break/continue
  LoopContext loops[8];
  int loop_depth;
} IrisFunction;

// Line number table entry (maps code offset to source line)
typedef struct {
  int code_offset;
  int line;
} LineInfo;

struct IrisProgram {
  unsigned char *code;
  int code_size;
  int code_capacity;
  IrisFunction funcs[32];
  int func_count;
  IrisString *strings[256];
  int string_count;
  StructType struct_types[32];
  int struct_type_count;
  ClassType class_types[32];
  int class_type_count;
  Export exports[64];
  int export_count;
  char source_path[256];

  // Line number tracking
  LineInfo *lines;
  int line_count;
  int line_capacity;

  // Source code storage for error messages
  char *source;
};

typedef struct {
  IrisTokenType type;
  char str[64];
  int64_t num;
  int line; // Line number where token appears
} IrisToken;

typedef struct {
  char path[256];
  int is_compiling;
  IrisProgram *program;
} IrisModule;

IrisModule g_modules[32];
int g_module_count = 0;

typedef struct {
  const char *src;
  int pos;
  int line; // Current line number (1-based)
  IrisToken current;
} Lexer;

void compile_statement(Lexer *lex, IrisProgram *prog,
                       IrisFunction *current_func);
void compile_comparison(Lexer *lex, IrisProgram *prog,
                        IrisFunction *current_func);
void compile_expr(Lexer *lex, IrisProgram *prog, IrisFunction *current_func);
void iris_value_incref(IrisValue *v);
void iris_value_decref(IrisValue *v);
void iris_future_incref(IrisFuture *f);
void iris_future_decref(IrisFuture *f);
void event_loop_schedule_task(EventLoop *loop, IrisTask *task);
bool throw_exception(VM *vm, IrisProgram *prog, IrisValue exception);

void emit_line(IrisProgram *prog, int line) {
  if (prog->line_count > 0 && prog->lines[prog->line_count - 1].line == line &&
      prog->lines[prog->line_count - 1].code_offset == prog->code_size) {
    return;
  }
  if (prog->line_count >= prog->line_capacity) {
    prog->line_capacity = prog->line_capacity ? prog->line_capacity * 2 : 64;
    prog->lines = realloc(prog->lines, sizeof(LineInfo) * prog->line_capacity);
  }
  prog->lines[prog->line_count].code_offset = prog->code_size;
  prog->lines[prog->line_count].line = line;
  prog->line_count++;
}

// Find line number for a code offset
int get_line_for_offset(IrisProgram *prog, int offset) {
  int line = 1;
  for (int i = 0; i < prog->line_count; i++) {
    if (prog->lines[i].code_offset <= offset) {
      line = prog->lines[i].line;
    } else {
      break;
    }
  }
  return line;
}

// Get a specific line from source code (returns full line, not trimmed)
const char *get_source_line(const char *source, int line_num, char *buffer,
                            int buf_size) {
  if (!source)
    return NULL;
  const char *p = source;
  int current_line = 1;
  while (*p && current_line < line_num) {
    if (*p == '\n')
      current_line++;
    p++;
  }
  if (!*p)
    return NULL;
  int i = 0;
  while (*p && *p != '\n' && i < buf_size - 1) {
    buffer[i++] = *p++;
  }
  buffer[i] = '\0';
  return buffer;
}

// Global source storage for error reporting during compilation
static const char *g_current_source = NULL;
static const char *g_current_file = NULL;

// ANSI color codes for terminal output
#define COLOR_RED "\033[1;31m"
#define COLOR_YELLOW "\033[1;33m"
#define COLOR_CYAN "\033[1;36m"
#define COLOR_RESET "\033[0m"
#define COLOR_BOLD "\033[1m"

// Calculate column position within a line from source position
static int get_column(const char *source, int pos) {
  if (!source || pos <= 0)
    return 1;
  int col = 1;
  int i = pos - 1;
  while (i >= 0 && source[i] != '\n') {
    col++;
    i--;
  }
  return col;
}

// Print a formatted error message with source context
static void report_error_at(const char *file, int line, int col,
                            const char *source, const char *fmt, ...) {
  char msg[512];
  va_list args;
  va_start(args, fmt);
  vsnprintf(msg, sizeof(msg), fmt, args);
  va_end(args);
  fprintf(stderr, "\n%s%serror%s: %s\n", COLOR_BOLD, COLOR_RED, COLOR_RESET,
          msg);
  fprintf(stderr, "  %s--> %s%s:%d:%d%s\n", COLOR_CYAN, COLOR_RESET,
          file ? file : "<source>", line, col, COLOR_RESET);
  if (source && line > 0) {
    char line_buf[256];
    const char *src_line =
        get_source_line(source, line, line_buf, sizeof(line_buf));
    if (src_line) {
      fprintf(stderr, "   %s|%s\n", COLOR_CYAN, COLOR_RESET);
      fprintf(stderr, " %s%3d |%s %s\n", COLOR_CYAN, line, COLOR_RESET,
              src_line);
      fprintf(stderr, "   %s|%s ", COLOR_CYAN, COLOR_RESET);
      int spaces = 0;
      for (int i = 0; i < col - 1 && src_line[i]; i++) {
        if (src_line[i] == '\t')
          spaces += 4 - (spaces % 4);
        else
          spaces++;
      }
      for (int i = 0; i < spaces; i++)
        fputc(' ', stderr);
      fprintf(stderr, "%s^%s\n", COLOR_RED, COLOR_RESET);
    }
  }
  fprintf(stderr, "\n");
}

// Simplified error for lexer (uses global source)
static void lexer_error(int line, int pos, const char *fmt, ...) {
  char msg[512];
  va_list args;
  va_start(args, fmt);
  vsnprintf(msg, sizeof(msg), fmt, args);
  va_end(args);
  int col = get_column(g_current_source, pos);
  report_error_at(g_current_file, line, col, g_current_source, "%s", msg);
  exit(1);
}

// Token type name helper
static const char *token_name(IrisTokenType type) {
  switch (type) {
  case TOK_NUMBER:
    return "number";
  case TOK_IDENT:
    return "identifier";
  case TOK_FN:
    return "'function'";
  case TOK_RETURN:
    return "'return'";
  case TOK_FOR:
    return "'for'";
  case TOK_WHILE:
    return "'while'";
  case TOK_IF:
    return "'if'";
  case TOK_ELSE:
    return "'else'";
  case TOK_MUT:
    return "'mut'";
  case TOK_CONST:
    return "'const'";
  case TOK_IN:
    return "'in'";
  case TOK_STRUCT:
    return "'struct'";
  case TOK_CLASS:
    return "'class'";
  case TOK_NEW:
    return "'new'";
  case TOK_TRY:
    return "'try'";
  case TOK_CATCH:
    return "'catch'";
  case TOK_THROW:
    return "'throw'";
  case TOK_IMPORT:
    return "'import'";
  case TOK_EXPORT:
    return "'export'";
  case TOK_FROM:
    return "'from'";
  case TOK_RANGE:
    return "'..'";
  case TOK_LPAREN:
    return "'('";
  case TOK_RPAREN:
    return "')'";
  case TOK_LBRACE:
    return "'{'";
  case TOK_RBRACE:
    return "'}'";
  case TOK_COMMA:
    return "','";
  case TOK_SEMICOLON:
    return "';'";
  case TOK_DOT:
    return "'.'";
  case TOK_COLON:
    return "':'";
  case TOK_PLUS:
    return "'+'";
  case TOK_MINUS:
    return "'-'";
  case TOK_STAR:
    return "'*'";
  case TOK_SLASH:
    return "'/'";
  case TOK_EQ_EQ:
    return "'=='";
  case TOK_NOT_EQ:
    return "'!='";
  case TOK_LT:
    return "'<'";
  case TOK_GT:
    return "'>'";
  case TOK_LT_EQ:
    return "'<='";
  case TOK_GT_EQ:
    return "'>='";
  case TOK_ASSIGN:
    return "'='";
  case TOK_STRING:
    return "string";
  case TOK_FSTRING:
    return "f-string";
  case TOK_TRUE:
    return "'true'";
  case TOK_FALSE:
    return "'false'";
  case TOK_PERCENT:
    return "'%'";
  case TOK_AMP:
    return "'&'";
  case TOK_PIPE:
    return "'|'";
  case TOK_CARET:
    return "'^'";
  case TOK_TILDE:
    return "'~'";
  case TOK_SHL:
    return "'<<'";
  case TOK_SHR:
    return "'>>'";
  case TOK_PLUS_EQ:
    return "'+='";
  case TOK_MINUS_EQ:
    return "'-='";
  case TOK_STAR_EQ:
    return "'*='";
  case TOK_SLASH_EQ:
    return "'/='";
  case TOK_PERCENT_EQ:
    return "'%='";
  case TOK_AMP_EQ:
    return "'&='";
  case TOK_PIPE_EQ:
    return "'|='";
  case TOK_CARET_EQ:
    return "'^='";
  case TOK_SHL_EQ:
    return "'<<='";
  case TOK_SHR_EQ:
    return "'>>='";
  case TOK_BREAK:
    return "'break'";
  case TOK_CONTINUE:
    return "'continue'";
  case TOK_LBRACKET:
    return "'['";
  case TOK_RBRACKET:
    return "']'";
  case TOK_ASYNC:
    return "'async'";
  case TOK_AWAIT:
    return "'await'";
  case TOK_EOF:
    return "end of file";
  }
  return "unknown token";
}

// Parser error with source context
static void parser_error(Lexer *lex, const char *fmt, ...) {
  char msg[512];
  va_list args;
  va_start(args, fmt);
  vsnprintf(msg, sizeof(msg), fmt, args);
  va_end(args);
  int col = get_column(g_current_source, lex->pos);
  report_error_at(g_current_file, lex->current.line, col, g_current_source,
                  "%s", msg);
  exit(1);
}

// Runtime error with source context
static void runtime_error_at(IrisProgram *prog, VM *vm, const char *fmt, ...) {
  char msg[512];
  va_list args;
  va_start(args, fmt);
  vsnprintf(msg, sizeof(msg), fmt, args);
  va_end(args);

  int line = get_line_for_offset(prog, vm->ip);
  report_error_at(prog->source_path, line, 1, prog->source, "%s", msg);

  // Print stack trace if we have call frames
  if (vm->csp > 0) {
    fprintf(stderr, "%sStack trace:%s\n", COLOR_BOLD, COLOR_RESET);
    for (int i = vm->csp - 1; i >= 0; i--) {
      int frame_line = vm->frames[i].call_line;
      int func_idx = vm->frames[i].func_idx;
      const char *func_name =
          (func_idx >= 0) ? prog->funcs[func_idx].name : "<unknown>";
      fprintf(stderr, "  at %s (%s:%d)\n", func_name, prog->source_path,
              frame_line);
    }
    fprintf(stderr, "\n");
  }
}

IrisString *iris_string_new(const char *data, int length) {
  IrisString *str = malloc(sizeof(IrisString));
  str->data = malloc(length + 1);
  memcpy(str->data, data, length);
  str->data[length] = '\0';
  str->length = length;
  str->refcount = 1;
  return str;
}

IrisString *iris_string_copy(const char *data) {
  return iris_string_new(data, strlen(data));
}

void iris_string_incref(IrisString *str) {
  if (str)
    str->refcount++;
}

void iris_string_decref(IrisString *str) {
  if (str && --str->refcount == 0) {
    free(str->data);
    free(str);
  }
}

IrisString *iris_string_concat(IrisString *a, IrisString *b) {
  int new_len = a->length + b->length;
  IrisString *result = malloc(sizeof(IrisString));
  result->data = malloc(new_len + 1);
  memcpy(result->data, a->data, a->length);
  memcpy(result->data + a->length, b->data, b->length);
  result->data[new_len] = '\0';
  result->length = new_len;
  result->refcount = 1;
  return result;
}

IrisString *iris_value_to_string(IrisValue v) {
  char buf[256];

  if (v.type == VAL_INT) {
    snprintf(buf, sizeof(buf), "%" PRId64, v.as.num);
    return iris_string_copy(buf);
  } else if (v.type == VAL_STRING) {
    // For strings inside lists, add quotes
    int len = v.as.str->length + 3; // quotes + null
    char *quoted = malloc(len);
    snprintf(quoted, len, "\"%s\"", v.as.str->data);
    IrisString *result = iris_string_copy(quoted);
    free(quoted);
    return result;
  } else if (v.type == VAL_BOOL) {
    return iris_string_copy(v.as.boolean ? "true" : "false");
  } else if (v.type == VAL_STRUCT) {
    return iris_string_copy("<struct>");
  } else if (v.type == VAL_CLASS) {
    return iris_string_copy("<object>");
  } else if (v.type == VAL_LIST) {
    IrisList *list = v.as.list;

    // Calculate approximate size needed
    int estimated_size = 2; // []
    for (int i = 0; i < list->length; i++) {
      estimated_size += 32; // rough estimate per element
      if (i > 0)
        estimated_size += 2; // ", "
    }

    char *buffer = malloc(estimated_size > 1024 ? estimated_size : 1024);
    int pos = 0;
    buffer[pos++] = '[';

    for (int i = 0; i < list->length; i++) {
      if (i > 0) {
        buffer[pos++] = ',';
        buffer[pos++] = ' ';
      }
      IrisString *elem_str = iris_value_to_string(list->items[i]);
      int elem_len = elem_str->length;
      memcpy(buffer + pos, elem_str->data, elem_len);
      pos += elem_len;
      iris_string_decref(elem_str);
    }

    buffer[pos++] = ']';
    buffer[pos] = '\0';

    IrisString *result = iris_string_copy(buffer);
    free(buffer);
    return result;
  } else if (v.type == VAL_FUTURE) {
    IrisFuture *f = v.as.future;
    const char *state_str;
    switch (f->state) {
    case FUTURE_PENDING:
      state_str = "pending";
      break;
    case FUTURE_RESOLVED:
      state_str = "resolved";
      break;
    case FUTURE_REJECTED:
      state_str = "rejected";
      break;
    default:
      state_str = "unknown";
      break;
    }
    snprintf(buf, sizeof(buf), "<Future:%s>", state_str);
    return iris_string_copy(buf);
  } else if (v.type == VAL_FUNC) {
    snprintf(buf, sizeof(buf), "<function:%d>", v.as.func_idx);
    return iris_string_copy(buf);
  }
  return iris_string_copy("<unknown>");
}

IrisStruct *iris_struct_new(int type_id, int field_count) {
  IrisStruct *s = malloc(sizeof(IrisStruct));
  s->type_id = type_id;
  s->refcount = 1;
  s->field_count = field_count;
  s->fields = malloc(sizeof(IrisValue) * field_count);
  for (int i = 0; i < field_count; i++) {
    s->fields[i].type = VAL_INT;
    s->fields[i].as.num = 0;
  }
  return s;
}

void iris_struct_incref(IrisStruct *s) {
  if (s)
    s->refcount++;
}

void iris_struct_decref(IrisStruct *s) {
  if (s && --s->refcount == 0) {
    for (int i = 0; i < s->field_count; i++) {
      iris_value_decref(&s->fields[i]);
    }
    free(s->fields);
    free(s);
  }
}

IrisClass *iris_class_new(int class_id, int field_count) {
  IrisClass *c = malloc(sizeof(IrisClass));
  c->class_id = class_id;
  c->refcount = 1;
  c->field_count = field_count;
  c->fields = malloc(sizeof(IrisValue) * field_count);
  for (int i = 0; i < field_count; i++) {
    c->fields[i].type = VAL_INT;
    c->fields[i].as.num = 0;
  }
  return c;
}

void iris_class_incref(IrisClass *c) {
  if (c)
    c->refcount++;
}

void iris_class_decref_simple(IrisClass *c) {
  if (c && --c->refcount == 0) {
    for (int i = 0; i < c->field_count; i++) {
      iris_value_decref(&c->fields[i]);
    }
    free(c->fields);
    free(c);
  }
}

IrisList *iris_list_new(int capacity) {
  IrisList *list = malloc(sizeof(IrisList));
  list->capacity = capacity > 0 ? capacity : 8;
  list->items = malloc(sizeof(IrisValue) * list->capacity);
  list->length = 0;
  list->refcount = 1;
  return list;
}

void iris_list_incref(IrisList *list) {
  if (list)
    list->refcount++;
}

void iris_list_decref(IrisList *list) {
  if (list && --list->refcount == 0) {
    for (int i = 0; i < list->length; i++) {
      iris_value_decref(&list->items[i]);
    }
    free(list->items);
    free(list);
  }
}

void iris_list_push(IrisList *list, IrisValue v) {
  if (list->length >= list->capacity) {
    list->capacity *= 2;
    list->items = realloc(list->items, sizeof(IrisValue) * list->capacity);
  }
  list->items[list->length++] = v;
}

IrisValue iris_list_pop(IrisList *list) {
  if (list->length == 0) {
    IrisValue v;
    v.type = VAL_INT;
    v.as.num = 0;
    return v;
  }
  return list->items[--list->length];
}

IrisValue iris_list(IrisList *list) {
  IrisValue v;
  v.type = VAL_LIST;
  v.as.list = list;
  return v;
}

IrisValue iris_class(IrisClass *c) {
  IrisValue v;
  v.type = VAL_CLASS;
  v.as.object = c;
  return v;
}

IrisValue iris_int(int64_t n) {
  IrisValue v;
  v.type = VAL_INT;
  v.as.num = n;
  return v;
}

IrisValue iris_bool(bool b) {
  IrisValue v;
  v.type = VAL_BOOL;
  v.as.boolean = b;
  return v;
}

IrisValue iris_string(IrisString *s) {
  IrisValue v;
  v.type = VAL_STRING;
  v.as.str = s;
  return v;
}

IrisValue iris_struct(IrisStruct *s) {
  IrisValue v;
  v.type = VAL_STRUCT;
  v.as.instance = s;
  return v;
}

IrisValue iris_func(int func_idx) {
  IrisValue v;
  v.type = VAL_FUNC;
  v.as.func_idx = func_idx;
  return v;
}

IrisFuture *iris_future_new(void) {
  IrisFuture *f = malloc(sizeof(IrisFuture));
  f->refcount = 1;
  f->state = FUTURE_PENDING;
  f->result.type = VAL_INT;
  f->result.as.num = 0;
  f->callbacks = NULL;
  f->chained = NULL;
  f->on_resolve_func = -1;
  f->on_reject_func = -1;
  f->on_finally_func = -1;
  f->combinator = NULL;
  f->combinator_type = 0;
  return f;
}

void iris_future_incref(IrisFuture *f) {
  if (f)
    f->refcount++;
}

void iris_future_decref(IrisFuture *f) {
  if (f && --f->refcount == 0) {
    iris_value_decref(&f->result);
    FutureCallback *cb = f->callbacks;
    while (cb) {
      FutureCallback *next = cb->next;
      free(cb);
      cb = next;
    }
    if (f->chained)
      iris_future_decref(f->chained);
    if (f->combinator && f->combinator_type > 0) {
      iris_list_decref((IrisList *)f->combinator);
    }
    free(f);
  }
}

void iris_future_handle(IrisFuture *f, IrisValue value, FutureState state) {
  if (f->state != FUTURE_PENDING)
    return; /** already handled */
  f->state = state;
  iris_value_incref(&value);
  f->result = value;

  /* wake up and defer waiting tasks */
  if (g_event_loop) {
    FutureCallback *cb = f->callbacks;
    while (cb) {
      /* find task by id and schedule it */
      for (int i = 0; i < g_event_loop->task_count; i++) {
        if (g_event_loop->all_tasks[i]->id == cb->task_id) {
          event_loop_schedule_task(g_event_loop, g_event_loop->all_tasks[i]);
          break;
        }
      }
      FutureCallback *next = cb->next;
      free(cb);
      cb = next;
    }
    f->callbacks = NULL;
  }
}

void iris_future_resolve(IrisFuture *f, IrisValue value) {
  iris_future_handle(f, value, FUTURE_RESOLVED);
}

void iris_future_reject(IrisFuture *f, IrisValue reason) {
  iris_future_handle(f, reason, FUTURE_REJECTED);
}

IrisValue iris_future(IrisFuture *f) {
  IrisValue v;
  v.type = VAL_FUTURE;
  v.as.future = f;
  return v;
}

IrisTask *iris_task_new(int id) {
  IrisTask *t = malloc(sizeof(IrisTask));
  t->id = id;
  t->refcount = 1;
  t->state = TASK_READY;
  t->ip = 0;
  t->bp = 0;
  t->sp = 0;
  t->csp = 0;
  t->handler_count = 0;
  t->future = iris_future_new();
  t->awaiting = NULL;
  memset(t->stack, 0, sizeof(t->stack));
  memset(t->locals, 0, sizeof(t->locals));
  return t;
}

void iris_task_incref(IrisTask *t) {
  if (t)
    t->refcount++;
}

void iris_task_decref(IrisTask *t) {
  if (t && --t->refcount == 0) {
    for (int i = 0; i < t->sp; i++) {
      iris_value_decref(&t->stack[i]);
    }
    for (int i = 0; i < 256; i++) {
      iris_value_decref(&t->locals[i]);
    }
    if (t->future)
      iris_future_decref(t->future);
    if (t->awaiting)
      iris_future_decref(t->awaiting);
    free(t);
  }
}

void iris_task_save_state(IrisTask *t, VM *vm) {
  t->ip = vm->ip;
  t->bp = vm->bp;
  t->sp = vm->sp;
  t->csp = vm->csp;
  t->handler_count = vm->handler_count;

  /* cp entire stack */
  for (int i = 0; i < vm->sp; i++) {
    iris_value_incref(&vm->stack[i]);
    t->stack[i] = vm->stack[i];
  }

  /* also copy all locals */
  for (int i = 0; i < 256; i++) {
    iris_value_incref(&vm->locals[i]);
    t->locals[i] = vm->locals[i];
  }

  /* then cp the call stack  */
  memcpy(t->call_stack, vm->call_stack, sizeof(vm->call_stack));
  memcpy(t->bp_stack, vm->bp_stack, sizeof(vm->bp_stack));
  memcpy(t->sp_stack, vm->sp_stack, sizeof(vm->sp_stack));
  memcpy(t->frames, vm->frames, sizeof(vm->frames));
  memcpy(t->handlers, vm->handlers, sizeof(vm->handlers));
}

void iris_task_restore_state(IrisTask *t, VM *vm) {
  vm->ip = t->ip;
  vm->bp = t->bp;
  vm->sp = t->sp;
  vm->csp = t->csp;
  vm->handler_count = t->handler_count;

  /* restore stack */
  for (int i = 0; i < t->sp; i++) {
    iris_value_incref(&t->stack[i]);
    vm->stack[i] = t->stack[i];
  }

  /* restore locals */
  for (int i = 0; i < 256; i++) {
    iris_value_incref(&t->locals[i]);
    vm->locals[i] = t->locals[i];
  }

  /* restore call stack */
  memcpy(vm->call_stack, t->call_stack, sizeof(vm->call_stack));
  memcpy(vm->bp_stack, t->bp_stack, sizeof(vm->bp_stack));
  memcpy(vm->sp_stack, t->sp_stack, sizeof(vm->sp_stack));
  memcpy(vm->frames, t->frames, sizeof(vm->frames));
  memcpy(vm->handlers, t->handlers, sizeof(vm->handlers));
}

EventLoop *event_loop_new(void) {
  EventLoop *loop = malloc(sizeof(EventLoop));
  loop->ready_capacity = 16;
  loop->ready_queue = malloc(sizeof(IrisTask *) * loop->ready_capacity);
  loop->ready_count = 0;
  loop->task_capacity = 16;
  loop->all_tasks = malloc(sizeof(IrisTask *) * loop->task_capacity);
  loop->task_count = 0;
  loop->timers = NULL;
  loop->current_task = NULL;
  loop->next_task_id = 1;
  loop->running = false;
  return loop;
}

void event_loop_free(EventLoop *loop) {
  if (!loop)
    return;
  /* free all tasks */
  for (int i = 0; i < loop->task_count; i++) {
    iris_task_decref(loop->all_tasks[i]);
  }
  free(loop->all_tasks);
  free(loop->ready_queue);
  /* free timers */
  TimerEntry *timer = loop->timers;
  while (timer) {
    TimerEntry *next = timer->next;
    iris_future_decref(timer->future);
    free(timer);
    timer = next;
  }
  free(loop);
}

void event_loop_add_task(EventLoop *loop, IrisTask *task) {
  if (loop->task_count >= loop->task_capacity) {
    loop->task_capacity *= 2;
    loop->all_tasks =
        realloc(loop->all_tasks, sizeof(IrisTask *) * loop->task_capacity);
  }
  iris_task_incref(task);
  loop->all_tasks[loop->task_count++] = task;

  if (loop->ready_count >= loop->ready_capacity) {
    loop->ready_capacity *= 2;
    loop->ready_queue =
        realloc(loop->ready_queue, sizeof(IrisTask *) * loop->ready_capacity);
  }
  iris_task_incref(task);
  loop->ready_queue[loop->ready_count++] = task;
}

void event_loop_schedule_task(EventLoop *loop, IrisTask *task) {
  if (loop->ready_count >= loop->ready_capacity) {
    loop->ready_capacity *= 2;
    loop->ready_queue =
        realloc(loop->ready_queue, sizeof(IrisTask *) * loop->ready_capacity);
  }
  iris_task_incref(task);
  loop->ready_queue[loop->ready_count++] = task;
}

int64_t current_time_ms(void) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (int64_t)tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

/* add a timer that resolves a future after ms milliseconds */
void event_loop_add_timer(EventLoop *loop, int64_t ms, IrisFuture *future) {
  TimerEntry *entry = malloc(sizeof(TimerEntry));
  entry->trigger_time_ms = current_time_ms() + ms;
  entry->future = future;
  iris_future_incref(future);
  entry->next = NULL;

  /* insert into sorted list */
  if (!loop->timers || entry->trigger_time_ms < loop->timers->trigger_time_ms) {
    entry->next = loop->timers;
    loop->timers = entry;
  } else {
    TimerEntry *prev = loop->timers;
    while (prev->next &&
           prev->next->trigger_time_ms <= entry->trigger_time_ms) {
      prev = prev->next;
    }
    entry->next = prev->next;
    prev->next = entry;
  }
}

/* process expired timers, resolving their futures */
void event_loop_process_timers(EventLoop *loop) {
  int64_t now = current_time_ms();

  while (loop->timers && loop->timers->trigger_time_ms <= now) {
    TimerEntry *entry = loop->timers;
    loop->timers = entry->next;

    // Resolve the timer's future with 0
    IrisValue zero;
    zero.type = VAL_INT;
    zero.as.num = 0;
    iris_future_resolve(entry->future, zero);

    /* wake up tasks waiting on this future */
    FutureCallback *cb = entry->future->callbacks;
    while (cb) {
      /* find the task and schedule it */
      for (int i = 0; i < loop->task_count; i++) {
        if (loop->all_tasks[i]->id == cb->task_id &&
            loop->all_tasks[i]->state == TASK_SUSPENDED) {
          loop->all_tasks[i]->state = TASK_READY;
          event_loop_schedule_task(loop, loop->all_tasks[i]);
          break;
        }
      }
      cb = cb->next;
    }

    iris_future_decref(entry->future);
    free(entry);
  }
}

void execute(VM *vm, IrisProgram *prog);

/* run the event loop until all tasks complete */
void event_loop_run(EventLoop *loop, VM *vm, IrisProgram *prog) {
  loop->running = true;

  while (loop->running) {
    // Process expired timers
    event_loop_process_timers(loop);

    // Check if we have ready tasks
    if (loop->ready_count > 0) {
      // Dequeue a ready task
      IrisTask *task = loop->ready_queue[--loop->ready_count];

      loop->current_task = task;

      if (task->state == TASK_READY || task->state == TASK_SUSPENDED) {
        // Restore task state to VM
        iris_task_restore_state(task, vm);

        // If resuming from await, handle the awaited future's result
        if (task->awaiting) {
          if (task->awaiting->state == FUTURE_RESOLVED) {
            iris_value_incref(&task->awaiting->result);
            vm->stack[vm->sp++] = task->awaiting->result;
            iris_future_decref(task->awaiting);
            task->awaiting = NULL;
            task->state = TASK_RUNNING;
            execute(vm, prog);
          } else if (task->awaiting->state == FUTURE_REJECTED) {
            // Handle rejection by invoking exception mechanism
            IrisValue exc = task->awaiting->result;
            iris_value_incref(&exc);
            iris_future_decref(task->awaiting);
            task->awaiting = NULL;
            task->state = TASK_RUNNING;

            if (throw_exception(vm, prog, exc)) {
              // Handler found - push exception for catch block and execute
              vm->stack[vm->sp++] = vm->exception;
              vm->has_exception = false;
              execute(vm, prog);
            } else {
              // No handler - reject this task's future
              iris_future_reject(task->future, exc);
              task->state = TASK_FAILED;
              iris_value_decref(&exc);
            }
          } else {
            // Still pending - for combinators, the saved state has IP pointing
            // back to OP_AWAIT and the future on stack, so just execute
            task->state = TASK_RUNNING;
            iris_future_decref(task->awaiting);
            task->awaiting = NULL;
            execute(vm, prog);
          }
        } else {
          task->state = TASK_RUNNING;
          execute(vm, prog);
        }
      } else {
      }

      loop->current_task = NULL;
      iris_task_decref(task);
    } else if (loop->timers) {
      // No ready tasks, wait for next timer
      int64_t wait_ms = loop->timers->trigger_time_ms - current_time_ms();
      if (wait_ms > 0) {
        /* Use select() for sleeping to avoid relying on nanosleep/usleep
         * portability and feature-test macros. select() is declared via
         * <sys/time.h> which is already included.
         */
        struct timeval tv;
        tv.tv_sec = wait_ms / 1000;
        tv.tv_usec = (wait_ms % 1000) * 1000;
        select(0, NULL, NULL, NULL, &tv);
      }
    } else {
      // No ready tasks and no timers - we're done
      loop->running = false;
    }
  }
}

void iris_value_incref(IrisValue *v) {
  if (v->type == VAL_STRING)
    iris_string_incref(v->as.str);
  else if (v->type == VAL_STRUCT)
    iris_struct_incref(v->as.instance);
  else if (v->type == VAL_CLASS)
    iris_class_incref(v->as.object);
  else if (v->type == VAL_LIST)
    iris_list_incref(v->as.list);
  else if (v->type == VAL_FUTURE)
    iris_future_incref(v->as.future);
}

void iris_value_decref(IrisValue *v) {
  if (v->type == VAL_STRING)
    iris_string_decref(v->as.str);
  else if (v->type == VAL_STRUCT)
    iris_struct_decref(v->as.instance);
  else if (v->type == VAL_CLASS)
    iris_class_decref_simple(v->as.object);
  else if (v->type == VAL_LIST)
    iris_list_decref(v->as.list);
  else if (v->type == VAL_FUTURE)
    iris_future_decref(v->as.future);
}

void native_print(VM *vm, int argc) {
  (void)argc;
  IrisValue v = vm->stack[--vm->sp];
  if (v.type == VAL_INT) {
    printf("%" PRId64 "\n", v.as.num);
  } else if (v.type == VAL_STRING) {
    printf("%s\n", v.as.str->data);
  } else if (v.type == VAL_BOOL) {
    printf("%s\n", v.as.boolean ? "true" : "false");
  } else if (v.type == VAL_STRUCT) {
    printf("<struct>\n");
  } else if (v.type == VAL_CLASS) {
    printf("<object>\n");
  } else if (v.type == VAL_LIST) {
    IrisList *list = v.as.list;
    printf("[");
    for (int i = 0; i < list->length; i++) {
      if (i > 0)
        printf(", ");
      IrisValue item = list->items[i];
      if (item.type == VAL_INT)
        printf("%" PRId64, item.as.num);
      else if (item.type == VAL_STRING)
        printf("\"%s\"", item.as.str->data);
      else if (item.type == VAL_BOOL)
        printf("%s", item.as.boolean ? "true" : "false");
      else if (item.type == VAL_LIST)
        printf("[...]");
      else
        printf("<value>");
    }
    printf("]\n");
  } else if (v.type == VAL_FUTURE) {
    IrisFuture *f = v.as.future;
    const char *state_str;
    switch (f->state) {
    case FUTURE_PENDING:
      state_str = "pending";
      break;
    case FUTURE_RESOLVED:
      state_str = "resolved";
      break;
    case FUTURE_REJECTED:
      state_str = "rejected";
      break;
    default:
      state_str = "unknown";
      break;
    }
    printf("<Future:%s>\n", state_str);
  } else if (v.type == VAL_FUNC) {
    printf("<function:%d>\n", v.as.func_idx);
  }
  iris_value_decref(&v);
}

void native_len(VM *vm, int argc) {
  (void)argc;
  IrisValue v = vm->stack[--vm->sp];
  int64_t len = 0;
  if (v.type == VAL_LIST) {
    len = v.as.list->length;
  } else if (v.type == VAL_STRING) {
    len = strlen(v.as.str->data);
  }
  iris_value_decref(&v);
  vm->stack[vm->sp++] = iris_int(len);
}

void native_push(VM *vm, int argc) {
  (void)argc;
  IrisValue val = vm->stack[--vm->sp];
  IrisValue list_val = vm->stack[--vm->sp];
  if (list_val.type == VAL_LIST) {
    iris_value_incref(&val);
    iris_list_push(list_val.as.list, val);
  }
  iris_value_decref(&val);
  iris_value_decref(&list_val);
}

void native_pop(VM *vm, int argc) {
  (void)argc;
  IrisValue list_val = vm->stack[--vm->sp];
  IrisValue result;
  result.type = VAL_INT;
  result.as.num = 0;
  if (list_val.type == VAL_LIST && list_val.as.list->length > 0) {
    result = iris_list_pop(list_val.as.list);
  }
  iris_value_decref(&list_val);
  vm->stack[vm->sp++] = result;
}

void native_peek(VM *vm, int argc) {
  (void)argc;
  IrisValue list_val = vm->stack[--vm->sp];
  IrisValue result;
  result.type = VAL_INT;
  result.as.num = 0;
  if (list_val.type == VAL_LIST && list_val.as.list->length > 0) {
    result = list_val.as.list->items[list_val.as.list->length - 1];
    iris_value_incref(&result);
  }
  iris_value_decref(&list_val);
  vm->stack[vm->sp++] = result;
}

void native_sleep(VM *vm, int argc) {
  (void)argc;
  IrisValue ms_val = vm->stack[--vm->sp];
  int64_t ms = 0;
  if (ms_val.type == VAL_INT) {
    ms = ms_val.as.num;
  }
  iris_value_decref(&ms_val);
  IrisFuture *future = iris_future_new();
  if (g_event_loop) {
    event_loop_add_timer(g_event_loop, ms, future);
  } else {
    iris_future_resolve(future, iris_int(0));
  }
  vm->stack[vm->sp++] = iris_future(future);
}

void native_future_ready(VM *vm, int argc) {
  (void)argc;
  IrisValue value = vm->stack[--vm->sp];
  IrisFuture *f = iris_future_new();
  iris_future_resolve(f, value);
  iris_value_decref(&value);
  vm->stack[vm->sp++] = iris_future(f);
}

void native_future_failed(VM *vm, int argc) {
  (void)argc;
  IrisValue reason = vm->stack[--vm->sp];
  IrisFuture *f = iris_future_new();
  iris_future_reject(f, reason);
  iris_value_decref(&reason);
  vm->stack[vm->sp++] = iris_future(f);
}

void native_all(VM *vm, int argc) {
  (void)argc;
  IrisValue arr_val = vm->stack[--vm->sp];

  if (arr_val.type != VAL_LIST) {
    iris_value_decref(&arr_val);
    IrisFuture *f = iris_future_new();
    IrisString *err = iris_string_copy("all() requires an array argument");
    iris_future_reject(f, iris_string(err));
    vm->stack[vm->sp++] = iris_future(f);
    return;
  }

  IrisList *arr = arr_val.as.list;
  int count = arr->length;
  int all_resolved = 1;
  int any_rejected = 0;
  int rejected_idx = -1;

  for (int i = 0; i < count; i++) {
    IrisValue item = arr->items[i];
    if (item.type == VAL_FUTURE) {
      IrisFuture *f = item.as.future;
      if (f->state == FUTURE_REJECTED) {
        any_rejected = 1;
        rejected_idx = i;
        break;
      } else if (f->state != FUTURE_RESOLVED) {
        all_resolved = 0;
      }
    }
  }

  if (any_rejected) {
    IrisFuture *rejected_f = arr->items[rejected_idx].as.future;
    IrisFuture *combined = iris_future_new();
    iris_value_incref(&rejected_f->result);
    iris_future_reject(combined, rejected_f->result);
    iris_value_decref(&arr_val);
    vm->stack[vm->sp++] = iris_future(combined);
    return;
  }

  if (all_resolved) {
    IrisList *result = iris_list_new(count > 0 ? count : 1);
    for (int i = 0; i < count; i++) {
      IrisValue item = arr->items[i];
      if (item.type == VAL_FUTURE) {
        iris_value_incref(&item.as.future->result);
        iris_list_push(result, item.as.future->result);
      } else {
        iris_value_incref(&item);
        iris_list_push(result, item);
      }
    }
    IrisFuture *combined = iris_future_new();
    IrisValue result_val;
    result_val.type = VAL_LIST;
    result_val.as.list = result;
    iris_future_resolve(combined, result_val);
    iris_value_decref(&arr_val);
    vm->stack[vm->sp++] = iris_future(combined);
    return;
  }

  IrisFuture *combined = iris_future_new();
  combined->combinator =
      arr; /* store array directly (takes ownership of ref) */
  combined->combinator_type = 1; /* 1 = all */
  iris_list_incref(arr);         /* keep array alive */
  iris_value_decref(&arr_val);   /* release original ref */
  vm->stack[vm->sp++] = iris_future(combined);
}

void native_race(VM *vm, int argc) {
  (void)argc;
  IrisValue arr_val = vm->stack[--vm->sp];

  if (arr_val.type != VAL_LIST) {
    iris_value_decref(&arr_val);
    IrisFuture *f = iris_future_new();
    IrisString *err = iris_string_copy("race() requires an array argument");
    iris_future_reject(f, iris_string(err));
    vm->stack[vm->sp++] = iris_future(f);
    return;
  }
  IrisList *arr = arr_val.as.list;
  int count = arr->length;

  for (int i = 0; i < count; i++) {
    IrisValue item = arr->items[i];
    if (item.type == VAL_FUTURE) {
      IrisFuture *f = item.as.future;
      if (f->state == FUTURE_RESOLVED) {
        IrisFuture *combined = iris_future_new();
        iris_value_incref(&f->result);
        iris_future_resolve(combined, f->result);
        iris_value_decref(&arr_val);
        vm->stack[vm->sp++] = iris_future(combined);
        return;
      } else if (f->state == FUTURE_REJECTED) {
        IrisFuture *combined = iris_future_new();
        iris_value_incref(&f->result);
        iris_future_reject(combined, f->result);
        iris_value_decref(&arr_val);
        vm->stack[vm->sp++] = iris_future(combined);
        return;
      }
    } else {
      IrisFuture *combined = iris_future_new();
      iris_value_incref(&item);
      iris_future_resolve(combined, item);
      iris_value_decref(&arr_val);
      vm->stack[vm->sp++] = iris_future(combined);
      return;
    }
  }

  IrisFuture *combined = iris_future_new();
  combined->combinator = arr;
  combined->combinator_type = 2; /* 2 = race */
  iris_list_incref(arr);
  iris_value_decref(&arr_val);
  vm->stack[vm->sp++] = iris_future(combined);
}

void advance(Lexer *lex) {
  /* skip whitespace */
  while (isspace(lex->src[lex->pos])) {
    if (lex->src[lex->pos] == '\n')
      lex->line++;
    lex->pos++;
  }
  /* skip // comments */
  if (lex->src[lex->pos] == '/' && lex->src[lex->pos + 1] == '/') {
    while (lex->src[lex->pos] && lex->src[lex->pos] != '\n') {
      lex->pos++;
    }
    advance(lex);
    return;
  }
  /* record line number in token */
  lex->current.line = lex->line;
  if (!lex->src[lex->pos]) {
    lex->current.type = TOK_EOF;
    return;
  }
  char c = lex->src[lex->pos];
  if (isdigit(c)) {
    lex->current.type = TOK_NUMBER;
    lex->current.num = 0;
    while (isdigit(lex->src[lex->pos])) {
      lex->current.num = lex->current.num * 10 + (lex->src[lex->pos] - '0');
      lex->pos++;
    }
    return;
  }

  /* allow fstring to take precedence here */
  if (c == 'f' &&
      (lex->src[lex->pos + 1] == '"' || lex->src[lex->pos + 1] == '\'')) {
    lex->pos++;
    char quote = lex->src[lex->pos];
    lex->pos++;
    lex->current.type = TOK_FSTRING;
    int i = 0;

    while (lex->src[lex->pos] && lex->src[lex->pos] != quote) {
      if (lex->src[lex->pos] == '\\' && lex->src[lex->pos + 1]) {
        lex->pos++;
        switch (lex->src[lex->pos]) {
        case 'n':
          lex->current.str[i++] = '\n';
          break;
        case 't':
          lex->current.str[i++] = '\t';
          break;
        case 'r':
          lex->current.str[i++] = '\r';
          break;
        case '\\':
          lex->current.str[i++] = '\\';
          break;
        case '\'':
          lex->current.str[i++] = '\'';
          break;
        case '"':
          lex->current.str[i++] = '"';
          break;
        default:
          lex->current.str[i++] = lex->src[lex->pos];
          break;
        }
      } else {
        lex->current.str[i++] = lex->src[lex->pos];
      }
      lex->pos++;
    }
    lex->current.str[i] = '\0';

    if (lex->src[lex->pos] == quote) {
      lex->pos++;
    } else {
      lexer_error(lex->line, lex->pos, "unterminated f-string literal");
    }
    return;
  }
  /* parse string tokens */
  if (c == '"' || c == '\'') {
    char quote = c;
    lex->pos++;
    lex->current.type = TOK_STRING;
    int i = 0;

    while (lex->src[lex->pos] && lex->src[lex->pos] != quote) {
      if (lex->src[lex->pos] == '\\' && lex->src[lex->pos + 1]) {
        lex->pos++;
        switch (lex->src[lex->pos]) {
        case 'n':
          lex->current.str[i++] = '\n';
          break;
        case 't':
          lex->current.str[i++] = '\t';
          break;
        case 'r':
          lex->current.str[i++] = '\r';
          break;
        case '\\':
          lex->current.str[i++] = '\\';
          break;
        case '\'':
          lex->current.str[i++] = '\'';
          break;
        case '"':
          lex->current.str[i++] = '"';
          break;
        case '0':
          lex->current.str[i++] = '\0';
          break;
        default:
          lex->current.str[i++] = lex->src[lex->pos];
          break;
        }
      } else {
        lex->current.str[i++] = lex->src[lex->pos];
      }
      lex->pos++;
    }
    lex->current.str[i] = '\0';

    if (lex->src[lex->pos] == quote) {
      lex->pos++;
    } else {
      lexer_error(lex->line, lex->pos, "unterminated string literal");
    }
    return;
  }

  /* parse identity, allowing underscores in naming */
  if (isalpha(c) || c == '_') {
    lex->current.type = TOK_IDENT;
    int i = 0;
    while (isalnum(lex->src[lex->pos]) || lex->src[lex->pos] == '_') {
      lex->current.str[i++] = lex->src[lex->pos++];
    }
    lex->current.str[i] = 0;

    struct {
      const char *k;
      int v;
    } map[] = {
        {"function", TOK_FN},   {"return", TOK_RETURN},
        {"if", TOK_IF},         {"else", TOK_ELSE},
        {"mut", TOK_MUT},       {"const", TOK_CONST},
        {"for", TOK_FOR},       {"while", TOK_WHILE},
        {"in", TOK_IN},         {"true", TOK_TRUE},
        {"false", TOK_FALSE},   {"struct", TOK_STRUCT},
        {"class", TOK_CLASS},   {"new", TOK_NEW},
        {"try", TOK_TRY},       {"catch", TOK_CATCH},
        {"throw", TOK_THROW},   {"import", TOK_IMPORT},
        {"export", TOK_EXPORT}, {"from", TOK_FROM},
        {"break", TOK_BREAK},   {"continue", TOK_CONTINUE},
        {"async", TOK_ASYNC},   {"await", TOK_AWAIT},
    };

    for (size_t i = 0; i < sizeof map / sizeof *map; i++)
      if (!strcmp(lex->current.str, map[i].k)) {
        lex->current.type = map[i].v;
        break;
      }

    return;
  }

  lex->pos++;

  if (c == '.' && lex->src[lex->pos] == '.') {
    lex->current.type = TOK_RANGE;
    lex->pos++;
    return;
  } else if (c == '.') {
    lex->current.type = TOK_DOT;
    return;
  } else if (c == '=' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_EQ_EQ;
    lex->pos++;
    return;
  } else if (c == '>' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_GT_EQ;
    lex->pos++;
    return;
  } else if (c == '<' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_LT_EQ;
    lex->pos++;
    return;
  } else if (c == '!' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_NOT_EQ;
    lex->pos++;
    return;
  }
  /* three-char compound assignments (must check before << and >>) */
  else if (c == '<' && lex->src[lex->pos] == '<' &&
           lex->src[lex->pos + 1] == '=') {
    lex->current.type = TOK_SHL_EQ;
    lex->pos += 2;
    return;
  } else if (c == '>' && lex->src[lex->pos] == '>' &&
             lex->src[lex->pos + 1] == '=') {
    lex->current.type = TOK_SHR_EQ;
    lex->pos += 2;
    return;
  }
  /* two-char shift operators */
  else if (c == '<' && lex->src[lex->pos] == '<') {
    lex->current.type = TOK_SHL;
    lex->pos++;
    return;
  } else if (c == '>' && lex->src[lex->pos] == '>') {
    lex->current.type = TOK_SHR;
    lex->pos++;
    return;
  }
  /* two-char compound assignments */
  else if (c == '+' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_PLUS_EQ;
    lex->pos++;
    return;
  } else if (c == '-' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_MINUS_EQ;
    lex->pos++;
    return;
  } else if (c == '*' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_STAR_EQ;
    lex->pos++;
    return;
  } else if (c == '/' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_SLASH_EQ;
    lex->pos++;
    return;
  } else if (c == '%' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_PERCENT_EQ;
    lex->pos++;
    return;
  } else if (c == '&' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_AMP_EQ;
    lex->pos++;
    return;
  } else if (c == '|' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_PIPE_EQ;
    lex->pos++;
    return;
  } else if (c == '^' && lex->src[lex->pos] == '=') {
    lex->current.type = TOK_CARET_EQ;
    lex->pos++;
    return;
  }
  static const int map[128] = {
      ['('] = TOK_LPAREN, [')'] = TOK_RPAREN,   ['{'] = TOK_LBRACE,
      ['}'] = TOK_RBRACE, [','] = TOK_COMMA,    [';'] = TOK_SEMICOLON,
      ['+'] = TOK_PLUS,   ['-'] = TOK_MINUS,    ['*'] = TOK_STAR,
      ['/'] = TOK_SLASH,  ['='] = TOK_ASSIGN,   ['<'] = TOK_LT,
      ['>'] = TOK_GT,     [':'] = TOK_COLON,    ['%'] = TOK_PERCENT,
      ['&'] = TOK_AMP,    ['|'] = TOK_PIPE,     ['^'] = TOK_CARET,
      ['~'] = TOK_TILDE,  ['['] = TOK_LBRACKET, [']'] = TOK_RBRACKET,
  };

  if ((unsigned)c < 128 && map[(unsigned)c])
    lex->current.type = map[(unsigned)c];
  else {
    lexer_error(lex->line, lex->pos, "unexpected character '%c'", c);
  }
}

void expect(Lexer *lex, IrisTokenType type) {
  if (lex->current.type != type) {
    parser_error(lex, "expected %s, found %s", token_name(type),
                 token_name(lex->current.type));
  }
  advance(lex);
}

void emit(IrisProgram *prog, unsigned char byte) {
  if (prog->code_size >= prog->code_capacity) {
    prog->code_capacity *= 2;
    prog->code = realloc(prog->code, prog->code_capacity);
  }
  prog->code[prog->code_size++] = byte;
}

void emit_int(IrisProgram *prog, int val) {
  emit(prog, val & 0xFF);
  emit(prog, (val >> 8) & 0xFF);
  emit(prog, (val >> 16) & 0xFF);
  emit(prog, (val >> 24) & 0xFF);
}

void emit_int64(IrisProgram *prog, int64_t val) {
  for (int i = 0; i < 8; i++) {
    emit(prog, (val >> (i * 8)) & 0xFF);
  }
}

void patch_int(IrisProgram *prog, int addr, int val) {
  prog->code[addr] = val & 0xFF;
  prog->code[addr + 1] = (val >> 8) & 0xFF;
  prog->code[addr + 2] = (val >> 16) & 0xFF;
  prog->code[addr + 3] = (val >> 24) & 0xFF;
}

int add_string_constant(IrisProgram *prog, const char *str) {
  IrisString *s = iris_string_copy(str);
  prog->strings[prog->string_count] = s;
  return prog->string_count++;
}

int find_func(IrisProgram *prog, const char *name) {
  for (int i = 0; i < prog->func_count; i++) {
    if (strcmp(prog->funcs[i].name, name) == 0)
      return i;
  }
  return -1;
}

int find_local(IrisFunction *func, const char *name) {
  for (int i = 0; i < func->param_count; i++) {
    if (strcmp(func->params[i], name) == 0)
      return i;
  }
  for (int i = 0; i < func->local_count; i++) {
    if (strcmp(func->locals[i], name) == 0)
      return func->param_count + i;
  }
  return -1;
}

int find_struct_type(IrisProgram *prog, const char *name) {
  for (int i = 0; i < prog->struct_type_count; i++) {
    if (strcmp(prog->struct_types[i].name, name) == 0)
      return i;
  }
  return -1;
}

int find_struct_field(StructType *st, const char *name) {
  for (int i = 0; i < st->field_count; i++) {
    if (strcmp(st->fields[i], name) == 0)
      return i;
  }
  return -1;
}

int find_class_type(IrisProgram *prog, const char *name) {
  for (int i = 0; i < prog->class_type_count; i++) {
    if (strcmp(prog->class_types[i].name, name) == 0)
      return i;
  }
  return -1;
}

int find_class_field(ClassType *ct, const char *name) {
  for (int i = 0; i < ct->field_count; i++) {
    if (strcmp(ct->fields[i], name) == 0)
      return i;
  }
  return -1;
}

int find_class_method(ClassType *ct, const char *name) {
  for (int i = 0; i < ct->method_count; i++) {
    if (strcmp(ct->methods[i].name, name) == 0)
      return ct->methods[i].method_idx;
  }
  return -1;
}

void register_native(IrisProgram *prog, const char *name, NativeFunc fn,
                     int param_count, int returns_void) {
  strcpy(prog->funcs[prog->func_count].name, name);
  prog->funcs[prog->func_count].is_native = 1;
  prog->funcs[prog->func_count].native_fn = fn;
  prog->funcs[prog->func_count].param_count = param_count;
  prog->funcs[prog->func_count].returns_void = returns_void;
  prog->funcs[prog->func_count].addr = 0;
  prog->func_count++;
}

void compile_struct_definition(Lexer *lex, IrisProgram *prog) {
  expect(lex, TOK_STRUCT);

  if (lex->current.type != TOK_IDENT) {
    parser_error(lex, "expected struct name");
  }

  StructType *st = &prog->struct_types[prog->struct_type_count];
  strcpy(st->name, lex->current.str);
  st->field_count = 0;
  advance(lex);

  expect(lex, TOK_LBRACE);

  if (lex->current.type != TOK_RBRACE) {
    if (lex->current.type != TOK_IDENT) {
      parser_error(lex, "expected field name");
    }
    strcpy(st->fields[st->field_count++], lex->current.str);
    advance(lex);

    while (lex->current.type == TOK_COMMA) {
      advance(lex);
      if (lex->current.type != TOK_IDENT) {
        parser_error(lex, "expected field name after ','");
      }
      strcpy(st->fields[st->field_count++], lex->current.str);
      advance(lex);
    }
  }

  expect(lex, TOK_RBRACE);
  prog->struct_type_count++;
}

void compile_class_method(Lexer *lex, IrisProgram *prog, ClassType *ct,
                          int is_ctor, int is_dtor) {
  /*
   * method name is already consumed for regular methods
   * for ctor/dtor, the keyword is already consumed
   */

  IrisFunction *func = &prog->funcs[prog->func_count];
  /* build function name safely into fixed buffer */
  func->name[0] = '\0';
  strncpy(func->name, ct->name, sizeof func->name - 1);
  func->name[sizeof func->name - 1] = '\0';
  if (is_ctor) {
    strncat(func->name, "__ctor", sizeof func->name - strlen(func->name) - 1);
  } else if (is_dtor) {
    strncat(func->name, "__dtor", sizeof func->name - strlen(func->name) - 1);
  } else {
    strncat(func->name, "__", sizeof func->name - strlen(func->name) - 1);
    strncat(func->name, lex->current.str,
            sizeof func->name - strlen(func->name) - 1);
    advance(lex);
  }
  func->is_native = 0;
  func->native_fn = NULL;
  func->param_count = 0;
  func->local_count = 0;

  /* first parameter is always 'self' for methods */
  strcpy(func->params[func->param_count++], "self");

  expect(lex, TOK_LPAREN);

  if (lex->current.type != TOK_RPAREN) {
    strcpy(func->params[func->param_count++], lex->current.str);
    expect(lex, TOK_IDENT);

    while (lex->current.type == TOK_COMMA) {
      advance(lex);
      strcpy(func->params[func->param_count++], lex->current.str);
      expect(lex, TOK_IDENT);
    }
  }

  expect(lex, TOK_RPAREN);
  expect(lex, TOK_LBRACE);

  /* jmp over function code, we don't want to directly evaluate it */
  emit(prog, OP_JMP);
  int jump_patch_addr = prog->code_size;
  emit_int(prog, 0);

  func->addr = prog->code_size;
  int func_idx = prog->func_count;
  prog->func_count++;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, func);
  }

  /* default return for methods */
  emit(prog, OP_PUSH);
  emit_int64(prog, 0);
  emit(prog, OP_RETURN);

  expect(lex, TOK_RBRACE);
  patch_int(prog, jump_patch_addr, prog->code_size);

  /* register ctor method index inside class */
  if (is_ctor) {
    ct->ctor_idx = func_idx;
  } else if (is_dtor) {
    ct->dtor_idx = func_idx;
  } else {
    ct->methods[ct->method_count].method_idx = func_idx;
    /* name was already set - extract method name from func name */
    char *method_name = strrchr(func->name, '_');
    if (method_name && method_name > func->name && *(method_name - 1) == '_') {
      strcpy(ct->methods[ct->method_count].name, method_name + 1);
    }
    ct->method_count++;
  }
}

void compile_class_definition(Lexer *lex, IrisProgram *prog) {
  expect(lex, TOK_CLASS);

  if (lex->current.type != TOK_IDENT) {
    parser_error(lex, "expected class name");
  }

  ClassType *ct = &prog->class_types[prog->class_type_count];
  strcpy(ct->name, lex->current.str);
  ct->field_count = 0;
  ct->method_count = 0;
  ct->ctor_idx = -1;
  ct->dtor_idx = -1;
  advance(lex);

  expect(lex, TOK_LBRACE);

  /* parse fields first (comma-separated identifiers at the start, ending with a
   * new line) */
  if (lex->current.type == TOK_IDENT) {
    /*
     * check if this is a field list or a method
     * fields: field1, field2, ...
     * methods: name(...)
     * we need to peek ahead - save current state
     */
    /* check if next token after ident is comma or open paren */
    char first_ident[64];
    strcpy(first_ident, lex->current.str);
    advance(lex);

    if (lex->current.type == TOK_COMMA ||
        (lex->current.type != TOK_LPAREN && lex->current.type != TOK_RBRACE)) {
      /* it's a field list */
      strcpy(ct->fields[ct->field_count++], first_ident);

      while (lex->current.type == TOK_COMMA) {
        advance(lex);
        if (lex->current.type != TOK_IDENT) {
          parser_error(lex, "expected field name after ','");
        }
        strcpy(ct->fields[ct->field_count++], lex->current.str);
        advance(lex);
      }
    } else if (lex->current.type == TOK_LPAREN) {
      /* it's a method - compile it
       * Re-create the state by using first_ident as the method name */
      IrisFunction *func = &prog->funcs[prog->func_count];
      /* build function name safely into fixed buffer */
      func->name[0] = '\0';
      strncpy(func->name, ct->name, sizeof func->name - 1);
      func->name[sizeof func->name - 1] = '\0';
      strncat(func->name, "__", sizeof func->name - strlen(func->name) - 1);
      strncat(func->name, first_ident,
              sizeof func->name - strlen(func->name) - 1);
      func->is_native = 0;
      func->native_fn = NULL;
      func->param_count = 0;
      func->local_count = 0;

      /* again, first parameter is always 'self' */
      strcpy(func->params[func->param_count++], "self");
      expect(lex, TOK_LPAREN);

      if (lex->current.type != TOK_RPAREN) {
        strcpy(func->params[func->param_count++], lex->current.str);
        expect(lex, TOK_IDENT);

        while (lex->current.type == TOK_COMMA) {
          advance(lex);
          strcpy(func->params[func->param_count++], lex->current.str);
          expect(lex, TOK_IDENT);
        }
      }

      expect(lex, TOK_RPAREN);
      expect(lex, TOK_LBRACE);

      emit(prog, OP_JMP);
      int jump_patch_addr = prog->code_size;
      emit_int(prog, 0);

      func->addr = prog->code_size;
      int func_idx = prog->func_count;
      prog->func_count++;

      while (lex->current.type != TOK_RBRACE) {
        compile_statement(lex, prog, func);
      }

      /* ctors return self, others return 0 */
      if (strcmp(first_ident, "ctor") == 0) {
        emit(prog, OP_LOAD_LOCAL);
        emit_int(prog, 0); /* self is param 0 */
      } else {
        emit(prog, OP_PUSH);
        emit_int64(prog, 0);
      }
      emit(prog, OP_RETURN);

      expect(lex, TOK_RBRACE);
      patch_int(prog, jump_patch_addr, prog->code_size);

      /* check for ctor/dtor special names */
      if (strcmp(first_ident, "ctor") == 0) {
        ct->ctor_idx = func_idx;
      } else if (strcmp(first_ident, "dtor") == 0) {
        ct->dtor_idx = func_idx;
      } else {
        strcpy(ct->methods[ct->method_count].name, first_ident);
        ct->methods[ct->method_count].method_idx = func_idx;
        ct->method_count++;
      }
    }
  }

  /* parse methods (ctor, dtor, or regular methods) */
  while (lex->current.type != TOK_RBRACE) {
    if (lex->current.type == TOK_IDENT) {
      char method_name[64];
      strcpy(method_name, lex->current.str);
      advance(lex);

      if (lex->current.type != TOK_LPAREN) {
        parser_error(lex, "expected '(' after method name");
      }

      IrisFunction *func = &prog->funcs[prog->func_count];
      /* build function name safely into fixed buffer */
      func->name[0] = '\0';
      strncpy(func->name, ct->name, sizeof func->name - 1);
      func->name[sizeof func->name - 1] = '\0';
      strncat(func->name, "__", sizeof func->name - strlen(func->name) - 1);
      strncat(func->name, method_name,
              sizeof func->name - strlen(func->name) - 1);
      func->is_native = 0;
      func->native_fn = NULL;
      func->param_count = 0;
      func->local_count = 0;
      strcpy(func->params[func->param_count++], "self");
      expect(lex, TOK_LPAREN);
      if (lex->current.type != TOK_RPAREN) {
        strcpy(func->params[func->param_count++], lex->current.str);
        expect(lex, TOK_IDENT);

        while (lex->current.type == TOK_COMMA) {
          advance(lex);
          strcpy(func->params[func->param_count++], lex->current.str);
          expect(lex, TOK_IDENT);
        }
      }
      expect(lex, TOK_RPAREN);
      expect(lex, TOK_LBRACE);

      emit(prog, OP_JMP);
      int jump_patch_addr = prog->code_size;
      emit_int(prog, 0);

      func->addr = prog->code_size;
      int func_idx = prog->func_count;
      prog->func_count++;

      while (lex->current.type != TOK_RBRACE) {
        compile_statement(lex, prog, func);
      }

      if (strcmp(method_name, "ctor") == 0) {
        emit(prog, OP_LOAD_LOCAL);
        emit_int(prog, 0);
      } else {
        emit(prog, OP_PUSH);
        emit_int64(prog, 0);
      }
      emit(prog, OP_RETURN);

      expect(lex, TOK_RBRACE);
      patch_int(prog, jump_patch_addr, prog->code_size);

      // Check for ctor/dtor special names
      if (strcmp(method_name, "ctor") == 0) {
        ct->ctor_idx = func_idx;
      } else if (strcmp(method_name, "dtor") == 0) {
        ct->dtor_idx = func_idx;
      } else {
        strcpy(ct->methods[ct->method_count].name, method_name);
        ct->methods[ct->method_count].method_idx = func_idx;
        ct->method_count++;
      }
    } else {
      parser_error(lex, "expected method definition in class");
    }
  }

  expect(lex, TOK_RBRACE);
  prog->class_type_count++;
}

void compile_call(Lexer *lex, IrisProgram *prog, IrisFunction *current_func,
                  const char *func_name) {
  expect(lex, TOK_LPAREN);
  int func_idx = find_func(prog, func_name);
  if (func_idx < 0) {
    parser_error(lex, "unknown function '%s'", func_name);
  }
  IrisFunction *target = &prog->funcs[func_idx];
  int argc = 0;
  if (lex->current.type != TOK_RPAREN) {
    compile_comparison(lex, prog, current_func);
    argc++;
    while (lex->current.type == TOK_COMMA) {
      advance(lex);
      compile_comparison(lex, prog, current_func);
      argc++;
    }
  }
  expect(lex, TOK_RPAREN);
  if (argc != target->param_count) {
    parser_error(lex, "function '%s' expects %d argument(s), got %d", func_name,
                 target->param_count, argc);
  }
  if (target->is_async) {
    emit(prog, OP_ASYNC_CALL);
  } else {
    emit(prog, OP_CALL);
  }
  emit_int(prog, func_idx);
  int caller_frame_size = current_func->param_count + current_func->local_count;
  emit_int(prog, caller_frame_size);
}

void compile_primary(Lexer *lex, IrisProgram *prog,
                     IrisFunction *current_func) {
  if (lex->current.type == TOK_NEW) {
    advance(lex);
    if (lex->current.type != TOK_IDENT) {
      parser_error(lex, "expected class name after 'new'");
    }
    char class_name[64];
    strcpy(class_name, lex->current.str);
    advance(lex);
    int class_idx = find_class_type(prog, class_name);
    if (class_idx < 0) {
      parser_error(lex, "unknown class '%s'", class_name);
    }
    ClassType *ct = &prog->class_types[class_idx];
    expect(lex, TOK_LPAREN);

    int argc = 0;
    if (lex->current.type != TOK_RPAREN) {
      compile_comparison(lex, prog, current_func);
      argc++;

      while (lex->current.type == TOK_COMMA) {
        advance(lex);
        compile_comparison(lex, prog, current_func);
        argc++;
      }
    }
    expect(lex, TOK_RPAREN);
    emit(prog, OP_NEW_CLASS);
    emit_int(prog, class_idx);
    emit_int(prog, ct->field_count);
    emit_int(prog, ct->ctor_idx);
    emit_int(prog, argc);
    int caller_frame_size =
        current_func->param_count + current_func->local_count;
    emit_int(prog, caller_frame_size);

    while (lex->current.type == TOK_DOT) {
      advance(lex);
      if (lex->current.type != TOK_IDENT) {
        parser_error(lex, "expected method name after '.'");
      }
      char member_name[64];
      snprintf(member_name, sizeof(member_name), "%s", lex->current.str);
      advance(lex);

      if (lex->current.type == TOK_LPAREN) {
        advance(lex);
        int method_argc = 0;
        if (lex->current.type != TOK_RPAREN) {
          compile_comparison(lex, prog, current_func);
          method_argc++;
          while (lex->current.type == TOK_COMMA) {
            advance(lex);
            compile_comparison(lex, prog, current_func);
            method_argc++;
          }
        }
        expect(lex, TOK_RPAREN);

        int str_idx = add_string_constant(prog, member_name);
        emit(prog, OP_CALL_METHOD);
        emit_int(prog, str_idx);
        emit_int(prog, method_argc);
        int method_caller_frame_size =
            current_func->param_count + current_func->local_count;
        emit_int(prog, method_caller_frame_size);
      } else {
        int str_idx = add_string_constant(prog, member_name);
        emit(prog, OP_GET_FIELD);
        emit_int(prog, str_idx);
      }
    }
  } else if (lex->current.type == TOK_NUMBER) {
    emit(prog, OP_PUSH);
    emit_int64(prog, lex->current.num);
    advance(lex);
  } else if (lex->current.type == TOK_TRUE) {
    emit(prog, OP_PUSH_BOOL);
    emit(prog, 1);
    advance(lex);
  } else if (lex->current.type == TOK_FALSE) {
    emit(prog, OP_PUSH_BOOL);
    emit(prog, 0);
    advance(lex);
  } else if (lex->current.type == TOK_STRING) {
    int str_idx = add_string_constant(prog, lex->current.str);
    emit(prog, OP_PUSH_STRING);
    emit_int(prog, str_idx);
    advance(lex);
  } else if (lex->current.type == TOK_FSTRING) {
    const char *template = lex->current.str;
    int len = strlen(template);
    int part_count = 0;
    char literal_buf[256];
    int literal_len = 0;

    for (int i = 0; i < len; i++) {
      if (template[i] == '{') {
        if (i + 1 < len && template[i + 1] == '{') {
          literal_buf[literal_len++] = '{';
          i++;
          continue;
        }

        if (literal_len > 0) {
          literal_buf[literal_len] = '\0';
          int str_idx = add_string_constant(prog, literal_buf);
          emit(prog, OP_PUSH_STRING);
          emit_int(prog, str_idx);
          if (part_count > 0) {
            emit(prog, OP_STRING_CONCAT);
          }
          part_count++;
          literal_len = 0;
        }

        i++;
        char expr_buf[256];
        int expr_len = 0;
        int brace_depth = 1;
        while (i < len && brace_depth > 0) {
          if (template[i] == '{')
            brace_depth++;
          else if (template[i] == '}')
            brace_depth--;
          if (brace_depth > 0) {
            expr_buf[expr_len++] = template[i];
          }
          i++;
        }
        i--;
        expr_buf[expr_len] = '\0';
        Lexer sub_lex = {.src = expr_buf, .pos = 0};
        advance(&sub_lex);
        compile_comparison(&sub_lex, prog, current_func);

        emit(prog, OP_TO_STRING);

        if (part_count > 0) {
          emit(prog, OP_STRING_CONCAT);
        }
        part_count++;
      } else if (template[i] == '}') {
        if (i + 1 < len && template[i + 1] == '}') {
          literal_buf[literal_len++] = '}';
          i++;
          continue;
        }
        literal_buf[literal_len++] = template[i];
      } else {
        literal_buf[literal_len++] = template[i];
      }
    }

    if (literal_len > 0) {
      literal_buf[literal_len] = '\0';
      int str_idx = add_string_constant(prog, literal_buf);
      emit(prog, OP_PUSH_STRING);
      emit_int(prog, str_idx);
      if (part_count > 0) {
        emit(prog, OP_STRING_CONCAT);
      }
      part_count++;
    }
    if (part_count == 0) {
      int str_idx = add_string_constant(prog, "");
      emit(prog, OP_PUSH_STRING);
      emit_int(prog, str_idx);
    }
    advance(lex);
  } else if (lex->current.type == TOK_IDENT) {
    char name[64];
    strcpy(name, lex->current.str);
    advance(lex);

    int struct_idx = find_struct_type(prog, name);
    if (struct_idx >= 0) {
      StructType *st = &prog->struct_types[struct_idx];

      if (lex->current.type == TOK_LBRACE) {
        advance(lex);
        int field_set[16] = {0};

        if (lex->current.type != TOK_RBRACE) {
          if (lex->current.type != TOK_IDENT) {
            parser_error(lex, "expected field name");
          }
          char field_name[64];
          strcpy(field_name, lex->current.str);
          int field_idx = find_struct_field(st, field_name);
          if (field_idx < 0) {
            parser_error(lex, "unknown field '%s'", field_name);
          }
          advance(lex);
          expect(lex, TOK_COLON);

          compile_comparison(lex, prog, current_func);
          field_set[field_idx] = 1;

          int compiled_fields[16];
          int compiled_count = 1;
          compiled_fields[0] = field_idx;

          while (lex->current.type == TOK_COMMA) {
            advance(lex);
            if (lex->current.type != TOK_IDENT) {
              fprintf(stderr, "Expected field name\n");
              exit(1);
            }
            strcpy(field_name, lex->current.str);
            field_idx = find_struct_field(st, field_name);
            if (field_idx < 0) {
              parser_error(lex, "unknown field '%s'", field_name);
            }
            if (field_set[field_idx]) {
              parser_error(lex, "field '%s' already set", field_name);
            }
            advance(lex);
            expect(lex, TOK_COLON);
            compile_comparison(lex, prog, current_func);
            field_set[field_idx] = 1;
            compiled_fields[compiled_count++] = field_idx;
          }

          if (compiled_count != st->field_count) {
            parser_error(
                lex, "struct '%s' requires all %d field(s) to be initialized",
                st->name, st->field_count);
          }

          emit(prog, OP_NEW_STRUCT);
          emit_int(prog, struct_idx);
          emit_int(prog, st->field_count);
          for (int i = compiled_count - 1; i >= 0; i--) {
            emit_int(prog, compiled_fields[i]);
          }
        } else {
          emit(prog, OP_NEW_STRUCT);
          emit_int(prog, struct_idx);
          emit_int(prog, 0);
        }

        expect(lex, TOK_RBRACE);
      } else if (lex->current.type == TOK_LPAREN) {
        advance(lex);
        int argc = 0;
        if (lex->current.type != TOK_RPAREN) {
          compile_comparison(lex, prog, current_func);
          argc++;

          while (lex->current.type == TOK_COMMA) {
            advance(lex);
            compile_comparison(lex, prog, current_func);
            argc++;
          }
        }
        expect(lex, TOK_RPAREN);
        if (argc != st->field_count) {
          parser_error(lex, "struct '%s' expects %d field(s), got %d", st->name,
                       st->field_count, argc);
        }

        emit(prog, OP_NEW_STRUCT);
        emit_int(prog, struct_idx);
        emit_int(prog, st->field_count);
        for (int i = st->field_count - 1; i >= 0; i--) {
          emit_int(prog, i);
        }
      } else {
        parser_error(lex, "expected '(' or '{' after struct name");
      }
    } else if (lex->current.type == TOK_DOT) {
      /*
       * check if this could be a static method call (Type.method())
       * only treat as static method if name is not a local variable
       */
      int local_idx = find_local(current_func, name);
      if (local_idx < 0) {
        /* not a local variable, treat as static method: Type.method() */
        advance(lex);
        if (lex->current.type != TOK_IDENT) {
          parser_error(lex, "expected method name after '.'");
        }
        char method_name[64];
        snprintf(method_name, sizeof(method_name), "%s", lex->current.str);
        advance(lex);

        char static_func_name[128];
        snprintf(static_func_name, sizeof(static_func_name), "%s_%s", name,
                 method_name);

        /* check if it's a function call */
        if (lex->current.type == TOK_LPAREN) {
          compile_call(lex, prog, current_func, static_func_name);
          /* after static method call, handle chaining: Type.method().another()
           */
          while (lex->current.type == TOK_DOT) {
            advance(lex);
            if (lex->current.type != TOK_IDENT) {
              parser_error(lex, "expected method name after '.'");
            }
            char member_name[64];
            snprintf(member_name, sizeof(member_name), "%s", lex->current.str);
            advance(lex);

            if (lex->current.type == TOK_LPAREN) {
              advance(lex);
              int argc = 0;
              if (lex->current.type != TOK_RPAREN) {
                compile_comparison(lex, prog, current_func);
                argc++;
                while (lex->current.type == TOK_COMMA) {
                  advance(lex);
                  compile_comparison(lex, prog, current_func);
                  argc++;
                }
              }
              expect(lex, TOK_RPAREN);

              int str_idx = add_string_constant(prog, member_name);
              emit(prog, OP_CALL_METHOD);
              emit_int(prog, str_idx);
              emit_int(prog, argc);
              int caller_frame_size =
                  current_func->param_count + current_func->local_count;
              emit_int(prog, caller_frame_size);
            } else {
              int str_idx = add_string_constant(prog, member_name);
              emit(prog, OP_GET_FIELD);
              emit_int(prog, str_idx);
            }
          }
        } else {
          parser_error(lex, "expected '(' after static method name");
        }
      } else {
        /* it's a local variable with DOT, handle normally */
        emit(prog, OP_LOAD_LOCAL);
        emit_int(prog, local_idx);

        while (lex->current.type == TOK_DOT ||
               lex->current.type == TOK_LBRACKET) {
          if (lex->current.type == TOK_LBRACKET) {
            // Index access: arr[idx]
            advance(lex);
            compile_expr(lex, prog, current_func);
            expect(lex, TOK_RBRACKET);
            emit(prog, OP_LIST_GET);
          } else {
            // Dot access
            advance(lex);
            if (lex->current.type != TOK_IDENT) {
              parser_error(lex, "expected field or method name after '.'");
            }
            char member_name[64];
            snprintf(member_name, sizeof(member_name), "%s", lex->current.str);
            advance(lex);

            // Check if this is a method call
            if (lex->current.type == TOK_LPAREN) {
              // Method call: obj.method(args)
              advance(lex); // consume '('

              int argc = 0;
              if (lex->current.type != TOK_RPAREN) {
                compile_comparison(lex, prog, current_func);
                argc++;

                while (lex->current.type == TOK_COMMA) {
                  advance(lex);
                  compile_comparison(lex, prog, current_func);
                  argc++;
                }
              }

              expect(lex, TOK_RPAREN);

              // Emit method call
              int str_idx = add_string_constant(prog, member_name);
              emit(prog, OP_CALL_METHOD);
              emit_int(prog, str_idx); // method name
              emit_int(prog, argc);    // argument count (not including self)
              int caller_frame_size =
                  current_func->param_count + current_func->local_count;
              emit_int(prog, caller_frame_size);
            } else {
              // Field access
              int str_idx = add_string_constant(prog, member_name);
              emit(prog, OP_GET_FIELD);
              emit_int(prog, str_idx);
            }
          }
        }
      }
    } else if (lex->current.type == TOK_LPAREN) {
      compile_call(lex, prog, current_func, name);
      // After function call, handle method chaining: func().method()
      while (lex->current.type == TOK_DOT) {
        advance(lex);
        if (lex->current.type != TOK_IDENT) {
          parser_error(lex, "expected method name after '.'");
        }
        char member_name[64];
        snprintf(member_name, sizeof(member_name), "%s", lex->current.str);
        advance(lex);

        if (lex->current.type == TOK_LPAREN) {
          advance(lex);
          int argc = 0;
          if (lex->current.type != TOK_RPAREN) {
            compile_comparison(lex, prog, current_func);
            argc++;
            while (lex->current.type == TOK_COMMA) {
              advance(lex);
              compile_comparison(lex, prog, current_func);
              argc++;
            }
          }
          expect(lex, TOK_RPAREN);

          int str_idx = add_string_constant(prog, member_name);
          emit(prog, OP_CALL_METHOD);
          emit_int(prog, str_idx);
          emit_int(prog, argc);
          int caller_frame_size =
              current_func->param_count + current_func->local_count;
          emit_int(prog, caller_frame_size);
        } else {
          int str_idx = add_string_constant(prog, member_name);
          emit(prog, OP_GET_FIELD);
          emit_int(prog, str_idx);
        }
      }
    } else {
      // Plain identifier - must be a local variable or parameter
      int local_idx = find_local(current_func, name);
      if (local_idx >= 0) {
        emit(prog, OP_LOAD_LOCAL);
        emit_int(prog, local_idx);
      } else {
        parser_error(lex, "unknown variable '%s'", name);
      }
    }
  } else if (lex->current.type == TOK_LPAREN) {
    // Array literal: [expr, expr, ...]
    advance(lex);
    int element_count = 0;
    if (lex->current.type != TOK_RBRACKET) {
      compile_comparison(lex, prog, current_func);
      element_count++;
      while (lex->current.type == TOK_COMMA) {
        advance(lex);
        compile_comparison(lex, prog, current_func);
        element_count++;
      }
    }
    expect(lex, TOK_RBRACKET);
    emit(prog, OP_NEW_LIST);
    emit_int(prog, element_count);
  } else if (lex->current.type == TOK_FN || lex->current.type == TOK_ASYNC) {
    // Anonymous function expression: function(params) { body }
    // or: async function(params) { body }
    int is_async = (lex->current.type == TOK_ASYNC);
    advance(lex);

    if (is_async) {
      if (lex->current.type != TOK_FN) {
        parser_error(lex, "expected 'function' after 'async'");
      }
      advance(lex);
    }

    // Create anonymous function
    int func_idx = prog->func_count;
    IrisFunction *anon = &prog->funcs[prog->func_count++];
    memset(anon, 0, sizeof(IrisFunction));
    snprintf(anon->name, sizeof anon->name, "__anon_%d", func_idx);
    anon->is_async = is_async;

    expect(lex, TOK_LPAREN);

    // Parse parameters
    anon->param_count = 0;
    if (lex->current.type != TOK_RPAREN) {
      if (lex->current.type != TOK_IDENT) {
        parser_error(lex, "expected parameter name");
      }
      strcpy(anon->params[anon->param_count++], lex->current.str);
      advance(lex);

      while (lex->current.type == TOK_COMMA) {
        advance(lex);
        if (lex->current.type != TOK_IDENT) {
          parser_error(lex, "expected parameter name");
        }
        strcpy(anon->params[anon->param_count++], lex->current.str);
        advance(lex);
      }
    }
    expect(lex, TOK_RPAREN);

    // Compile function body
    expect(lex, TOK_LBRACE);

    // Emit jump to skip over function body during linear execution
    emit(prog, OP_JMP);
    int jump_patch_addr = prog->code_size;
    emit_int(prog, 0);

    anon->addr = prog->code_size;

    while (lex->current.type != TOK_RBRACE && lex->current.type != TOK_EOF) {
      compile_statement(lex, prog, anon);
    }
    expect(lex, TOK_RBRACE);

    // Add implicit return for functions that don't explicitly return
    if (is_async) {
      emit(prog, OP_PUSH);
      emit_int64(prog, 0);
      emit(prog, OP_TASK_RETURN);
    } else {
      emit(prog, OP_PUSH);
      emit_int64(prog, 0);
      emit(prog, OP_RETURN);
    }

    // Patch jump to skip over function body
    patch_int(prog, jump_patch_addr, prog->code_size);

    // Push function reference onto stack
    emit(prog, OP_PUSH_FUNC);
    emit_int(prog, func_idx);
  } else {
    parser_error(lex, "unexpected token %s in expression",
                 token_name(lex->current.type));
  }
}

// Unary operators: ~ (bitwise not), - (negation), and await
void compile_unary(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  if (lex->current.type == TOK_AWAIT) {
    if (!current_func->is_async) {
      parser_error(lex, "'await' can only be used inside async functions");
    }
    advance(lex);
    compile_unary(lex, prog, current_func); // Compile the future expression
    emit(prog, OP_AWAIT);
  } else if (lex->current.type == TOK_TILDE) {
    advance(lex);
    compile_unary(lex, prog, current_func);
    emit(prog, OP_BITNOT);
  } else if (lex->current.type == TOK_MINUS) {
    advance(lex);
    compile_unary(lex, prog, current_func);
    emit(prog, OP_NEGATE);
  } else {
    compile_primary(lex, prog, current_func);
  }
}

void compile_multiplicative(Lexer *lex, IrisProgram *prog,
                            IrisFunction *current_func) {
  compile_unary(lex, prog, current_func);

  while (lex->current.type == TOK_STAR || lex->current.type == TOK_SLASH ||
         lex->current.type == TOK_PERCENT) {
    IrisTokenType op = lex->current.type;
    advance(lex);
    compile_unary(lex, prog, current_func);

    if (op == TOK_STAR)
      emit(prog, OP_MUL);
    else if (op == TOK_SLASH)
      emit(prog, OP_DIV);
    else
      emit(prog, OP_MOD);
  }
}

void compile_additive(Lexer *lex, IrisProgram *prog,
                      IrisFunction *current_func) {
  compile_multiplicative(lex, prog, current_func);

  while (lex->current.type == TOK_PLUS || lex->current.type == TOK_MINUS) {
    IrisTokenType op = lex->current.type;
    advance(lex);
    compile_multiplicative(lex, prog, current_func);

    if (op == TOK_PLUS)
      emit(prog, OP_ADD);
    else
      emit(prog, OP_SUB);
  }
}

// Bit shift operators: << >>
void compile_shift(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  compile_additive(lex, prog, current_func);

  while (lex->current.type == TOK_SHL || lex->current.type == TOK_SHR) {
    IrisTokenType op = lex->current.type;
    advance(lex);
    compile_additive(lex, prog, current_func);

    if (op == TOK_SHL)
      emit(prog, OP_SHL);
    else
      emit(prog, OP_SHR);
  }
}

// Bitwise AND: &
void compile_bitand(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  compile_shift(lex, prog, current_func);

  while (lex->current.type == TOK_AMP) {
    advance(lex);
    compile_shift(lex, prog, current_func);
    emit(prog, OP_BITAND);
  }
}

// Bitwise XOR: ^
void compile_bitxor(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  compile_bitand(lex, prog, current_func);

  while (lex->current.type == TOK_CARET) {
    advance(lex);
    compile_bitand(lex, prog, current_func);
    emit(prog, OP_BITXOR);
  }
}

// Bitwise OR: |
void compile_bitor(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  compile_bitxor(lex, prog, current_func);

  while (lex->current.type == TOK_PIPE) {
    advance(lex);
    compile_bitxor(lex, prog, current_func);
    emit(prog, OP_BITOR);
  }
}

void compile_comparison(Lexer *lex, IrisProgram *prog,
                        IrisFunction *current_func) {
  compile_bitor(lex, prog, current_func);

  if (lex->current.type == TOK_EQ_EQ || lex->current.type == TOK_NOT_EQ ||
      lex->current.type == TOK_LT || lex->current.type == TOK_GT ||
      lex->current.type == TOK_LT_EQ || lex->current.type == TOK_GT_EQ) {

    IrisTokenType op = lex->current.type;
    advance(lex);
    compile_bitor(lex, prog, current_func);

    if (op == TOK_EQ_EQ)
      emit(prog, OP_EQ);
    else if (op == TOK_NOT_EQ)
      emit(prog, OP_NOT_EQ);
    else if (op == TOK_LT)
      emit(prog, OP_LT);
    else if (op == TOK_GT)
      emit(prog, OP_GT);
    else if (op == TOK_LT_EQ)
      emit(prog, OP_LT_EQ);
    else if (op == TOK_GT_EQ)
      emit(prog, OP_GT_EQ);
  }
}

void compile_expr(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  compile_comparison(lex, prog, current_func);
}

void compile_if(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  expect(lex, TOK_IF);
  expect(lex, TOK_LPAREN);
  compile_comparison(lex, prog, current_func);
  expect(lex, TOK_RPAREN);

  emit(prog, OP_JMP_IF_FALSE);
  int false_jump = prog->code_size;
  emit_int(prog, 0);

  expect(lex, TOK_LBRACE);

  current_func->current_scope++;
  int scope_start = current_func->local_count;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, current_func);
  }

  int locals_to_pop = 0;
  for (int i = current_func->local_count - 1; i >= scope_start; i--) {
    if (current_func->scope_depth[i] == current_func->current_scope) {
      current_func->local_count--;
      locals_to_pop++;
    }
  }
  current_func->current_scope--;

  expect(lex, TOK_RBRACE);

  if (lex->current.type == TOK_ELSE) {
    emit(prog, OP_JMP);
    int end_jump = prog->code_size;
    emit_int(prog, 0);

    patch_int(prog, false_jump, prog->code_size);

    advance(lex);
    expect(lex, TOK_LBRACE);

    current_func->current_scope++;
    scope_start = current_func->local_count;

    while (lex->current.type != TOK_RBRACE) {
      compile_statement(lex, prog, current_func);
    }

    for (int i = current_func->local_count - 1; i >= scope_start; i--) {
      if (current_func->scope_depth[i] == current_func->current_scope) {
        current_func->local_count--;
      }
    }
    current_func->current_scope--;

    expect(lex, TOK_RBRACE);

    patch_int(prog, end_jump, prog->code_size);
  } else {
    patch_int(prog, false_jump, prog->code_size);
  }
}

void compile_for(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  expect(lex, TOK_FOR);
  expect(lex, TOK_LPAREN);
  expect(lex, TOK_CONST);

  if (lex->current.type != TOK_IDENT) {
    parser_error(lex, "expected loop variable name");
  }
  char loop_var[64];
  strcpy(loop_var, lex->current.str);
  advance(lex);

  expect(lex, TOK_IN);

  // Compile the first expression (could be start of range or collection)
  compile_expr(lex, prog, current_func);

  if (lex->current.type == TOK_RANGE) {
    // Range-based for loop: for (const i in 0..10)
    advance(lex);
    compile_expr(lex, prog, current_func);
    expect(lex, TOK_RPAREN);

    strcpy(current_func->locals[current_func->local_count], "__end");
    current_func->is_mutable[current_func->local_count] = 0;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope;
    int end_local = current_func->param_count + current_func->local_count;
    current_func->local_count++;
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, end_local);

    strcpy(current_func->locals[current_func->local_count], "__iter");
    current_func->is_mutable[current_func->local_count] = 1;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope;
    int iter_local = current_func->param_count + current_func->local_count;
    current_func->local_count++;
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, iter_local);

    expect(lex, TOK_LBRACE);

    strcpy(current_func->locals[current_func->local_count], loop_var);
    current_func->is_mutable[current_func->local_count] = 0;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope + 1;
    int loop_var_local = current_func->param_count + current_func->local_count;
    current_func->local_count++;

    // Push loop context for break/continue
    LoopContext *loop = &current_func->loops[current_func->loop_depth++];
    loop->break_count = 0;
    loop->continue_count = 0;

    int loop_start = prog->code_size;

    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, iter_local);

    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, end_local);

    emit(prog, OP_LT);

    emit(prog, OP_JMP_IF_FALSE);
    int exit_jump = prog->code_size;
    emit_int(prog, 0);

    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, iter_local);
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, loop_var_local);

    current_func->current_scope++;

    while (lex->current.type != TOK_RBRACE) {
      compile_statement(lex, prog, current_func);
    }

    current_func->current_scope--;
    expect(lex, TOK_RBRACE);

    // Patch continue jumps to the increment section
    int continue_target = prog->code_size;
    for (int i = 0; i < loop->continue_count; i++) {
      patch_int(prog, loop->continue_patches[i], continue_target);
    }

    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, iter_local);
    emit(prog, OP_PUSH);
    emit_int64(prog, 1);
    emit(prog, OP_ADD);
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, iter_local);

    emit(prog, OP_JMP);
    emit_int(prog, loop_start);

    // Patch exit jump and all break jumps
    patch_int(prog, exit_jump, prog->code_size);
    for (int i = 0; i < loop->break_count; i++) {
      patch_int(prog, loop->break_patches[i], prog->code_size);
    }

    current_func->loop_depth--;
    current_func->local_count--;
    current_func->local_count--;
    current_func->local_count--;
  } else if (lex->current.type == TOK_RPAREN) {
    // Array-based for loop: for (const item in arr)
    advance(lex);

    // Store the collection in a hidden local
    strcpy(current_func->locals[current_func->local_count], "__collection");
    current_func->is_mutable[current_func->local_count] = 0;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope;
    int collection_local =
        current_func->param_count + current_func->local_count;
    current_func->local_count++;
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, collection_local);

    // Create index counter initialized to 0
    strcpy(current_func->locals[current_func->local_count], "__idx");
    current_func->is_mutable[current_func->local_count] = 1;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope;
    int idx_local = current_func->param_count + current_func->local_count;
    current_func->local_count++;
    emit(prog, OP_PUSH);
    emit_int64(prog, 0);
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, idx_local);

    expect(lex, TOK_LBRACE);

    // Create loop variable
    strcpy(current_func->locals[current_func->local_count], loop_var);
    current_func->is_mutable[current_func->local_count] = 0;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope + 1;
    int loop_var_local = current_func->param_count + current_func->local_count;
    current_func->local_count++;

    // Push loop context for break/continue
    LoopContext *loop = &current_func->loops[current_func->loop_depth++];
    loop->break_count = 0;
    loop->continue_count = 0;

    int loop_start = prog->code_size;

    // Check: __idx < len(__collection)
    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, idx_local);
    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, collection_local);
    emit(prog, OP_LIST_LEN);
    emit(prog, OP_LT);

    emit(prog, OP_JMP_IF_FALSE);
    int exit_jump = prog->code_size;
    emit_int(prog, 0);

    // loop_var = __collection[__idx]
    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, collection_local);
    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, idx_local);
    emit(prog, OP_LIST_GET);
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, loop_var_local);

    current_func->current_scope++;

    while (lex->current.type != TOK_RBRACE) {
      compile_statement(lex, prog, current_func);
    }

    current_func->current_scope--;
    expect(lex, TOK_RBRACE);

    // Continue target: the increment section
    loop->continue_target = prog->code_size;

    // __idx++
    emit(prog, OP_LOAD_LOCAL);
    emit_int(prog, idx_local);
    emit(prog, OP_PUSH);
    emit_int64(prog, 1);
    emit(prog, OP_ADD);
    emit(prog, OP_STORE_LOCAL);
    emit_int(prog, idx_local);

    emit(prog, OP_JMP);
    emit_int(prog, loop_start);

    // Patch exit jump and all break jumps
    patch_int(prog, exit_jump, prog->code_size);
    for (int i = 0; i < loop->break_count; i++) {
      patch_int(prog, loop->break_patches[i], prog->code_size);
    }

    // Patch continue jumps to increment section
    for (int i = 0; i < loop->continue_count; i++) {
      patch_int(prog, loop->continue_patches[i], loop->continue_target);
    }

    current_func->loop_depth--;
    current_func->local_count--;
    current_func->local_count--;
    current_func->local_count--;
  } else {
    parser_error(lex, "expected '..' or ')' in for loop");
  }
}

void compile_while(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  expect(lex, TOK_WHILE);
  expect(lex, TOK_LPAREN);

  // Push loop context for break/continue
  LoopContext *loop = &current_func->loops[current_func->loop_depth++];
  loop->break_count = 0;
  loop->continue_count = 0;

  int loop_start = prog->code_size;
  loop->continue_target = loop_start; // Continue goes back to condition

  compile_comparison(lex, prog, current_func);
  expect(lex, TOK_RPAREN);

  emit(prog, OP_JMP_IF_FALSE);
  int exit_jump = prog->code_size;
  emit_int(prog, 0);

  expect(lex, TOK_LBRACE);

  current_func->current_scope++;
  int scope_start = current_func->local_count;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, current_func);
  }

  // Clean up locals in this scope
  for (int i = current_func->local_count - 1; i >= scope_start; i--) {
    if (current_func->scope_depth[i] == current_func->current_scope) {
      current_func->local_count--;
    }
  }
  current_func->current_scope--;

  expect(lex, TOK_RBRACE);

  // Jump back to condition check
  emit(prog, OP_JMP);
  emit_int(prog, loop_start);

  // Patch exit jump and all break jumps
  patch_int(prog, exit_jump, prog->code_size);
  for (int i = 0; i < loop->break_count; i++) {
    patch_int(prog, loop->break_patches[i], prog->code_size);
  }

  // Patch continue jumps to loop start
  for (int i = 0; i < loop->continue_count; i++) {
    patch_int(prog, loop->continue_patches[i], loop->continue_target);
  }

  current_func->loop_depth--;
}

void compile_try(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  expect(lex, TOK_TRY);
  expect(lex, TOK_LBRACE);

  // Emit OP_TRY_BEGIN with placeholder for catch address
  emit_line(prog, lex->current.line);
  emit(prog, OP_TRY_BEGIN);
  int catch_addr_patch = prog->code_size;
  emit_int(prog, 0); // Will be patched with catch block address

  // Compile try block
  current_func->current_scope++;
  int scope_start = current_func->local_count;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, current_func);
  }

  // Clean up locals in try scope
  for (int i = current_func->local_count - 1; i >= scope_start; i--) {
    if (current_func->scope_depth[i] == current_func->current_scope) {
      current_func->local_count--;
    }
  }
  current_func->current_scope--;

  expect(lex, TOK_RBRACE);

  // Emit OP_TRY_END to remove exception handler
  emit(prog, OP_TRY_END);

  // Jump over catch block
  emit(prog, OP_JMP);
  int end_jump = prog->code_size;
  emit_int(prog, 0);

  // Patch catch address
  patch_int(prog, catch_addr_patch, prog->code_size);

  // Parse catch block
  expect(lex, TOK_CATCH);
  expect(lex, TOK_LPAREN);

  if (lex->current.type != TOK_IDENT) {
    parser_error(lex, "expected exception variable name in catch");
  }

  // Add exception variable to scope
  char exc_var[64];
  strcpy(exc_var, lex->current.str);
  advance(lex);
  expect(lex, TOK_RPAREN);
  expect(lex, TOK_LBRACE);

  // Create local variable for exception
  strcpy(current_func->locals[current_func->local_count], exc_var);
  current_func->is_mutable[current_func->local_count] = 0;
  current_func->scope_depth[current_func->local_count] =
      current_func->current_scope + 1;
  int exc_local = current_func->param_count + current_func->local_count;
  current_func->local_count++;

  // Exception value will be on stack when catch is entered
  // Store it in the exception variable
  emit(prog, OP_STORE_LOCAL);
  emit_int(prog, exc_local);

  current_func->current_scope++;
  scope_start = current_func->local_count;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, current_func);
  }

  // Clean up locals in catch scope
  for (int i = current_func->local_count - 1; i >= scope_start; i--) {
    if (current_func->scope_depth[i] == current_func->current_scope) {
      current_func->local_count--;
    }
  }
  // Also remove exception variable
  current_func->local_count--;
  current_func->current_scope--;

  expect(lex, TOK_RBRACE);

  // Patch end jump
  patch_int(prog, end_jump, prog->code_size);
}

void compile_throw(Lexer *lex, IrisProgram *prog, IrisFunction *current_func) {
  int throw_line = lex->current.line;
  expect(lex, TOK_THROW);

  emit_line(prog, throw_line);

  // Compile the expression to throw
  compile_expr(lex, prog, current_func);
  expect(lex, TOK_SEMICOLON);

  emit(prog, OP_THROW);
}

// Helper: Check if token is a compound assignment operator
static bool is_compound_assign(IrisTokenType type) {
  return type == TOK_PLUS_EQ || type == TOK_MINUS_EQ || type == TOK_STAR_EQ ||
         type == TOK_SLASH_EQ || type == TOK_PERCENT_EQ || type == TOK_AMP_EQ ||
         type == TOK_PIPE_EQ || type == TOK_CARET_EQ || type == TOK_SHL_EQ ||
         type == TOK_SHR_EQ;
}

// Helper: Emit the operation for compound assignment
static void emit_compound_op(IrisProgram *prog, IrisTokenType type) {
  switch (type) {
  case TOK_PLUS_EQ:
    emit(prog, OP_ADD);
    break;
  case TOK_MINUS_EQ:
    emit(prog, OP_SUB);
    break;
  case TOK_STAR_EQ:
    emit(prog, OP_MUL);
    break;
  case TOK_SLASH_EQ:
    emit(prog, OP_DIV);
    break;
  case TOK_PERCENT_EQ:
    emit(prog, OP_MOD);
    break;
  case TOK_AMP_EQ:
    emit(prog, OP_BITAND);
    break;
  case TOK_PIPE_EQ:
    emit(prog, OP_BITOR);
    break;
  case TOK_CARET_EQ:
    emit(prog, OP_BITXOR);
    break;
  case TOK_SHL_EQ:
    emit(prog, OP_SHL);
    break;
  case TOK_SHR_EQ:
    emit(prog, OP_SHR);
    break;
  default:
    break;
  }
}

void compile_statement(Lexer *lex, IrisProgram *prog,
                       IrisFunction *current_func) {
  // Record line info for this statement
  emit_line(prog, lex->current.line);

  if (lex->current.type == TOK_RETURN) {
    advance(lex);
    compile_expr(lex, prog, current_func);
    expect(lex, TOK_SEMICOLON);
    // Use OP_TASK_RETURN for async functions
    if (current_func->is_async) {
      emit(prog, OP_TASK_RETURN);
    } else {
      emit(prog, OP_RETURN);
    }
  } else if (lex->current.type == TOK_FOR) {
    compile_for(lex, prog, current_func);
  } else if (lex->current.type == TOK_WHILE) {
    compile_while(lex, prog, current_func);
  } else if (lex->current.type == TOK_TRY) {
    compile_try(lex, prog, current_func);
  } else if (lex->current.type == TOK_THROW) {
    compile_throw(lex, prog, current_func);
  } else if (lex->current.type == TOK_IF) {
    compile_if(lex, prog, current_func);
  } else if (lex->current.type == TOK_BREAK) {
    advance(lex);
    expect(lex, TOK_SEMICOLON);
    if (current_func->loop_depth == 0) {
      parser_error(lex, "'break' outside of loop");
    }
    LoopContext *loop = &current_func->loops[current_func->loop_depth - 1];
    emit(prog, OP_JMP);
    loop->break_patches[loop->break_count++] = prog->code_size;
    emit_int(prog, 0); // Will be patched
  } else if (lex->current.type == TOK_CONTINUE) {
    advance(lex);
    expect(lex, TOK_SEMICOLON);
    if (current_func->loop_depth == 0) {
      parser_error(lex, "'continue' outside of loop");
    }
    LoopContext *loop = &current_func->loops[current_func->loop_depth - 1];
    emit(prog, OP_JMP);
    loop->continue_patches[loop->continue_count++] = prog->code_size;
    emit_int(prog, 0); // Will be patched
  } else if (lex->current.type == TOK_CONST || lex->current.type == TOK_MUT) {
    int is_mut = (lex->current.type == TOK_MUT);
    advance(lex);

    if (lex->current.type != TOK_IDENT) {
      parser_error(lex, "expected variable name");
    }

    char name[64];
    strcpy(name, lex->current.str);
    strcpy(current_func->locals[current_func->local_count], name);
    current_func->is_mutable[current_func->local_count] = is_mut;
    current_func->scope_depth[current_func->local_count] =
        current_func->current_scope;
    int local_idx = current_func->param_count + current_func->local_count;
    current_func->local_count++;

    advance(lex);

    if (lex->current.type == TOK_ASSIGN) {
      advance(lex);
      compile_expr(lex, prog, current_func);
      emit(prog, OP_STORE_LOCAL);
      emit_int(prog, local_idx);
    } else {
      emit(prog, OP_PUSH);
      emit_int64(prog, 0);
      emit(prog, OP_STORE_LOCAL);
      emit_int(prog, local_idx);
    }

    expect(lex, TOK_SEMICOLON);
  } else if (lex->current.type == TOK_IDENT) {
    char name[64];
    strcpy(name, lex->current.str);
    advance(lex);

    if (lex->current.type == TOK_DOT) {
      int local_idx = find_local(current_func, name);
      if (local_idx < 0) {
        parser_error(lex, "unknown variable '%s'", name);
      }

      emit(prog, OP_LOAD_LOCAL);
      emit_int(prog, local_idx);

      advance(lex);
      if (lex->current.type != TOK_IDENT) {
        parser_error(lex, "expected field or method name after '.'");
      }

      char member_name[64];
      strcpy(member_name, lex->current.str);
      advance(lex);

      while (lex->current.type == TOK_DOT) {
        int str_idx = add_string_constant(prog, member_name);
        emit(prog, OP_GET_FIELD);
        emit_int(prog, str_idx);

        advance(lex);
        if (lex->current.type != TOK_IDENT) {
          parser_error(lex, "expected field name after '.'");
        }
        strcpy(member_name, lex->current.str);
        advance(lex);
      }

      if (lex->current.type == TOK_LPAREN) {
        // Method call: obj.method(args)
        advance(lex);

        int argc = 0;
        if (lex->current.type != TOK_RPAREN) {
          compile_comparison(lex, prog, current_func);
          argc++;

          while (lex->current.type == TOK_COMMA) {
            advance(lex);
            compile_comparison(lex, prog, current_func);
            argc++;
          }
        }

        expect(lex, TOK_RPAREN);
        expect(lex, TOK_SEMICOLON);

        int str_idx = add_string_constant(prog, member_name);
        emit(prog, OP_CALL_METHOD);
        emit_int(prog, str_idx);
        emit_int(prog, argc);
        int caller_frame_size =
            current_func->param_count + current_func->local_count;
        emit_int(prog, caller_frame_size);

        // Pop method return value (not used in statement)
        emit(prog, OP_POP);
      } else if (lex->current.type == TOK_ASSIGN ||
                 is_compound_assign(lex->current.type)) {
        // Field assignment: check const-ness
        if (local_idx >= current_func->param_count) {
          int var_idx = local_idx - current_func->param_count;
          if (!current_func->is_mutable[var_idx]) {
            parser_error(lex, "cannot modify field of const variable '%s'",
                         name);
          }
        }

        IrisTokenType assign_op = lex->current.type;
        advance(lex);

        // For compound assignment, duplicate object and get field value
        if (assign_op != TOK_ASSIGN) {
          emit(prog, OP_DUP); // Duplicate object reference
          int str_idx = add_string_constant(prog, member_name);
          emit(prog, OP_GET_FIELD);
          emit_int(prog, str_idx);
        }

        compile_expr(lex, prog, current_func);

        // Emit operation for compound assignment
        if (assign_op != TOK_ASSIGN) {
          emit_compound_op(prog, assign_op);
        }

        int str_idx = add_string_constant(prog, member_name);
        emit(prog, OP_SET_FIELD);
        emit_int(prog, str_idx);

        expect(lex, TOK_SEMICOLON);
      } else {
        parser_error(lex, "expected '(' or '=' after member name");
      }
    } else if (lex->current.type == TOK_LBRACKET) {
      // Index assignment: arr[idx] = value
      int local_idx = find_local(current_func, name);
      if (local_idx < 0) {
        parser_error(lex, "unknown variable '%s'", name);
      }

      // Check mutability
      if (local_idx >= current_func->param_count) {
        int var_idx = local_idx - current_func->param_count;
        if (!current_func->is_mutable[var_idx]) {
          parser_error(lex, "cannot modify const variable '%s'", name);
        }
      }

      advance(lex); // consume '['

      // Load the list
      emit(prog, OP_LOAD_LOCAL);
      emit_int(prog, local_idx);

      // Compile the index expression
      compile_expr(lex, prog, current_func);
      expect(lex, TOK_RBRACKET);

      if (lex->current.type == TOK_ASSIGN) {
        advance(lex);
        compile_expr(lex, prog, current_func);
        emit(prog, OP_LIST_SET);
        expect(lex, TOK_SEMICOLON);
      } else if (is_compound_assign(lex->current.type)) {
        IrisTokenType assign_op = lex->current.type;
        advance(lex);

        // Stack: [list, index]
        // Need to duplicate both and get current value
        emit(prog, OP_DUP2);     // Duplicate list and index
        emit(prog, OP_LIST_GET); // Get current value

        compile_expr(lex, prog, current_func); // Compile RHS
        emit_compound_op(prog, assign_op);     // Apply operation
        emit(prog, OP_LIST_SET);               // Set new value
        expect(lex, TOK_SEMICOLON);
      } else {
        parser_error(lex, "expected '=' or compound assignment after ']'");
      }
    } else if (lex->current.type == TOK_ASSIGN ||
               is_compound_assign(lex->current.type)) {
      IrisTokenType assign_op = lex->current.type;
      advance(lex);

      int local_idx = find_local(current_func, name);
      if (local_idx < 0) {
        parser_error(lex, "unknown variable '%s'", name);
      }

      // Check mutability
      if (local_idx >= current_func->param_count) {
        int var_idx = local_idx - current_func->param_count;
        if (!current_func->is_mutable[var_idx]) {
          parser_error(lex, "cannot assign to const variable '%s'", name);
        }
      }

      // For compound assignment, load current value first
      if (assign_op != TOK_ASSIGN) {
        emit(prog, OP_LOAD_LOCAL);
        emit_int(prog, local_idx);
      }

      compile_expr(lex, prog, current_func);

      // Emit operation for compound assignment
      if (assign_op != TOK_ASSIGN) {
        emit_compound_op(prog, assign_op);
      }

      emit(prog, OP_STORE_LOCAL);
      emit_int(prog, local_idx);
      expect(lex, TOK_SEMICOLON);
    } else if (lex->current.type == TOK_LPAREN) {
      int func_idx = find_func(prog, name);
      compile_call(lex, prog, current_func, name);
      expect(lex, TOK_SEMICOLON);

      if (func_idx >= 0 && !prog->funcs[func_idx].returns_void) {
        emit(prog, OP_POP);
      }
    } else {
      parser_error(lex, "expected '=', '.' or '(' after identifier");
    }
  } else if (lex->current.type == TOK_AWAIT) {
    // await expression as statement
    compile_expr(lex, prog, current_func); // compile_unary will handle await
    expect(lex, TOK_SEMICOLON);
    emit(prog, OP_POP); // Discard the result
  } else {
    parser_error(lex, "unexpected token %s at start of statement",
                 token_name(lex->current.type));
  }
}

void compile_function(Lexer *lex, IrisProgram *prog) {
  // Check for 'async' modifier
  int is_async = 0;
  if (lex->current.type == TOK_ASYNC) {
    is_async = 1;
    advance(lex);
  }

  expect(lex, TOK_FN);

  if (lex->current.type != TOK_IDENT) {
    parser_error(lex, "expected function name");
  }

  IrisFunction *func = &prog->funcs[prog->func_count];
  strcpy(func->name, lex->current.str);
  func->is_native = 0;
  func->native_fn = NULL;
  func->param_count = 0;
  func->local_count = 0;
  func->is_async = is_async;

  advance(lex);
  expect(lex, TOK_LPAREN);

  if (lex->current.type != TOK_RPAREN) {
    strcpy(func->params[func->param_count++], lex->current.str);
    expect(lex, TOK_IDENT);

    while (lex->current.type == TOK_COMMA) {
      advance(lex);
      strcpy(func->params[func->param_count++], lex->current.str);
      expect(lex, TOK_IDENT);
    }
  }

  expect(lex, TOK_RPAREN);
  expect(lex, TOK_LBRACE);

  emit(prog, OP_JMP);
  int jump_patch_addr = prog->code_size;
  emit_int(prog, 0);

  func->addr = prog->code_size;
  prog->func_count++;

  while (lex->current.type != TOK_RBRACE) {
    compile_statement(lex, prog, func);
  }

  // Default return: for async functions use OP_TASK_RETURN
  emit(prog, OP_PUSH);
  emit_int64(prog, 0);
  if (is_async) {
    emit(prog, OP_TASK_RETURN);
  } else {
    emit(prog, OP_RETURN);
  }

  expect(lex, TOK_RBRACE);
  patch_int(prog, jump_patch_addr, prog->code_size);
}

IrisProgram *compile_module(const char *path);
void compile_import(Lexer *lex, IrisProgram *prog);

void get_directory(const char *path, char *dir) {
  strcpy(dir, path);
  char *last_slash = strrchr(dir, '/');
  if (last_slash) {
    *(last_slash + 1) = '\0';
  } else {
    strcpy(dir, "./");
  }
}

void resolve_import_path(const char *current_file, const char *import_path,
                         char *resolved) {
  char dir[256];
  get_directory(current_file, dir);

  if (import_path[0] == '.' && import_path[1] == '/') {
    snprintf(resolved, 256, "%s%s", dir, import_path + 2);
  } else if (import_path[0] == '/') {
    strcpy(resolved, import_path);
  } else {
    snprintf(resolved, 256, "%s%s", dir, import_path);
  }

  if (!strstr(resolved, ".iris")) {
    strcat(resolved, ".iris");
  }
}

IrisModule *find_module(const char *path) {
  for (int i = 0; i < g_module_count; i++) {
    if (strcmp(g_modules[i].path, path) == 0) {
      return &g_modules[i];
    }
  }
  return NULL;
}

Export *find_export(IrisProgram *prog, const char *name) {
  for (int i = 0; i < prog->export_count; i++) {
    if (strcmp(prog->exports[i].name, name) == 0) {
      return &prog->exports[i];
    }
  }
  return NULL;
}

IrisProgram *compile_with_path(const char *source, const char *source_path) {
  IrisProgram *prog = malloc(sizeof(IrisProgram));
  prog->code = malloc(1024);
  prog->code_capacity = 1024;
  prog->code_size = 0;
  prog->func_count = 0;
  prog->string_count = 0;
  prog->struct_type_count = 0;
  prog->class_type_count = 0;
  prog->export_count = 0;
  strcpy(prog->source_path, source_path);

  // Initialize line tracking
  prog->lines = NULL;
  prog->line_count = 0;
  prog->line_capacity = 0;

  // Store source for error messages
  prog->source = NULL;
  if (source) {
    size_t _len = strlen(source);
    prog->source = malloc(_len + 1);
    if (prog->source)
      memcpy(prog->source, source, _len + 1);
  }

  register_native(prog, "print", native_print, 1, 1);
  register_native(prog, "len", native_len, 1, 0);
  register_native(prog, "push", native_push, 2, 1);
  register_native(prog, "pop", native_pop, 1, 0);
  register_native(prog, "peek", native_peek, 1, 0);
  register_native(prog, "sleep", native_sleep, 1, 0);
  register_native(prog, "all", native_all, 1, 0);
  register_native(prog, "race", native_race, 1, 0);
  register_native(prog, "Future_ready", native_future_ready, 1, 0);
  register_native(prog, "Future_failed", native_future_failed, 1, 0);
  register_native(prog, "Future_resolve", native_future_ready, 1,
                  0); // Alias for Future.resolve();

  // Register C functions from current IrisState if available
  if (g_current_iris_state) {
    IrisState *I = g_current_iris_state;
    for (int i = 0; i < I->cfunc_count; i++) {
      register_native(prog, I->cfuncs[i].name, iris_cfunc_bridge,
                      I->cfuncs[i].nparams < 0 ? 0 : I->cfuncs[i].nparams, 0);
    }
  }

  // Set globals for error reporting
  g_current_source = source;
  g_current_file = source_path;

  Lexer lex = {.src = source, .pos = 0, .line = 1};
  advance(&lex);

  IrisFunction main_func = {0};
  strcpy(main_func.name, "__main");
  int main_start = -1;

  while (lex.current.type != TOK_EOF) {
    if (lex.current.type == TOK_IMPORT) {
      compile_import(&lex, prog);
    } else if (lex.current.type == TOK_EXPORT) {
      advance(&lex);
      if (lex.current.type == TOK_FN || lex.current.type == TOK_ASYNC) {
        int func_idx = prog->func_count;
        compile_function(&lex, prog);
        strcpy(prog->exports[prog->export_count].name,
               prog->funcs[func_idx].name);
        prog->exports[prog->export_count].type = IMPORT_FN;
        prog->exports[prog->export_count].index = func_idx;
        prog->export_count++;
      } else if (lex.current.type == TOK_STRUCT) {
        int struct_idx = prog->struct_type_count;
        compile_struct_definition(&lex, prog);
        strcpy(prog->exports[prog->export_count].name,
               prog->struct_types[struct_idx].name);
        prog->exports[prog->export_count].type = IMPORT_STRUCT;
        prog->exports[prog->export_count].index = struct_idx;
        prog->export_count++;
      } else if (lex.current.type == TOK_CLASS) {
        int class_idx = prog->class_type_count;
        compile_class_definition(&lex, prog);
        strcpy(prog->exports[prog->export_count].name,
               prog->class_types[class_idx].name);

        prog->exports[prog->export_count].type = IMPORT_CLASS;
        prog->exports[prog->export_count].index = class_idx;
        prog->export_count++;
      } else {
        parser_error(&lex,
                     "expected 'function', 'struct' or 'class' after 'export'");
      }
    } else if (lex.current.type == TOK_FN || lex.current.type == TOK_ASYNC) {
      compile_function(&lex, prog);
    } else if (lex.current.type == TOK_STRUCT) {
      compile_struct_definition(&lex, prog);
    } else if (lex.current.type == TOK_CLASS) {
      compile_class_definition(&lex, prog);
    } else {
      if (main_start == -1) {
        main_start = prog->code_size;
      }
      compile_statement(&lex, prog, &main_func);
    }
  }

  if (main_start != -1) {
    emit(prog, OP_HALT);

    strcpy(prog->funcs[prog->func_count].name, "__main");
    prog->funcs[prog->func_count].addr = main_start;
    prog->funcs[prog->func_count].is_native = 0;
    prog->funcs[prog->func_count].param_count = 0;
    prog->func_count++;
  }

  return prog;
}

// Helper: Copy a function from one program to another, returning the new
// function index
static int copy_function_to_program(IrisProgram *dst, IrisProgram *src,
                                    int src_func_idx) {
  IrisFunction *src_func = &src->funcs[src_func_idx];

  // Find function bytecode boundaries
  int func_start = src_func->addr;
  int func_end = src->code_size;
  for (int j = 0; j < src->func_count; j++) {
    if (src->funcs[j].addr > func_start && src->funcs[j].addr < func_end) {
      func_end = src->funcs[j].addr;
    }
  }

  int func_size = func_end - func_start;
  int code_offset = dst->code_size;

  // Ensure capacity
  while (dst->code_size + func_size >= dst->code_capacity) {
    dst->code_capacity *= 2;
    dst->code = realloc(dst->code, dst->code_capacity);
  }

  // Copy bytecode
  memcpy(dst->code + dst->code_size, src->code + func_start, func_size);
  dst->code_size += func_size;

  // Copy function metadata with new address
  int dst_idx = dst->func_count;
  memcpy(&dst->funcs[dst_idx], src_func, sizeof(IrisFunction));
  dst->funcs[dst_idx].addr = code_offset;
  dst->func_count++;

  return dst_idx;
}

void compile_import(Lexer *lex, IrisProgram *prog) {
  expect(lex, TOK_IMPORT);
  expect(lex, TOK_LBRACE);

  char import_names[16][32];
  int import_count = 0;

  if (lex->current.type != TOK_RBRACE) {
    if (lex->current.type != TOK_IDENT) {
      parser_error(lex, "expected identifier in import");
    }
    strcpy(import_names[import_count++], lex->current.str);
    advance(lex);

    while (lex->current.type == TOK_COMMA) {
      advance(lex);
      if (lex->current.type != TOK_IDENT) {
        parser_error(lex, "expected identifier after ','");
      }
      strcpy(import_names[import_count++], lex->current.str);
      advance(lex);
    }
  }

  expect(lex, TOK_RBRACE);
  expect(lex, TOK_FROM);

  if (lex->current.type != TOK_STRING) {
    parser_error(lex, "expected path string after 'from'");
  }

  char import_path[256];
  strcpy(import_path, lex->current.str);
  advance(lex);

  char resolved_path[256];
  resolve_import_path(prog->source_path, import_path, resolved_path);

  IrisModule *existing = find_module(resolved_path);
  if (existing && existing->is_compiling) {
    parser_error(lex, "circular import detected: %s", resolved_path);
  }

  IrisProgram *imported;
  if (existing) {
    imported = existing->program;
  } else {
    imported = compile_module(resolved_path);
  }

  for (int i = 0; i < import_count; i++) {
    Export *exp = find_export(imported, import_names[i]);
    if (!exp) {
      parser_error(lex, "export '%s' not found in module '%s'", import_names[i],
                   resolved_path);
    }

    if (exp->type == IMPORT_FN) {
      IrisFunction *src_func = &imported->funcs[exp->index];
      int code_offset = prog->code_size;

      int func_start = src_func->addr;
      int func_end = imported->code_size;

      for (int j = 0; j < imported->func_count; j++) {
        if (imported->funcs[j].addr > func_start &&
            imported->funcs[j].addr < func_end) {
          func_end = imported->funcs[j].addr;
        }
      }

      while (prog->code_size + (func_end - func_start) >= prog->code_capacity) {
        prog->code_capacity *= 2;
        prog->code = realloc(prog->code, prog->code_capacity);
      }

      memcpy(prog->code + prog->code_size, imported->code + func_start,
             func_end - func_start);
      prog->code_size += (func_end - func_start);

      for (int j = 0; j < imported->string_count; j++) {
        prog->strings[prog->string_count++] =
            iris_string_copy(imported->strings[j]->data);
      }

      IrisFunction *dst_func = &prog->funcs[prog->func_count];
      memcpy(dst_func, src_func, sizeof(IrisFunction));
      dst_func->addr = code_offset;
      prog->func_count++;

    } else if (exp->type == IMPORT_STRUCT) {
      StructType *src_struct = &imported->struct_types[exp->index];
      StructType *dst_struct = &prog->struct_types[prog->struct_type_count];
      memcpy(dst_struct, src_struct, sizeof(StructType));
      prog->struct_type_count++;
    } else if (exp->type == IMPORT_CLASS) {
      ClassType *src_class = &imported->class_types[exp->index];
      ClassType *dst_class = &prog->class_types[prog->class_type_count];
      memcpy(dst_class, src_class, sizeof(ClassType));

      // Copy string constants from imported module
      for (int j = 0; j < imported->string_count; j++) {
        prog->strings[prog->string_count++] =
            iris_string_copy(imported->strings[j]->data);
      }

      // Copy constructor function if present
      if (src_class->ctor_idx >= 0) {
        dst_class->ctor_idx =
            copy_function_to_program(prog, imported, src_class->ctor_idx);
      }

      // Copy destructor function if present
      if (src_class->dtor_idx >= 0) {
        dst_class->dtor_idx =
            copy_function_to_program(prog, imported, src_class->dtor_idx);
      }

      // Copy all method functions
      for (int m = 0; m < src_class->method_count; m++) {
        int src_method_idx = src_class->methods[m].method_idx;
        dst_class->methods[m].method_idx =
            copy_function_to_program(prog, imported, src_method_idx);
      }

      prog->class_type_count++;
    }
  }
}

IrisProgram *compile_module(const char *path) {
  IrisModule *mod = &g_modules[g_module_count++];
  strcpy(mod->path, path);
  mod->is_compiling = 1;
  mod->program = NULL;

  FILE *f = fopen(path, "r");
  if (!f) {
    fprintf(stderr, "Cannot open module: %s\n", path);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *source = malloc(size + 1);
  fread(source, 1, size, f);
  source[size] = 0;
  fclose(f);

  // Save and restore global source for nested compilation
  const char *saved_source = g_current_source;
  const char *saved_file = g_current_file;

  IrisProgram *prog = compile_with_path(source, path);
  free(source);

  // Restore parent compilation's source context
  g_current_source = saved_source;
  g_current_file = saved_file;

  mod->is_compiling = 0;
  mod->program = prog;
  return prog;
}

IrisProgram *compile(const char *source) {
  return compile_with_path(source, "main.iris");
}

int read_int(unsigned char *code, int *ip) {
  int val = code[*ip] | (code[*ip + 1] << 8) | (code[*ip + 2] << 16) |
            (code[*ip + 3] << 24);
  *ip += 4;
  return val;
}

int64_t read_int64(unsigned char *code, int *ip) {
  int64_t val = 0;
  for (int i = 0; i < 8; i++) {
    val |= ((int64_t)code[*ip + i]) << (i * 8);
  }
  *ip += 8;
  return val;
}

// Find which function contains a given code offset
int find_func_for_offset(IrisProgram *prog, int offset) {
  int best_idx = -1;
  int best_addr = -1;

  for (int i = 0; i < prog->func_count; i++) {
    if (prog->funcs[i].is_native)
      continue;
    if (prog->funcs[i].addr <= offset && prog->funcs[i].addr > best_addr) {
      best_addr = prog->funcs[i].addr;
      best_idx = i;
    }
  }
  return best_idx;
}

// Print stack trace
void print_stack_trace(VM *vm, IrisProgram *prog, const char *error_msg) {
  fflush(stdout); // Ensure all output is printed before error
  fprintf(stderr, "\n\033[1;31mError: %s\033[0m\n", error_msg);
  fprintf(stderr, "\nStack trace (most recent call last):\n");

  char line_buf[256];

  // Print call stack frames in reverse order (oldest first)
  for (int i = 0; i < vm->csp; i++) {
    int func_idx = vm->frames[i].func_idx;
    int call_line = vm->frames[i].call_line;

    const char *func_name =
        (func_idx >= 0) ? prog->funcs[func_idx].name : "<unknown>";
    const char *file_name = prog->source_path;

    // Skip internal function names that start with __
    if (func_name[0] == '_' && func_name[1] == '_') {
      func_name = "<module>";
    }

    fprintf(stderr, "  File \"%s\", line %d, in %s\n", file_name, call_line,
            func_name);

    // Try to show the source line
    const char *src_line =
        get_source_line(prog->source, call_line, line_buf, sizeof(line_buf));
    if (src_line && *src_line) {
      fprintf(stderr, "    %s\n", src_line);
    }
  }

  // Print current location
  int current_line = get_line_for_offset(prog, vm->ip - 1);
  int current_func_idx = find_func_for_offset(prog, vm->ip - 1);
  const char *current_func =
      (current_func_idx >= 0) ? prog->funcs[current_func_idx].name : "<module>";

  if (current_func[0] == '_' && current_func[1] == '_') {
    current_func = "<module>";
  }

  fprintf(stderr, "  File \"%s\", line %d, in %s\n", prog->source_path,
          current_line, current_func);

  const char *src_line =
      get_source_line(prog->source, current_line, line_buf, sizeof(line_buf));
  if (src_line && *src_line) {
    fprintf(stderr, "    %s\n", src_line);
  }

  fprintf(stderr, "\n");
}

// Convert exception value to string for error message
const char *exception_to_string(IrisValue *v, char *buf, int buf_size) {
  if (v->type == VAL_STRING) {
    return v->as.str->data;
  } else if (v->type == VAL_INT) {
    snprintf(buf, buf_size, "%" PRId64, v->as.num);
    return buf;
  } else if (v->type == VAL_BOOL) {
    return v->as.boolean ? "true" : "false";
  } else {
    return "<exception>";
  }
}

// Throw an exception - returns true if handled, false if not
bool throw_exception(VM *vm, IrisProgram *prog __attribute__((unused)),
                     IrisValue exception) {
  // Store the exception
  if (vm->has_exception) {
    iris_value_decref(&vm->exception);
  }
  vm->exception = exception;
  vm->has_exception = true;

  // Look for a handler
  if (vm->handler_count > 0) {
    ExceptionHandler *handler = &vm->handlers[--vm->handler_count];

    // Restore state to when try was entered
    vm->sp = handler->sp;
    vm->bp = handler->bp;

    // Unwind call stack
    while (vm->csp > handler->csp) {
      vm->csp--;
    }

    // Jump to catch block
    vm->ip = handler->catch_addr;
    return true;
  }

  return false;
}

// Runtime error that throws an exception
void runtime_error(VM *vm, IrisProgram *prog, const char *format, ...) {
  char msg[512];
  va_list args;
  va_start(args, format);
  vsnprintf(msg, sizeof(msg), format, args);
  va_end(args);

  IrisString *str = iris_string_copy(msg);
  IrisValue exception = iris_string(str);

  if (!throw_exception(vm, prog, exception)) {
    // No handler - print stack trace and exit
    print_stack_trace(vm, prog, msg);
    exit(1);
  }
}

void execute(VM *vm, IrisProgram *prog) {
  // Only reset IP to __main if we're NOT resuming a task
  // (When resuming a task, iris_task_restore_state already set the IP)
  if (!g_event_loop || !g_event_loop->current_task) {
    int main_idx = find_func(prog, "__main");
    vm->ip = prog->funcs[main_idx].addr;
  } else {
  }

  while (vm->ip < prog->code_size) {
    IrisOpCode op = prog->code[vm->ip++];

    switch (op) {
    case OP_PUSH:
      vm->stack[vm->sp++] = iris_int(read_int64(prog->code, &vm->ip));
      break;
    case OP_PUSH_BOOL: {
      int b = prog->code[vm->ip++];
      vm->stack[vm->sp++] = iris_bool(b != 0);
      break;
    }
    case OP_PUSH_STRING: {
      int str_idx = read_int(prog->code, &vm->ip);
      IrisString *s = prog->strings[str_idx];
      iris_string_incref(s);
      vm->stack[vm->sp++] = iris_string(s);
      break;
    }
    case OP_STRING_CONCAT: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      if (a.type != VAL_STRING || b.type != VAL_STRING) {
        runtime_error_at(prog, vm, "cannot concatenate non-string values");
        exit(1);
      }
      IrisString *result = iris_string_concat(a.as.str, b.as.str);
      iris_value_decref(&a);
      iris_value_decref(&b);
      vm->stack[vm->sp++] = iris_string(result);
      break;
    }
    case OP_TO_STRING: {
      IrisValue v = vm->stack[--vm->sp];
      IrisString *result = NULL;

      if (v.type == VAL_STRING) {
        iris_string_incref(v.as.str);
        vm->stack[vm->sp++] = iris_string(v.as.str);
        iris_value_decref(&v);
        break;
      } else if (v.type == VAL_INT) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%" PRId64, v.as.num);
        result = iris_string_copy(buf);
      } else if (v.type == VAL_BOOL) {
        result = iris_string_copy(v.as.boolean ? "true" : "false");
      } else if (v.type == VAL_STRUCT) {
        result = iris_string_copy("<struct>");
      } else if (v.type == VAL_CLASS) {
        result = iris_string_copy("<object>");
      } else if (v.type == VAL_LIST) {
        result = iris_value_to_string(v);
      } else if (v.type == VAL_FUTURE) {
        result = iris_value_to_string(v);
      } else if (v.type == VAL_FUNC) {
        result = iris_value_to_string(v);
      } else {
        result = iris_string_copy("<unknown>");
      }

      iris_value_decref(&v);
      vm->stack[vm->sp++] = iris_string(result);
      break;
    }
    case OP_LOAD_LOCAL: {
      int idx = read_int(prog->code, &vm->ip);
      IrisValue value = vm->locals[vm->bp + idx];
      iris_value_incref(&value);
      vm->stack[vm->sp++] = value;
      break;
    }
    case OP_STORE_LOCAL: {
      int idx = read_int(prog->code, &vm->ip);
      IrisValue old_val = vm->locals[vm->bp + idx];
      iris_value_decref(&old_val);
      vm->locals[vm->bp + idx] = vm->stack[--vm->sp];
      break;
    }
    case OP_ADD: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      if (a.type == VAL_STRING && b.type == VAL_STRING) {
        IrisString *result = iris_string_concat(a.as.str, b.as.str);
        iris_value_decref(&a);
        iris_value_decref(&b);
        vm->stack[vm->sp++] = iris_string(result);
      } else if (a.type == VAL_INT && b.type == VAL_INT) {
        vm->stack[vm->sp++] = iris_int(a.as.num + b.as.num);
      } else {
        runtime_error_at(prog, vm, "type error: cannot add incompatible types");
        exit(1);
      }
      break;
    }
    case OP_SUB: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num - b.as.num);
      break;
    }
    case OP_MUL: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num * b.as.num);
      break;
    }
    case OP_DIV: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      if (b.as.num == 0) {
        runtime_error(vm, prog, "Division by zero");
        if (vm->has_exception) {
          vm->stack[vm->sp++] = vm->exception;
          vm->has_exception = false;
          continue;
        }
        return;
      }
      vm->stack[vm->sp++] = iris_int(a.as.num / b.as.num);
      break;
    }
    case OP_EQ: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      bool result = false;
      if (a.type == b.type) {
        if (a.type == VAL_INT) {
          result = (a.as.num == b.as.num);
        } else if (a.type == VAL_STRING) {
          result = (strcmp(a.as.str->data, b.as.str->data) == 0);
        } else if (a.type == VAL_BOOL) {
          result = (a.as.boolean == b.as.boolean);
        }
      }
      iris_value_decref(&a);
      iris_value_decref(&b);
      vm->stack[vm->sp++] = iris_bool(result);
      break;
    }
    case OP_GT_EQ: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_bool(a.as.num >= b.as.num);
      break;
    }
    case OP_LT_EQ: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_bool(a.as.num <= b.as.num);
      break;
    }
    case OP_NOT_EQ: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      bool result = true;
      if (a.type == b.type) {
        if (a.type == VAL_INT) {
          result = (a.as.num != b.as.num);
        } else if (a.type == VAL_STRING) {
          result = (strcmp(a.as.str->data, b.as.str->data) != 0);
        } else if (a.type == VAL_BOOL) {
          result = (a.as.boolean != b.as.boolean);
        }
      }
      iris_value_decref(&a);
      iris_value_decref(&b);
      vm->stack[vm->sp++] = iris_bool(result);
      break;
    }
    case OP_LT: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_bool(a.as.num < b.as.num);
      break;
    }
    case OP_GT: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_bool(a.as.num > b.as.num);
      break;
    }
    case OP_JMP: {
      int addr = read_int(prog->code, &vm->ip);
      vm->ip = addr;
      break;
    }
    case OP_JMP_IF_FALSE: {
      int addr = read_int(prog->code, &vm->ip);
      IrisValue cond = vm->stack[--vm->sp];
      bool is_false = false;
      if (cond.type == VAL_INT) {
        is_false = (cond.as.num == 0);
      } else if (cond.type == VAL_BOOL) {
        is_false = !cond.as.boolean;
      }
      iris_value_decref(&cond);
      if (is_false)
        vm->ip = addr;
      break;
    }
    case OP_CALL: {
      int func_idx = read_int(prog->code, &vm->ip);
      int caller_frame_size = read_int(prog->code, &vm->ip);
      IrisFunction *f = &prog->funcs[func_idx];

      if (f->is_native) {
        g_current_native_func_name = f->name;
        f->native_fn(vm, f->param_count);
        g_current_native_func_name = NULL;
      } else {
        // Store enhanced frame info for stack traces
        int call_site_line =
            get_line_for_offset(prog, vm->ip - 9); // Approx call site
        int caller_func = find_func_for_offset(prog, vm->ip - 9);

        vm->call_stack[vm->csp] = vm->ip;
        vm->bp_stack[vm->csp] = vm->bp;
        vm->sp_stack[vm->csp] = vm->sp - f->param_count;
        vm->frames[vm->csp].func_idx = caller_func;
        vm->frames[vm->csp].call_line = call_site_line;
        vm->frames[vm->csp].return_addr = vm->ip;
        vm->csp++;

        int new_bp = vm->bp + caller_frame_size;

        for (int i = 0; i < f->param_count; i++) {
          iris_value_decref(&vm->locals[new_bp + i]);
          vm->locals[new_bp + i] = vm->stack[vm->sp - f->param_count + i];
        }

        vm->bp = new_bp;
        vm->ip = f->addr;
      }
      break;
    }
    case OP_RETURN: {
      IrisValue retval = vm->stack[vm->sp - 1];
      vm->sp--;

      if (vm->csp == 0)
        return;

      vm->csp--;
      vm->ip = vm->call_stack[vm->csp];
      vm->bp = vm->bp_stack[vm->csp];
      vm->sp = vm->sp_stack[vm->csp];

      vm->stack[vm->sp++] = retval;
      break;
    }
    case OP_POP: {
      IrisValue v = vm->stack[--vm->sp];
      iris_value_decref(&v);
      break;
    }
    case OP_NEW_STRUCT: {
      int type_id = read_int(prog->code, &vm->ip);
      int field_count = read_int(prog->code, &vm->ip);

      IrisStruct *s = iris_struct_new(type_id, field_count);

      for (int i = 0; i < field_count; i++) {
        int field_idx = read_int(prog->code, &vm->ip);
        IrisValue val = vm->stack[--vm->sp];
        s->fields[field_idx] = val;
      }

      vm->stack[vm->sp++] = iris_struct(s);
      break;
    }
    case OP_GET_FIELD: {
      int str_idx = read_int(prog->code, &vm->ip);
      IrisValue obj_val = vm->stack[--vm->sp];
      const char *field_name = prog->strings[str_idx]->data;
      int field_idx = -1;
      IrisValue *fields = NULL;

      if (obj_val.type == VAL_STRUCT) {
        IrisStruct *s = obj_val.as.instance;
        StructType *st = &prog->struct_types[s->type_id];
        fields = s->fields;

        for (int i = 0; i < st->field_count; i++) {
          if (strcmp(st->fields[i], field_name) == 0) {
            field_idx = i;
            break;
          }
        }
      } else if (obj_val.type == VAL_CLASS) {
        IrisClass *c = obj_val.as.object;
        ClassType *ct = &prog->class_types[c->class_id];
        fields = c->fields;

        for (int i = 0; i < ct->field_count; i++) {
          if (strcmp(ct->fields[i], field_name) == 0) {
            field_idx = i;
            break;
          }
        }
      } else {
        runtime_error_at(prog, vm,
                         "cannot access field of non-struct/class value");
        exit(1);
      }

      if (field_idx < 0) {
        runtime_error_at(prog, vm, "unknown field '%s'", field_name);
        exit(1);
      }

      IrisValue field = fields[field_idx];
      iris_value_incref(&field);
      iris_value_decref(&obj_val);
      vm->stack[vm->sp++] = field;
      break;
    }
    case OP_SET_FIELD: {
      int str_idx = read_int(prog->code, &vm->ip);
      IrisValue new_val = vm->stack[--vm->sp];
      IrisValue obj_val = vm->stack[--vm->sp];
      const char *field_name = prog->strings[str_idx]->data;
      int field_idx = -1;
      IrisValue *fields = NULL;

      if (obj_val.type == VAL_STRUCT) {
        IrisStruct *s = obj_val.as.instance;
        StructType *st = &prog->struct_types[s->type_id];
        fields = s->fields;

        for (int i = 0; i < st->field_count; i++) {
          if (strcmp(st->fields[i], field_name) == 0) {
            field_idx = i;
            break;
          }
        }
      } else if (obj_val.type == VAL_CLASS) {
        IrisClass *c = obj_val.as.object;
        ClassType *ct = &prog->class_types[c->class_id];
        fields = c->fields;

        for (int i = 0; i < ct->field_count; i++) {
          if (strcmp(ct->fields[i], field_name) == 0) {
            field_idx = i;
            break;
          }
        }
      } else {
        runtime_error_at(prog, vm,
                         "cannot set field of non-struct/class value");
        exit(1);
      }

      if (field_idx < 0) {
        runtime_error_at(prog, vm, "unknown field '%s'", field_name);
        exit(1);
      }

      iris_value_decref(&fields[field_idx]);
      fields[field_idx] = new_val;
      iris_value_decref(&obj_val);
      break;
    }
    case OP_NEW_CLASS: {
      int class_idx = read_int(prog->code, &vm->ip);
      int field_count = read_int(prog->code, &vm->ip);
      int ctor_idx = read_int(prog->code, &vm->ip);
      int argc = read_int(prog->code, &vm->ip);
      int caller_frame_size = read_int(prog->code, &vm->ip);

      // Create the class instance
      IrisClass *obj = iris_class_new(class_idx, field_count);

      if (ctor_idx >= 0) {
        // Call constructor: stack has argc arguments
        // We need to pass 'self' as first argument
        // Constructor returns 'self', so the return value is the object
        IrisFunction *ctor = &prog->funcs[ctor_idx];

        if (argc + 1 != ctor->param_count) {
          runtime_error_at(prog, vm,
                           "constructor expects %d argument(s), got %d",
                           ctor->param_count - 1, argc);
          exit(1);
        }

        // Save call state
        vm->call_stack[vm->csp] = vm->ip;
        vm->bp_stack[vm->csp] = vm->bp;
        vm->sp_stack[vm->csp] = vm->sp - argc; // before the constructor args
        vm->csp++;

        int new_bp = vm->bp + caller_frame_size;

        // First param is self (the new object)
        iris_value_decref(&vm->locals[new_bp]);
        vm->locals[new_bp] = iris_class(obj);
        // obj is now owned by locals, don't need extra incref

        // Copy other arguments from stack to locals
        for (int i = 0; i < argc; i++) {
          iris_value_decref(&vm->locals[new_bp + 1 + i]);
          vm->locals[new_bp + 1 + i] = vm->stack[vm->sp - argc + i];
        }
        vm->sp -= argc; // Remove args from stack

        vm->bp = new_bp;
        vm->ip = ctor->addr;
        // Constructor will return self, which becomes the result on stack
      } else {
        // No constructor, just push the object
        vm->stack[vm->sp++] = iris_class(obj);
      }
      break;
    }
    case OP_CALL_METHOD: {
      int str_idx = read_int(prog->code, &vm->ip);
      int argc = read_int(prog->code, &vm->ip);
      int caller_frame_size = read_int(prog->code, &vm->ip);
      (void)caller_frame_size; // May be unused for built-in methods

      const char *method_name = prog->strings[str_idx]->data;

      // Stack has: ... obj arg1 arg2 ... argN
      // We need to get the object to find its class

      // The object is at stack[sp - argc - 1]
      IrisValue obj_val = vm->stack[vm->sp - argc - 1];

      // Handle list methods
      if (obj_val.type == VAL_LIST) {
        IrisList *list = obj_val.as.list;

        if (strcmp(method_name, "push") == 0) {
          if (argc != 1) {
            runtime_error_at(prog, vm, "list.push() expects 1 argument, got %d",
                             argc);
            exit(1);
          }
          IrisValue val = vm->stack[vm->sp - 1];
          iris_list_push(list, val);
          // Pop arg and list, push list back (for chaining)
          vm->sp -= 2;
          iris_value_incref(&obj_val);
          vm->stack[vm->sp++] = obj_val;
        } else if (strcmp(method_name, "pop") == 0) {
          if (argc != 0) {
            runtime_error_at(prog, vm, "list.pop() expects 0 arguments, got %d",
                             argc);
            exit(1);
          }
          if (list->length == 0) {
            runtime_error_at(prog, vm, "cannot pop from empty list");
            exit(1);
          }
          IrisValue val = iris_list_pop(list);
          // Pop list, push result
          vm->sp -= 1;
          vm->stack[vm->sp++] = val;
        } else if (strcmp(method_name, "peek") == 0) {
          if (argc != 0) {
            runtime_error_at(prog, vm,
                             "list.peek() expects 0 arguments, got %d", argc);
            exit(1);
          }
          if (list->length == 0) {
            runtime_error_at(prog, vm, "cannot peek empty list");
            exit(1);
          }
          IrisValue val = list->items[list->length - 1];
          iris_value_incref(&val);
          // Pop list, push result
          vm->sp -= 1;
          vm->stack[vm->sp++] = val;
        } else if (strcmp(method_name, "len") == 0) {
          if (argc != 0) {
            runtime_error_at(prog, vm, "list.len() expects 0 arguments, got %d",
                             argc);
            exit(1);
          }
          int len = list->length;
          // Pop list, push result
          vm->sp -= 1;
          vm->stack[vm->sp++] = iris_int(len);
        } else if (strcmp(method_name, "clear") == 0) {
          if (argc != 0) {
            runtime_error_at(prog, vm,
                             "list.clear() expects 0 arguments, got %d", argc);
            exit(1);
          }
          // Decref all items
          for (int i = 0; i < list->length; i++) {
            iris_value_decref(&list->items[i]);
          }
          list->length = 0;
          // Pop list, push list back (for chaining)
          vm->sp -= 1;
          iris_value_incref(&obj_val);
          vm->stack[vm->sp++] = obj_val;
        } else if (strcmp(method_name, "shift") == 0) {
          if (argc != 0) {
            runtime_error_at(prog, vm,
                             "list.shift() expects 0 arguments, got %d", argc);
            exit(1);
          }
          if (list->length == 0) {
            runtime_error_at(prog, vm, "cannot shift from empty list");
            exit(1);
          }
          // Get first element
          IrisValue val = list->items[0];
          // Shift all elements left
          for (int i = 0; i < list->length - 1; i++) {
            list->items[i] = list->items[i + 1];
          }
          list->length--;
          // Pop list, push result
          vm->sp -= 1;
          vm->stack[vm->sp++] = val;
        } else if (strcmp(method_name, "unshift") == 0) {
          if (argc != 1) {
            runtime_error_at(prog, vm,
                             "list.unshift() expects 1 argument, got %d", argc);
            exit(1);
          }
          IrisValue val = vm->stack[vm->sp - 1];
          // Ensure capacity
          if (list->length >= list->capacity) {
            list->capacity = list->capacity == 0 ? 8 : list->capacity * 2;
            list->items =
                realloc(list->items, list->capacity * sizeof(IrisValue));
          }
          // Shift all elements right
          for (int i = list->length; i > 0; i--) {
            list->items[i] = list->items[i - 1];
          }
          iris_value_incref(&val);
          list->items[0] = val;
          list->length++;
          // Pop arg and list, push list back (for chaining)
          vm->sp -= 2;
          iris_value_incref(&obj_val);
          vm->stack[vm->sp++] = obj_val;
        } else {
          runtime_error_at(prog, vm, "unknown method '%s' on list",
                           method_name);
          exit(1);
        }
        iris_value_decref(&obj_val);
        break;
      }

      // Handle Future methods (.then, .catch, .finally)
      if (obj_val.type == VAL_FUTURE) {
        IrisFuture *future = obj_val.as.future;

        if (strcmp(method_name, "then") == 0) {
          if (argc != 1) {
            runtime_error_at(prog, vm,
                             "future.then() expects 1 argument, got %d", argc);
            exit(1);
          }
          IrisValue callback = vm->stack[vm->sp - 1];
          if (callback.type != VAL_FUNC) {
            runtime_error_at(prog, vm, ".then() callback must be a function");
            exit(1);
          }

          // Create chained future that stores the source and callback
          IrisFuture *chained = iris_future_new();
          chained->chained = future;
          iris_future_incref(future);
          chained->result = callback; // Store callback in result temporarily
          iris_value_incref(&callback);
          chained->combinator_type = 3; // 3 = then chain

          vm->sp -= 2; // Pop callback and future
          vm->stack[vm->sp++] = iris_future(chained);
          iris_value_decref(&obj_val);
          break;

        } else if (strcmp(method_name, "catch") == 0) {
          if (argc != 1) {
            runtime_error_at(prog, vm,
                             "future.catch() expects 1 argument, got %d", argc);
            exit(1);
          }
          IrisValue callback = vm->stack[vm->sp - 1];
          if (callback.type != VAL_FUNC) {
            runtime_error_at(prog, vm, ".catch() callback must be a function");
            exit(1);
          }

          IrisFuture *chained = iris_future_new();
          chained->chained = future;
          iris_future_incref(future);
          chained->result = callback;
          iris_value_incref(&callback);
          chained->combinator_type = 4; // 4 = catch chain

          vm->sp -= 2;
          vm->stack[vm->sp++] = iris_future(chained);
          iris_value_decref(&obj_val);
          break;

        } else if (strcmp(method_name, "finally") == 0) {
          if (argc != 1) {
            runtime_error_at(
                prog, vm, "future.finally() expects 1 argument, got %d", argc);
            exit(1);
          }
          IrisValue callback = vm->stack[vm->sp - 1];
          if (callback.type != VAL_FUNC) {
            runtime_error_at(prog, vm,
                             ".finally() callback must be a function");
            exit(1);
          }

          IrisFuture *chained = iris_future_new();
          chained->chained = future;
          iris_future_incref(future);
          chained->result = callback;
          iris_value_incref(&callback);
          chained->combinator_type = 5; // 5 = finally chain

          vm->sp -= 2;
          vm->stack[vm->sp++] = iris_future(chained);
          iris_value_decref(&obj_val);
          break;

        } else {
          runtime_error_at(prog, vm, "unknown method '%s' on Future",
                           method_name);
          exit(1);
        }
      }

      if (obj_val.type != VAL_CLASS) {
        runtime_error_at(prog, vm,
                         "cannot call method on non-class value (type=%d)",
                         obj_val.type);
        exit(1);
      }

      IrisClass *obj = obj_val.as.object;
      ClassType *ct = &prog->class_types[obj->class_id];

      // Find the method
      int method_idx = find_class_method(ct, method_name);
      if (method_idx < 0) {
        runtime_error_at(prog, vm, "unknown method '%s' on class '%s'",
                         method_name, ct->name);
        exit(1);
      }

      IrisFunction *method = &prog->funcs[method_idx];

      // Check argument count (method has 'self' as first param)
      if (argc + 1 != method->param_count) {
        runtime_error_at(prog, vm, "method '%s' expects %d argument(s), got %d",
                         method_name, method->param_count - 1, argc);
        exit(1);
      }

      // Set up the call
      vm->call_stack[vm->csp] = vm->ip;
      vm->bp_stack[vm->csp] = vm->bp;
      vm->sp_stack[vm->csp] = vm->sp - argc - 1; // before obj and args
      vm->csp++;

      int new_bp = vm->bp + caller_frame_size;

      // First param is self (the object)
      iris_value_decref(&vm->locals[new_bp]);
      vm->locals[new_bp] = vm->stack[vm->sp - argc - 1];
      iris_value_incref(&vm->locals[new_bp]);

      // Copy other arguments
      for (int i = 0; i < argc; i++) {
        iris_value_decref(&vm->locals[new_bp + 1 + i]);
        vm->locals[new_bp + 1 + i] = vm->stack[vm->sp - argc + i];
      }

      // Pop args and object from stack (they're now in locals)
      vm->sp -= (argc + 1);

      vm->bp = new_bp;
      vm->ip = method->addr;
      break;
    }
    case OP_TRY_BEGIN: {
      int catch_addr = read_int(prog->code, &vm->ip);

      // Push exception handler
      if (vm->handler_count >= 32) {
        runtime_error(vm, prog, "Too many nested try blocks");
        if (vm->has_exception)
          continue; // Handler found, continue
        return;
      }

      ExceptionHandler *handler = &vm->handlers[vm->handler_count++];
      handler->catch_addr = catch_addr;
      handler->sp = vm->sp;
      handler->bp = vm->bp;
      handler->csp = vm->csp;
      break;
    }
    case OP_TRY_END: {
      // Pop exception handler (try block completed successfully)
      if (vm->handler_count > 0) {
        vm->handler_count--;
      }
      break;
    }
    case OP_THROW: {
      // Get exception value from stack
      IrisValue exc = vm->stack[--vm->sp];

      if (!throw_exception(vm, prog, exc)) {
        // No handler found
        if (g_event_loop && g_event_loop->current_task) {
          // In async context - reject the task's future
          IrisTask *task = g_event_loop->current_task;
          iris_future_reject(task->future, exc);
          task->state = TASK_FAILED;
          iris_value_decref(&exc);
          return; // Exit this task's execution
        }
        // Not in async context - print error and exit
        char buf[64];
        const char *msg = exception_to_string(&exc, buf, sizeof(buf));
        print_stack_trace(vm, prog, msg);
        iris_value_decref(&exc);
        exit(1);
      }
      // Handler found - push exception value for catch block
      vm->stack[vm->sp++] = vm->exception;
      vm->has_exception = false;
      break;
    }
    // Bitwise and modulo operators
    case OP_MOD: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      if (b.as.num == 0) {
        runtime_error(vm, prog, "Modulo by zero");
        if (vm->has_exception) {
          vm->stack[vm->sp++] = vm->exception;
          vm->has_exception = false;
          continue;
        }
        return;
      }
      vm->stack[vm->sp++] = iris_int(a.as.num % b.as.num);
      break;
    }
    case OP_BITAND: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num & b.as.num);
      break;
    }
    case OP_BITOR: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num | b.as.num);
      break;
    }
    case OP_BITXOR: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num ^ b.as.num);
      break;
    }
    case OP_BITNOT: {
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(~a.as.num);
      break;
    }
    case OP_SHL: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num << b.as.num);
      break;
    }
    case OP_SHR: {
      IrisValue b = vm->stack[--vm->sp];
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(a.as.num >> b.as.num);
      break;
    }
    case OP_NEGATE: {
      IrisValue a = vm->stack[--vm->sp];
      vm->stack[vm->sp++] = iris_int(-a.as.num);
      break;
    }
    case OP_DUP: {
      IrisValue v = vm->stack[vm->sp - 1];
      iris_value_incref(&v);
      vm->stack[vm->sp++] = v;
      break;
    }
    case OP_DUP2: {
      // Duplicate top two stack values (for compound index assignment)
      IrisValue v1 = vm->stack[vm->sp - 2]; // list
      IrisValue v2 = vm->stack[vm->sp - 1]; // index
      iris_value_incref(&v1);
      iris_value_incref(&v2);
      vm->stack[vm->sp++] = v1;
      vm->stack[vm->sp++] = v2;
      break;
    }
    // Array/list operations
    case OP_NEW_LIST: {
      int count = read_int(prog->code, &vm->ip);
      IrisList *list = iris_list_new(count > 0 ? count : 8);
      // Pop elements in reverse order and add to list
      for (int i = count - 1; i >= 0; i--) {
        IrisValue v = vm->stack[--vm->sp];
        list->items[i] = v;
      }
      list->length = count;
      vm->stack[vm->sp++] = iris_list(list);
      break;
    }
    case OP_LIST_GET: {
      IrisValue idx_val = vm->stack[--vm->sp];
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot index non-list value");
        exit(1);
      }
      int idx = (int)idx_val.as.num;
      IrisList *list = list_val.as.list;
      if (idx < 0 || idx >= list->length) {
        runtime_error_at(prog, vm, "list index out of bounds: %d (length %d)",
                         idx, list->length);
        exit(1);
      }
      IrisValue item = list->items[idx];
      iris_value_incref(&item);
      iris_value_decref(&list_val);
      vm->stack[vm->sp++] = item;
      break;
    }
    case OP_LIST_SET: {
      IrisValue new_val = vm->stack[--vm->sp];
      IrisValue idx_val = vm->stack[--vm->sp];
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot index non-list value");
        exit(1);
      }
      int idx = (int)idx_val.as.num;
      IrisList *list = list_val.as.list;
      if (idx < 0 || idx >= list->length) {
        runtime_error_at(prog, vm, "list index out of bounds: %d (length %d)",
                         idx, list->length);
        exit(1);
      }
      iris_value_decref(&list->items[idx]);
      list->items[idx] = new_val;
      iris_value_decref(&list_val);
      break;
    }
    case OP_LIST_PUSH: {
      IrisValue val = vm->stack[--vm->sp];
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot push to non-list value");
        exit(1);
      }
      iris_list_push(list_val.as.list, val);
      iris_value_decref(&list_val);
      break;
    }
    case OP_LIST_POP: {
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot pop from non-list value");
        exit(1);
      }
      IrisValue val = iris_list_pop(list_val.as.list);
      iris_value_decref(&list_val);
      vm->stack[vm->sp++] = val;
      break;
    }
    case OP_LIST_LEN: {
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot get length of non-list value");
        exit(1);
      }
      int len = list_val.as.list->length;
      iris_value_decref(&list_val);
      vm->stack[vm->sp++] = iris_int(len);
      break;
    }
    case OP_LIST_PEEK: {
      IrisValue list_val = vm->stack[--vm->sp];
      if (list_val.type != VAL_LIST) {
        runtime_error_at(prog, vm, "cannot peek non-list value");
        exit(1);
      }
      IrisList *list = list_val.as.list;
      if (list->length == 0) {
        runtime_error_at(prog, vm, "cannot peek empty list");
        exit(1);
      }
      IrisValue val = list->items[list->length - 1];
      iris_value_incref(&val);
      iris_value_decref(&list_val);
      vm->stack[vm->sp++] = val;
      break;
    }

      // ========================================================================
      // Async/Await Opcodes
      // ========================================================================

    case OP_AWAIT: {
      IrisValue fval = vm->stack[--vm->sp];

      // If not a future, treat as immediate value (pass through)
      if (fval.type != VAL_FUTURE) {
        vm->stack[vm->sp++] = fval;
        break;
      }

      IrisFuture *f = fval.as.future;

      if (f->state == FUTURE_RESOLVED) {
        // Already resolved - push result
        iris_value_incref(&f->result);
        vm->stack[vm->sp++] = f->result;
        iris_future_decref(f);
      } else if (f->state == FUTURE_REJECTED) {
        // Rejected - throw exception
        IrisValue exc = f->result;
        iris_value_incref(&exc);
        iris_future_decref(f);

        if (!throw_exception(vm, prog, exc)) {
          // No handler found
          if (g_event_loop && g_event_loop->current_task) {
            // In async context - reject the task's future
            IrisTask *task = g_event_loop->current_task;
            iris_future_reject(task->future, exc);
            task->state = TASK_FAILED;
            iris_value_decref(&exc);
            return; // Exit this task's execution
          }
          // Not in async context - fatal error
          char buf[64];
          const char *msg = exception_to_string(&exc, buf, sizeof(buf));
          print_stack_trace(vm, prog, msg);
          iris_value_decref(&exc);
          exit(1);
        }
        // Handler found - push exception value for catch block
        vm->stack[vm->sp++] = vm->exception;
        vm->has_exception = false;
        break;
      } else {
        // Pending - check if it's a combinator that might be ready
        if (f->combinator_type == 1 && f->combinator != NULL) {
          // all() combinator - check if ready (combinator is IrisList*)
          IrisList *arr = (IrisList *)f->combinator;
          int all_resolved = 1;
          int any_rejected = 0;
          int rejected_idx = -1;

          for (int i = 0; i < arr->length; i++) {
            IrisValue item = arr->items[i];
            if (item.type == VAL_FUTURE) {
              IrisFuture *src = item.as.future;
              if (src->state == FUTURE_REJECTED) {
                any_rejected = 1;
                rejected_idx = i;
                break;
              } else if (src->state != FUTURE_RESOLVED) {
                all_resolved = 0;
              }
            }
          }

          if (any_rejected) {
            IrisFuture *rej_f = arr->items[rejected_idx].as.future;
            iris_value_incref(&rej_f->result);
            iris_future_reject(f, rej_f->result);
            // Now resolved - handle as rejected
            IrisValue exc = f->result;
            iris_value_incref(&exc);
            iris_future_decref(f);

            if (!throw_exception(vm, prog, exc)) {
              if (g_event_loop && g_event_loop->current_task) {
                IrisTask *task = g_event_loop->current_task;
                iris_future_reject(task->future, exc);
                task->state = TASK_FAILED;
                iris_value_decref(&exc);
                return;
              }
              char buf[64];
              const char *msg = exception_to_string(&exc, buf, sizeof(buf));
              print_stack_trace(vm, prog, msg);
              iris_value_decref(&exc);
              exit(1);
            }
            vm->stack[vm->sp++] = vm->exception;
            vm->has_exception = false;
            break;
          }

          if (all_resolved) {
            // Build result array
            IrisList *result_list = iris_list_new(arr->length);
            for (int i = 0; i < arr->length; i++) {
              IrisValue item = arr->items[i];
              if (item.type == VAL_FUTURE) {
                iris_value_incref(&item.as.future->result);
                iris_list_push(result_list, item.as.future->result);
              } else {
                iris_value_incref(&item);
                iris_list_push(result_list, item);
              }
            }
            IrisValue result_val;
            result_val.type = VAL_LIST;
            result_val.as.list = result_list;
            iris_future_resolve(f, result_val);
            // Now resolved - push result
            iris_value_incref(&f->result);
            vm->stack[vm->sp++] = f->result;
            iris_future_decref(f);
            break;
          }
        } else if (f->combinator_type == 2 && f->combinator != NULL) {
          // race() combinator - check if any settled (combinator is IrisList*)
          IrisList *arr = (IrisList *)f->combinator;
          for (int i = 0; i < arr->length; i++) {
            IrisValue item = arr->items[i];
            if (item.type == VAL_FUTURE) {
              IrisFuture *src = item.as.future;
              if (src->state == FUTURE_RESOLVED) {
                iris_value_incref(&src->result);
                iris_future_resolve(f, src->result);
                iris_value_incref(&f->result);
                vm->stack[vm->sp++] = f->result;
                iris_future_decref(f);
                goto await_done;
              } else if (src->state == FUTURE_REJECTED) {
                iris_value_incref(&src->result);
                iris_future_reject(f, src->result);
                IrisValue exc = f->result;
                iris_value_incref(&exc);
                iris_future_decref(f);

                if (!throw_exception(vm, prog, exc)) {
                  if (g_event_loop && g_event_loop->current_task) {
                    IrisTask *task = g_event_loop->current_task;
                    iris_future_reject(task->future, exc);
                    task->state = TASK_FAILED;
                    iris_value_decref(&exc);
                    return;
                  }
                  char buf[64];
                  const char *msg = exception_to_string(&exc, buf, sizeof(buf));
                  print_stack_trace(vm, prog, msg);
                  iris_value_decref(&exc);
                  exit(1);
                }
                vm->stack[vm->sp++] = vm->exception;
                vm->has_exception = false;
                goto await_done;
              }
            } else {
              // Non-future wins immediately
              iris_value_incref(&item);
              iris_future_resolve(f, item);
              iris_value_incref(&f->result);
              vm->stack[vm->sp++] = f->result;
              iris_future_decref(f);
              goto await_done;
            }
          }
        } else if (f->combinator_type == 3 && f->chained != NULL) {
          // .then() chain - callback is stored in f->result as VAL_FUNC
          IrisFuture *source = f->chained;
          if (source->state == FUTURE_RESOLVED) {
            // Source resolved - call callback with result
            if (f->result.type != VAL_FUNC) {
              runtime_error_at(prog, vm, ".then() callback is not a function");
              exit(1);
            }
            int callback_idx = f->result.as.func_idx;
            IrisFunction *callback = &prog->funcs[callback_idx];

            // Find current function to get proper frame size
            int current_func_idx = find_func_for_offset(prog, vm->ip - 1);
            int caller_frame_size = 0;
            if (current_func_idx >= 0) {
              IrisFunction *current_func = &prog->funcs[current_func_idx];
              caller_frame_size =
                  current_func->param_count + current_func->local_count;
            }

            // Set up call to callback
            // The callback's return value will be awaited result
            vm->call_stack[vm->csp] = vm->ip;
            vm->bp_stack[vm->csp] = vm->bp;
            vm->sp_stack[vm->csp] = vm->sp;
            vm->csp++;

            // Push argument (source's result)
            if (callback->param_count >= 1) {
              iris_value_incref(&source->result);
              vm->stack[vm->sp++] = source->result;
            }

            // Set up locals for callback
            int new_bp = vm->bp + caller_frame_size;
            if (callback->param_count >= 1) {
              iris_value_decref(&vm->locals[new_bp]);
              vm->locals[new_bp] = vm->stack[vm->sp - 1];
              iris_value_incref(&vm->locals[new_bp]);
              vm->sp--;
            }

            vm->bp = new_bp;
            vm->ip = callback->addr;
            iris_future_decref(f);
            break; // Execute callback, its return value will be the result
          } else if (source->state == FUTURE_REJECTED) {
            // Source rejected - propagate without calling callback
            iris_value_incref(&source->result);
            vm->stack[vm->sp++] = source->result;
            iris_future_decref(f);

            // Throw as exception
            IrisValue exc = vm->stack[--vm->sp];
            if (!throw_exception(vm, prog, exc)) {
              if (g_event_loop && g_event_loop->current_task) {
                IrisTask *task = g_event_loop->current_task;
                iris_future_reject(task->future, exc);
                task->state = TASK_FAILED;
                iris_value_decref(&exc);
                return;
              }
              char buf[64];
              const char *msg = exception_to_string(&exc, buf, sizeof(buf));
              print_stack_trace(vm, prog, msg);
              iris_value_decref(&exc);
              exit(1);
            }
            vm->stack[vm->sp++] = vm->exception;
            vm->has_exception = false;
            break;
          }
          // Source pending - fall through to suspension logic
        } else if (f->combinator_type == 4 && f->chained != NULL) {
          // .catch() chain
          IrisFuture *source = f->chained;
          if (source->state == FUTURE_RESOLVED) {
            // Source resolved - propagate without calling callback
            iris_value_incref(&source->result);
            vm->stack[vm->sp++] = source->result;
            iris_future_decref(f);
            break;
          } else if (source->state == FUTURE_REJECTED) {
            // Source rejected - call callback with error
            if (f->result.type != VAL_FUNC) {
              runtime_error_at(prog, vm, ".catch() callback is not a function");
              exit(1);
            }
            int callback_idx = f->result.as.func_idx;
            IrisFunction *callback = &prog->funcs[callback_idx];

            vm->call_stack[vm->csp] = vm->ip;
            vm->bp_stack[vm->csp] = vm->bp;
            vm->sp_stack[vm->csp] = vm->sp;
            vm->csp++;

            if (callback->param_count >= 1) {
              iris_value_incref(&source->result);
              vm->stack[vm->sp++] = source->result;
            }

            int new_bp = vm->bp + 16;
            if (callback->param_count >= 1) {
              iris_value_decref(&vm->locals[new_bp]);
              vm->locals[new_bp] = vm->stack[vm->sp - 1];
              iris_value_incref(&vm->locals[new_bp]);
              vm->sp--;
            }

            vm->bp = new_bp;
            vm->ip = callback->addr;
            iris_future_decref(f);
            break;
          }
          // Source pending - fall through
        } else if (f->combinator_type == 5 && f->chained != NULL) {
          // .finally() chain
          IrisFuture *source = f->chained;
          if (source->state != FUTURE_PENDING) {
            // Source settled - call callback (no args), then propagate original
            if (f->result.type != VAL_FUNC) {
              runtime_error_at(prog, vm,
                               ".finally() callback is not a function");
              exit(1);
            }
            int callback_idx = f->result.as.func_idx;
            IrisFunction *callback = &prog->funcs[callback_idx];

            // For finally, we need to remember to propagate the original result
            // after the callback completes. Store source state for later.
            // For simplicity, just call the callback and then propagate.
            vm->call_stack[vm->csp] = vm->ip;
            vm->bp_stack[vm->csp] = vm->bp;
            vm->sp_stack[vm->csp] = vm->sp;
            vm->csp++;

            int new_bp = vm->bp + 16;
            vm->bp = new_bp;
            vm->ip = callback->addr;
            // Note: finally callback's return value is ignored
            // The original source result should be propagated
            // This is a simplified implementation
            iris_future_decref(f);
            break;
          }
          // Source pending - fall through
        }

        // Still pending - suspend current task
        if (!g_event_loop || !g_event_loop->current_task) {
          runtime_error_at(prog, vm,
                           "await on pending future outside async context");
          exit(1);
        }

        IrisTask *task = g_event_loop->current_task;

        // For combinator/chain awaits, save IP pointing back to OP_AWAIT so we
        // re-check when resuming. Push the future back on stack for
        // re-execution.
        if (f->combinator_type >= 1 && f->combinator_type <= 5) {
          vm->ip--;                             // Point back to OP_AWAIT
          vm->stack[vm->sp++] = iris_future(f); // Re-push for next OP_AWAIT
          iris_future_incref(f);
        }

        // Save VM state to task
        iris_task_save_state(task, vm);
        task->state = TASK_SUSPENDED;
        task->awaiting = f;
        iris_future_incref(f);

        // Register callback to resume this task when future settles
        FutureCallback *cb = malloc(sizeof(FutureCallback));
        cb->task_id = task->id;
        cb->next = f->callbacks;
        f->callbacks = cb;

        // For combinators, register callbacks on source futures
        if ((f->combinator_type == 1 || f->combinator_type == 2) &&
            f->combinator) {
          IrisList *arr = (IrisList *)f->combinator;
          for (int i = 0; i < arr->length; i++) {
            if (arr->items[i].type == VAL_FUTURE) {
              IrisFuture *src = arr->items[i].as.future;
              FutureCallback *src_cb = malloc(sizeof(FutureCallback));
              src_cb->task_id = task->id;
              src_cb->next = src->callbacks;
              src->callbacks = src_cb;
            }
          }
        }

        // For chain futures, register callback on source future
        if (f->combinator_type >= 3 && f->combinator_type <= 5 && f->chained) {
          FutureCallback *src_cb = malloc(sizeof(FutureCallback));
          src_cb->task_id = task->id;
          src_cb->next = f->chained->callbacks;
          f->chained->callbacks = src_cb;
        }

        iris_future_decref(f);
        return; // Exit execution - event loop will resume later
      }
    await_done:
      break;
    }

    case OP_ASYNC_CALL: {
      int func_idx = read_int(prog->code, &vm->ip);
      int caller_frame_size = read_int(prog->code, &vm->ip);
      (void)caller_frame_size;

      IrisFunction *f = &prog->funcs[func_idx];

      // Create a new task for this async function
      int task_id = g_event_loop ? g_event_loop->next_task_id++ : 1;
      IrisTask *task = iris_task_new(task_id);

      // Copy arguments to task's locals
      for (int i = 0; i < f->param_count; i++) {
        task->locals[i] = vm->stack[vm->sp - f->param_count + i];
        iris_value_incref(&task->locals[i]);
      }
      vm->sp -= f->param_count;

      // Set task's initial state
      task->ip = f->addr;
      task->bp = 0;
      task->sp = 0;
      task->csp = 0;
      task->state = TASK_READY;

      // Add task to event loop
      if (g_event_loop) {
        event_loop_add_task(g_event_loop, task);
      }

      // Push task's future onto caller's stack
      iris_future_incref(task->future);
      vm->stack[vm->sp++] = iris_future(task->future);

      iris_task_decref(task); // event_loop now owns it
      break;
    }

    case OP_TASK_RETURN: {
      IrisValue retval = vm->stack[vm->sp - 1];
      vm->sp--;

      if (!g_event_loop || !g_event_loop->current_task) {
        // Not in async context - behave like normal return
        if (vm->csp == 0) {
          iris_value_decref(&retval);
          return;
        }
        vm->csp--;
        vm->ip = vm->call_stack[vm->csp];
        vm->bp = vm->bp_stack[vm->csp];
        vm->sp = vm->sp_stack[vm->csp];
        vm->stack[vm->sp++] = retval;
        break;
      }

      // In async context - resolve task's future
      IrisTask *task = g_event_loop->current_task;
      iris_future_resolve(task->future, retval);
      task->state = TASK_COMPLETED;
      iris_value_decref(&retval);

      return; // Exit this task's execution
    }

    case OP_PUSH_FUNC: {
      int func_idx = read_int(prog->code, &vm->ip);
      vm->stack[vm->sp++] = iris_func(func_idx);
      break;
    }

    case OP_HALT:
      return;
    }
  }
}

char *read_file(const char *filename) {
  FILE *f = fopen(filename, "r");
  if (!f) {
    fprintf(stderr, "Cannot open file: %s\n", filename);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *content = malloc(size + 1);
  fread(content, 1, size, f);
  content[size] = 0;
  fclose(f);

  return content;
}

void vm_cleanup(VM *vm) {
  for (int i = 0; i < vm->sp; i++) {
    iris_value_decref(&vm->stack[i]);
  }
  for (int i = 0; i < 256; i++) {
    iris_value_decref(&vm->locals[i]);
  }
  // Clean up any pending exception
  if (vm->has_exception) {
    iris_value_decref(&vm->exception);
    vm->has_exception = false;
  }
}

void prog_cleanup(IrisProgram *prog) {
  free(prog->code);
  for (int i = 0; i < prog->string_count; i++) {
    iris_string_decref(prog->strings[i]);
  }
  if (prog->lines) {
    free(prog->lines);
  }
  if (prog->source) {
    free(prog->source);
  }
  free(prog);
}

// ============================================================================
// Iris Embedding API Implementation
// ============================================================================

// Convert negative index to positive (Lua-style)
static int iris_abs_index(IrisState *I, int index) {
  if (index >= 0)
    return index;
  return I->api_top + index + 1;
}

// Create a new Iris state
IrisState *iris_open(void) {
  IrisState *I = calloc(1, sizeof(IrisState));
  if (!I)
    return NULL;

  I->api_top = 0;
  I->global_count = 0;
  I->cfunc_count = 0;
  I->last_status = IRIS_OK;
  I->prog = NULL;
  I->error_msg[0] = '\0';

  return I;
}

// Close and free an Iris state
void iris_close(IrisState *I) {
  if (!I)
    return;

  // Clean up API stack
  for (int i = 0; i < I->api_top; i++) {
    iris_value_decref(&I->api_stack[i]);
  }

  // Clean up globals
  for (int i = 0; i < I->global_count; i++) {
    iris_value_decref(&I->globals[i].value);
  }

  // Clean up VM
  vm_cleanup(&I->vm);

  // Clean up program
  if (I->prog) {
    prog_cleanup(I->prog);
  }

  free(I);
}

// Get the current stack top (number of elements)
int iris_gettop(IrisState *I) { return I->api_top; }

// Set the stack top
void iris_settop(IrisState *I, int index) {
  if (index >= 0) {
    // Grow stack with nil-equivalent values (zero integers)
    while (I->api_top < index) {
      I->api_stack[I->api_top].type = VAL_INT;
      I->api_stack[I->api_top].as.num = 0;
      I->api_top++;
    }
    // Shrink stack, decref removed values
    while (I->api_top > index) {
      I->api_top--;
      iris_value_decref(&I->api_stack[I->api_top]);
    }
  } else {
    iris_settop(I, I->api_top + index + 1);
  }
}

// Pop n values from the stack
void iris_pop(IrisState *I, int n) { iris_settop(I, -n - 1); }

// Push a copy of value at index
void iris_pushvalue(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx >= 0 && idx < I->api_top) {
    I->api_stack[I->api_top] = I->api_stack[idx];
    iris_value_incref(&I->api_stack[I->api_top]);
    I->api_top++;
  }
}

// Push nil (represented as 0 integer since Iris doesn't have nil)
void iris_pushnil(IrisState *I) {
  I->api_stack[I->api_top].type = VAL_INT;
  I->api_stack[I->api_top].as.num = 0;
  I->api_top++;
}

// Push an integer
void iris_pushinteger(IrisState *I, int64_t n) {
  I->api_stack[I->api_top++] = iris_int(n);
}

// Push a boolean
void iris_pushboolean(IrisState *I, bool b) {
  I->api_stack[I->api_top++] = iris_bool(b);
}

// Push a string (copies the string)
void iris_pushstring(IrisState *I, const char *s) {
  IrisString *str = iris_string_copy(s);
  I->api_stack[I->api_top++] = iris_string(str);
}

// Push a string with explicit length
void iris_pushlstring(IrisState *I, const char *s, size_t len) {
  IrisString *str = iris_string_new(s, len);
  I->api_stack[I->api_top++] = iris_string(str);
}

// Check type at index
bool iris_isinteger(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;
  return I->api_stack[idx].type == VAL_INT;
}

bool iris_isstring(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;
  return I->api_stack[idx].type == VAL_STRING;
}

bool iris_isboolean(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;
  return I->api_stack[idx].type == VAL_BOOL;
}

bool iris_isstruct(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;
  return I->api_stack[idx].type == VAL_STRUCT;
}

bool iris_isobject(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;
  return I->api_stack[idx].type == VAL_CLASS;
}

bool iris_isnil(IrisState *I, int index) {
  // Since Iris doesn't have nil, we treat 0 as nil-like
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return true;
  IrisValue *v = &I->api_stack[idx];
  return v->type == VAL_INT && v->as.num == 0;
}

// Get type at index
IrisType iris_type(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return IRIS_TNIL;

  switch (I->api_stack[idx].type) {
  case VAL_INT:
    return IRIS_TINT;
  case VAL_STRING:
    return IRIS_TSTRING;
  case VAL_BOOL:
    return IRIS_TBOOL;
  case VAL_STRUCT:
    return IRIS_TSTRUCT;
  case VAL_CLASS:
    return IRIS_TOBJECT;
  case VAL_LIST:
    return IRIS_TLIST;
  case VAL_FUTURE:
    return IRIS_TFUTURE;
  case VAL_FUNC:
    return IRIS_TNIL; // Functions not exposed to API yet
  }
  return IRIS_TNIL;
}

// Get type name
const char *iris_typename(IrisState *I, IrisType t) {
  (void)I;
  switch (t) {
  case IRIS_TNIL:
    return "nil";
  case IRIS_TINT:
    return "integer";
  case IRIS_TSTRING:
    return "string";
  case IRIS_TBOOL:
    return "boolean";
  case IRIS_TSTRUCT:
    return "struct";
  case IRIS_TOBJECT:
    return "object";
  case IRIS_TLIST:
    return "list";
  case IRIS_TFUTURE:
    return "Future";
  }
  return "unknown";
}

// Convert to integer
int64_t iris_tointeger(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return 0;

  IrisValue *v = &I->api_stack[idx];
  if (v->type == VAL_INT)
    return v->as.num;
  if (v->type == VAL_BOOL)
    return v->as.boolean ? 1 : 0;
  return 0;
}

// Convert to boolean
bool iris_toboolean(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return false;

  IrisValue *v = &I->api_stack[idx];
  if (v->type == VAL_BOOL)
    return v->as.boolean;
  if (v->type == VAL_INT)
    return v->as.num != 0;
  if (v->type == VAL_STRING)
    return v->as.str->length > 0;
  return true; // Structs and objects are truthy
}

// Convert to string (returns NULL if not a string)
const char *iris_tostring(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return NULL;

  IrisValue *v = &I->api_stack[idx];
  if (v->type == VAL_STRING)
    return v->as.str->data;
  return NULL;
}

// Get string length
size_t iris_rawlen(IrisState *I, int index) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top)
    return 0;

  IrisValue *v = &I->api_stack[idx];
  if (v->type == VAL_STRING)
    return v->as.str->length;
  return 0;
}

// Argument checking helpers for native functions
int64_t iris_checkinteger(IrisState *I, int arg) {
  if (!iris_isinteger(I, arg)) {
    snprintf(I->error_msg, sizeof(I->error_msg),
             "bad argument #%d (integer expected, got %s)", arg,
             iris_typename(I, iris_type(I, arg)));
    I->last_status = IRIS_ERRTYPE;
  }
  return iris_tointeger(I, arg);
}

const char *iris_checkstring(IrisState *I, int arg) {
  if (!iris_isstring(I, arg)) {
    snprintf(I->error_msg, sizeof(I->error_msg),
             "bad argument #%d (string expected, got %s)", arg,
             iris_typename(I, iris_type(I, arg)));
    I->last_status = IRIS_ERRTYPE;
    return "";
  }
  return iris_tostring(I, arg);
}

bool iris_checkboolean(IrisState *I, int arg) {
  if (!iris_isboolean(I, arg)) {
    snprintf(I->error_msg, sizeof(I->error_msg),
             "bad argument #%d (boolean expected, got %s)", arg,
             iris_typename(I, iris_type(I, arg)));
    I->last_status = IRIS_ERRTYPE;
  }
  return iris_toboolean(I, arg);
}

void iris_argcheck(IrisState *I, bool cond, int arg, const char *msg) {
  if (!cond) {
    snprintf(I->error_msg, sizeof(I->error_msg), "bad argument #%d (%s)", arg,
             msg);
    I->last_status = IRIS_ERRTYPE;
  }
}

// Get last error message
const char *iris_error(IrisState *I) { return I->error_msg; }

// Set error message
void iris_seterror(IrisState *I, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vsnprintf(I->error_msg, sizeof(I->error_msg), fmt, args);
  va_end(args);
  I->last_status = IRIS_ERRRUN;
}

// Bridge function for C functions called from Iris
static void iris_cfunc_bridge(VM *vm, int argc) {
  IrisState *I = g_current_iris_state;
  if (!I || !g_current_native_func_name)
    return;

  // Save current api_top
  int saved_top = I->api_top;

  // Move arguments from VM stack to API stack
  for (int i = 0; i < argc; i++) {
    I->api_stack[I->api_top] = vm->stack[vm->sp - argc + i];
    iris_value_incref(&I->api_stack[I->api_top]);
    I->api_top++;
  }
  vm->sp -= argc;

  // Find the C function by name
  int cfunc_idx = -1;
  for (int i = 0; i < I->cfunc_count; i++) {
    if (strcmp(I->cfuncs[i].name, g_current_native_func_name) == 0) {
      cfunc_idx = i;
      break;
    }
  }

  // Call the C function
  if (cfunc_idx >= 0) {
    int nresults = I->cfuncs[cfunc_idx].fn(I);

    // Move return values from API stack to VM stack
    for (int i = 0; i < nresults && I->api_top > saved_top; i++) {
      vm->stack[vm->sp++] = I->api_stack[--I->api_top];
    }
  }

  // Clean up remaining API stack values
  while (I->api_top > saved_top) {
    iris_value_decref(&I->api_stack[--I->api_top]);
  }
}

// Register a C function
void iris_register(IrisState *I, const char *name, IrisCFunction fn,
                   int nparams) {
  if (I->cfunc_count >= 64)
    return;

  // Store in C function registry
  strcpy(I->cfuncs[I->cfunc_count].name, name);
  I->cfuncs[I->cfunc_count].fn = fn;
  I->cfuncs[I->cfunc_count].nparams = nparams;
  I->cfunc_count++;

  // Register with the program if loaded
  if (I->prog) {
    register_native(I->prog, name, iris_cfunc_bridge, nparams < 0 ? 0 : nparams,
                    0);
  }
}

// Load and compile source code
IrisStatus iris_loadstring(IrisState *I, const char *source) {
  if (I->prog) {
    prog_cleanup(I->prog);
  }

  // Set current state so compile_with_path can register C functions
  g_current_iris_state = I;
  I->prog = compile(source);
  g_current_iris_state = NULL;

  if (!I->prog) {
    snprintf(I->error_msg, sizeof(I->error_msg), "compilation failed");
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  I->last_status = IRIS_OK;
  return IRIS_OK;
}

// Load and compile from file
IrisStatus iris_loadfile(IrisState *I, const char *path) {
  char *source = read_file(path);
  if (!source) {
    snprintf(I->error_msg, sizeof(I->error_msg), "cannot open file: %s", path);
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  if (I->prog) {
    prog_cleanup(I->prog);
  }

  // Set current state so compile_with_path can register C functions
  g_current_iris_state = I;
  I->prog = compile_with_path(source, path);
  g_current_iris_state = NULL;
  free(source);

  if (!I->prog) {
    snprintf(I->error_msg, sizeof(I->error_msg), "compilation failed");
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  I->last_status = IRIS_OK;
  return IRIS_OK;
}

// Run the loaded program
IrisStatus iris_run(IrisState *I) {
  if (!I->prog) {
    snprintf(I->error_msg, sizeof(I->error_msg), "no program loaded");
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  // Set global state for C function bridging
  g_current_iris_state = I;

  // Reset VM
  memset(&I->vm, 0, sizeof(VM));

  // Execute
  execute(&I->vm, I->prog);

  g_current_iris_state = NULL;

  I->last_status = IRIS_OK;
  return IRIS_OK;
}

// Protected call (currently same as run, but with error catching)
IrisStatus iris_pcall(IrisState *I, int nargs, int nresults) {
  (void)nargs;
  (void)nresults;

  if (!I->prog) {
    snprintf(I->error_msg, sizeof(I->error_msg), "no program loaded");
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  g_current_iris_state = I;
  memset(&I->vm, 0, sizeof(VM));
  execute(&I->vm, I->prog);
  g_current_iris_state = NULL;

  if (I->vm.has_exception) {
    snprintf(I->error_msg, sizeof(I->error_msg), "unhandled exception");
    I->last_status = IRIS_ERRRUN;
    return IRIS_ERRRUN;
  }

  I->last_status = IRIS_OK;
  return IRIS_OK;
}

// Find global by name
static int iris_find_global(IrisState *I, const char *name) {
  for (int i = 0; i < I->global_count; i++) {
    if (strcmp(I->globals[i].name, name) == 0) {
      return i;
    }
  }
  return -1;
}

// Get global variable, pushes to stack
IrisType iris_getglobal(IrisState *I, const char *name) {
  int idx = iris_find_global(I, name);
  if (idx < 0) {
    iris_pushnil(I);
    return IRIS_TNIL;
  }

  I->api_stack[I->api_top] = I->globals[idx].value;
  iris_value_incref(&I->api_stack[I->api_top]);
  I->api_top++;

  return iris_type(I, -1);
}

// Set global variable, pops from stack
void iris_setglobal(IrisState *I, const char *name) {
  if (I->api_top <= 0)
    return;

  int idx = iris_find_global(I, name);
  if (idx < 0) {
    if (I->global_count >= 128)
      return;
    idx = I->global_count++;
    strcpy(I->globals[idx].name, name);
    I->globals[idx].value.type = VAL_INT;
    I->globals[idx].value.as.num = 0;
  }

  iris_value_decref(&I->globals[idx].value);
  I->globals[idx].value = I->api_stack[--I->api_top];
}

// Get field from struct/object at index
void iris_getfield(IrisState *I, int index, const char *field) {
  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top) {
    iris_pushnil(I);
    return;
  }

  IrisValue *v = &I->api_stack[idx];

  if (v->type == VAL_STRUCT) {
    IrisStruct *s = v->as.instance;
    StructType *st = &I->prog->struct_types[s->type_id];
    for (int i = 0; i < st->field_count; i++) {
      if (strcmp(st->fields[i], field) == 0) {
        I->api_stack[I->api_top] = s->fields[i];
        iris_value_incref(&I->api_stack[I->api_top]);
        I->api_top++;
        return;
      }
    }
  } else if (v->type == VAL_CLASS) {
    IrisClass *c = v->as.object;
    ClassType *ct = &I->prog->class_types[c->class_id];
    for (int i = 0; i < ct->field_count; i++) {
      if (strcmp(ct->fields[i], field) == 0) {
        I->api_stack[I->api_top] = c->fields[i];
        iris_value_incref(&I->api_stack[I->api_top]);
        I->api_top++;
        return;
      }
    }
  }

  iris_pushnil(I);
}

// Set field in struct/object at index (pops value from stack)
void iris_setfield(IrisState *I, int index, const char *field) {
  if (I->api_top <= 0)
    return;

  int idx = iris_abs_index(I, index) - 1;
  if (idx < 0 || idx >= I->api_top - 1)
    return;

  IrisValue *v = &I->api_stack[idx];
  IrisValue new_val = I->api_stack[--I->api_top];

  if (v->type == VAL_STRUCT) {
    IrisStruct *s = v->as.instance;
    StructType *st = &I->prog->struct_types[s->type_id];
    for (int i = 0; i < st->field_count; i++) {
      if (strcmp(st->fields[i], field) == 0) {
        iris_value_decref(&s->fields[i]);
        s->fields[i] = new_val;
        return;
      }
    }
  } else if (v->type == VAL_CLASS) {
    IrisClass *c = v->as.object;
    ClassType *ct = &I->prog->class_types[c->class_id];
    for (int i = 0; i < ct->field_count; i++) {
      if (strcmp(ct->fields[i], field) == 0) {
        iris_value_decref(&c->fields[i]);
        c->fields[i] = new_val;
        return;
      }
    }
  }

  iris_value_decref(&new_val);
}
#endif
