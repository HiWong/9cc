#ifndef _CC_H
#define _CC_H

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <float.h>
#include <wchar.h>
#include <stdbool.h>
#include <time.h>
#include <locale.h>
#include <ctype.h>

#include "mcc.h"
#include "utils.h"

// alloc.c
extern void *alloc_node(void);
extern void *alloc_token(void);
extern void *alloc_macro(void);

// value
#define VALUE_U(v)    ((v).u)
#define VALUE_I(v)    ((v).u)
#define VALUE_D(v)    ((v).d)
#define VALUE_P(v)    ((v).p)
#define VALUE_G(v)    ((v).g)

union value {
    unsigned long long u;
    long double d;
    void *p;
    void (*g) ();
};

// source
struct source {
    unsigned line;
    unsigned column;
    const char *file;
};

enum {
#define _a(a, b, c)     a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#define _k(a, b, c)     a,
#include "token.def"
    TOKEND
};

#define ID_BITS    10

// token
struct token {
    int id:ID_BITS;
    int kind:ID_BITS;
    bool bol:1;                // beginning of line
    bool space:1;                // leading space
    const char *name;
    struct source src;
    struct hideset *hideset;
};

// input.c
#define MAX_UNREADC  8

struct cc_char {
    bool dirty:1;
    int ch:16;
    unsigned line;
    unsigned column;
};

struct file {
    int kind:3;
    bool bol:1;                // beginning of line
    bool stub:1;
    int histp:8;
    int charp:8;
    char *buf;
    char *pc;
    char *pe;
    long bread;
    FILE *fp;                // FILE handle
    size_t pos;                // input string position
    const char *file;        // file name or input string
    const char *name;        // buffer name
    unsigned line;
    unsigned column;
    struct vector *ifstubs;
    struct cc_char hists[MAX_UNREADC + 1];        // readc history
    struct cc_char chars[MAX_UNREADC];        // readc ungets
    struct vector *buffer;        // lex ungets
    struct vector *tokens;        // parser ungets
};

struct ifstub {
    int id:10;
    bool b:1;
    struct source src;
};

extern void input_init(const char *file);
extern int readc(void);
extern void unreadc(int c);

extern struct file *with_string(const char *input, const char *name);
extern struct file *with_file(const char *file, const char *name);
extern struct file *with_buffer(struct vector *v);

extern void file_sentinel(struct file *f);
extern void file_unsentinel(void);
extern void file_stub(struct file *f);
extern void file_unstub(void);
extern struct file *current_file(void);

extern void if_sentinel(struct ifstub *i);
extern void if_unsentinel(void);
extern struct ifstub *new_ifstub(struct ifstub *i);
extern struct ifstub *current_ifstub(void);

// lex.c
extern struct source source;
extern struct token *token;
extern struct token *ahead_token;
extern struct token *newline_token;
extern struct token *space_token;

extern int isletter(int c);
extern int isxalpha(int c);

#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

extern struct token *lex(void);
extern void unget(struct token *t);
extern struct token *header_name(void);
extern struct token *new_token(struct token *tok);
extern void skip_ifstub(void);

extern int gettok(void);
extern struct token *lookahead(void);
extern void expect(int t);
extern void match(int t, int follow[]);
extern int skipto(int (*test[]) (struct token *));
extern const char *id2s(int t);
extern const char *unwrap_scon(const char *name);

extern void print_buffer_stat(void);

#define FARRAY(...)  ((int (*[]) (struct token *)){__VA_ARGS__, NULL})

// cpp.c
// macro kind
enum {
    MACRO_OBJ,
    MACRO_FUNC,
    MACRO_SPECIAL
};

struct macro {
    int kind:3;
    bool vararg:1;
    bool builtin:1;
    struct vector *body;
    struct vector *params;
    void (*handler) (struct token *);        // special macro handler
    struct source src;
};

extern void cpp_init(struct vector *options);
extern struct token *get_pptok(void);
extern struct vector *all_pptoks(void);

// ast.h
#include "ast.h"

// eval.c
extern node_t *eval(node_t * expr, node_t * ty);
extern bool eval_cpp_cond(void);
extern node_t *int_literal_node(node_t * ty, union value v);

// expr.c
#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
extern node_t *expression(void);
extern node_t *assign_expr(void);
extern long intexpr1(node_t * ty);
extern long intexpr(void);
extern bool islvalue(node_t * node);
extern node_t *assignconv(node_t * ty, node_t * node);
// for expression in conditional statement
extern node_t *bool_expr(void);
// for expression in switch statement
extern node_t *switch_expr(void);
// bop
extern node_t *bop(int op, node_t * l, node_t * r);

// literals
extern node_t *new_integer_literal(int i);
extern node_t *new_string_literal(const char *string);

// decl.c
extern size_t extra_stack_size;

extern node_t **declaration(void);
extern node_t *translation_unit(void);
extern node_t *typename(void);
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern bool first_typename(struct token *t);
extern node_t *make_localdecl(const char *name, node_t * ty, int sclass);

// initializer.c
extern node_t *initializer(node_t * ty);
extern node_t *initializer_list(node_t * ty);
extern void decl_initializer(node_t * decl, int sclass, int kind);
extern void init_string(node_t * ty, node_t * node);
extern bool has_static_extent(node_t * sym);

// stmt.c
extern void func_body(node_t *decl);
extern node_t *make_localvar(const char *name, node_t * ty, int sclass);

// type.c
extern void type_init(void);
extern int type_op(node_t * type);
extern void prepend_type(node_t ** typelist, node_t * type);
extern void attach_type(node_t ** typelist, node_t * type);
extern node_t *qual(int t, node_t * ty);
extern node_t *unqual(node_t * ty);
extern bool eqtype(node_t * ty1, node_t * ty2);
extern bool eqarith(node_t * ty1, node_t * ty2);
extern node_t *lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern node_t *new_field(void);
extern node_t *array_type(node_t * ty);
extern node_t *ptr_type(node_t * ty);
extern node_t *func_type(void);
extern node_t *tag_type(int t, const char *tag, struct source src);
extern void set_typesize(node_t * ty);
extern node_t *find_field(node_t * ty, const char *name);
extern int indexof_field(node_t * ty, node_t * field);
extern node_t *compose(node_t * ty1, node_t * ty2);
extern bool qual_contains(node_t * ty1, node_t * ty2);
extern int qual_union(node_t * ty1, node_t * ty2);
extern bool isincomplete(node_t * ty);
extern node_t *unpack(node_t * ty);

extern node_t *chartype;        // char
extern node_t *unsignedchartype;        // unsigned char
extern node_t *signedchartype;        // signed char
extern node_t *wchartype;        // wchar_t
extern node_t *shorttype;        // short (int)
extern node_t *unsignedshorttype;        // unsigned short (int)
extern node_t *inttype;                // int
extern node_t *unsignedinttype;        // unsigned (int)
extern node_t *longtype;        // long
extern node_t *unsignedlongtype;        // unsigned long (int)
extern node_t *longlongtype;        // long long (int)
extern node_t *unsignedlonglongtype;        // unsigned long long (int)
extern node_t *floattype;        // float
extern node_t *doubletype;        // double
extern node_t *longdoubletype;        // long double
extern node_t *voidtype;        // void
extern node_t *booltype;        // bool

#define BITS(bytes)     (CHAR_BIT * (bytes))
#define BYTES(bits)     ((ROUNDUP(bits, CHAR_BIT)) / (CHAR_BIT))

extern bool isconst1(int kind);
extern bool isvolatile1(int kind);
extern bool isrestrict1(int kind);

extern bool isconst(node_t * ty);
extern bool isvolatile(node_t * ty);
extern bool isrestrict(node_t * ty);

#define isinline(ty)    (_TYPE_INLINE(ty))
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))

// operations on the unqual type
#define TYPE_KIND(ty)            _TYPE_KIND(unqual(ty))
#define TYPE_NAME(ty)            _TYPE_NAME(unqual(ty))
#define TYPE_SIZE(ty)            _TYPE_SIZE(unpack(unqual(ty)))
#define TYPE_ALIGN(ty)           _TYPE_ALIGN(unpack(unqual(ty)))
#define TYPE_LEN(ty)             _TYPE_LEN(unqual(ty))
#define TYPE_RANK(ty)            _TYPE_RANK(unpack(unqual(ty)))
#define TYPE_INLINE(ty)          _TYPE_INLINE(unqual(ty))
#define TYPE_TYPE(ty)            _TYPE_TYPE(unqual(ty))
#define TYPE_TAG(ty)             _TYPE_TAG(unqual(ty))
#define TYPE_PARAMS(ty)          _TYPE_PARAMS(unqual(ty))
#define TYPE_OLDSTYLE(ty)        _TYPE_OLDSTYLE(unqual(ty))
#define TYPE_VARG(ty)            _TYPE_VARG(unqual(ty))
#define TYPE_TSYM(ty)            _TYPE_TSYM(unqual(ty))
#define TYPE_FIELDS(ty)          _TYPE_FIELDS(unqual(ty))
#define TYPE_LIMITS_MAX(ty)      _TYPE_LIMITS_MAX(unpack(unqual(ty)))
#define TYPE_LIMITS_MIN(ty)      _TYPE_LIMITS_MIN(unpack(unqual(ty)))
#define TYPE_A_ASSIGN(ty)        _TYPE_A_ASSIGN(unqual(ty))
#define TYPE_A_CONST(ty)         _TYPE_A_CONST(unqual(ty))
#define TYPE_A_VOLATILE(ty)      _TYPE_A_VOLATILE(unqual(ty))
#define TYPE_A_RESTRICT(ty)      _TYPE_A_RESTRICT(unqual(ty))
#define TYPE_A_STATIC(ty)        _TYPE_A_STATIC(unqual(ty))
#define TYPE_A_STAR(ty)          _TYPE_A_STAR(unqual(ty))

// alias
#define rtype(ty)       TYPE_TYPE(ty)
#define TYPE_OP(ty)     type_op(ty)

extern bool isfunc(node_t * type);
extern bool isarray(node_t * type);
extern bool isptr(node_t * type);
extern bool isvoid(node_t * type);
extern bool isenum(node_t * type);
extern bool isstruct(node_t * type);
extern bool isunion(node_t * type);
extern bool isrecord(node_t * type);        // isstruct or isunion
extern bool istag(node_t * type);        // isstruct or isunion or isenum

extern bool isint(node_t * ty);
extern bool isfloat(node_t * ty);
extern bool isarith(node_t * ty);
extern bool isscalar(node_t * ty);
extern bool isptrto(node_t * ty, int kind);
extern bool isbool(node_t *ty);

// sym.c
// scope level
enum {
    CONSTANT,
    GLOBAL,
    PARAM,
    LOCAL,
};

struct table {
    int scope;
    struct table *up;
    struct dict *dict;
};

// sym
extern void symbol_init(void);
extern int scopelevel(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern bool is_current_scope(node_t *sym);
extern bool is_anonymous(const char *name);

// create an anonymous symbol
extern node_t *anonymous(struct table **tpp, int scope);

// look up a symbol from this table to previous one, and so on
extern node_t *lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern node_t *install(const char *name, struct table **tpp, int scope);

extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;

#define SCOPE  scopelevel()

// simplify.c
extern node_t * simplify(node_t *tree);

// register.c
extern void free_reg(struct reg *r);
extern struct reg *get_iarg_reg(void);
extern struct reg *get_farg_reg(void);
extern struct reg *use_int_reg(void);
extern struct reg *use_float_reg(void);
extern struct operand *make_literal_operand(const char *name);
extern struct operand *make_memory_operand(const char *name);
extern struct operand *make_register_operand(struct reg *reg);
extern const char *get_operand_name(struct operand *operand, int size);
extern void use_operand(struct operand *operand);
extern void free_operand(struct operand *operand);
extern void init_regs(void);
extern void print_register_state(void);

// gen.c
extern void emit(const char *fmt, ...);
extern void gen(node_t * tree, FILE * fp);

// print.c
extern void print_tree(node_t * tree);
extern void print_gen_tree(node_t * tree);
extern const char *type2s(node_t * ty);
extern const char *node2s(node_t * node);
extern void print_node_size(void);

// error.c
enum {
    WRN = 1,                // warning
    ERR,                        // error
    FTL,                        // fatal
};
extern unsigned errors;
extern unsigned warnings;
extern void warningf(struct source src, const char *fmt, ...);
extern void errorf(struct source src, const char *fmt, ...);
extern void fatalf(struct source src, const char *fmt, ...);
#define warning(...)  warningf(source, __VA_ARGS__)
#define error(...)    errorf(source, __VA_ARGS__)
#define fatal(...)    fatalf(source, __VA_ARGS__)

#define SAVE_ERRORS    unsigned err = errors
#define NO_ERROR       (err == errors)
#define HAS_ERROR      (err != errors)

#define cc_assert(expr)                         \
    do {                                        \
        if (!(expr)) {                          \
            error("assert failed");             \
            assert(expr);                       \
        }                                       \
    } while (0)

#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"

extern void redefinition_error(struct source src, node_t * sym);
extern void conflicting_types_error(struct source src, node_t * sym);
extern void field_not_found_error(node_t * ty, const char *name);

#endif
