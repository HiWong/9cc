#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include "cc.h"

static struct tree *condexpr(struct type *ty, struct tree *cond,
                             struct tree *then, struct tree *els);
struct func func;

#define ERR_INCOMPATIBLE_CONV \
    "incompatible type conversion from '%T' to '%T'"

#define ERR_REDEFINITION \
    "redefinition of '%s', previous definition at %S"

#define ERR_CONFLICTING_TYPES \
    "conflicting types for '%s', previous at %S"

#define ERR_DUPLICATE_MEMBER \
    "duplicate member '%s', previous declaration at %S"

#define ERR_TYPE \
    "expect type '%s', not '%T'"

#define ERR_INLINE \
    "'inline' can only appear on functions"

#define ERR_ARRAY_OF_FUNC  \
    "array of function is invalid"

#define ERR_FUNC_RET_ARRAY \
    "function cannot return array type '%T'"

#define ERR_FUNC_RET_FUNC \
    "function cannot return function type '%T'"

#define ERR_INCOMPLETE_VAR \
    "variable '%s' has incomplete type '%T'"

#define ERR_INCOMPLETE_ELEMENT \
    "array has incomplete element type '%T'"

#define ERR_INIT_EMPTY_RECORD \
    "initializer for aggregate with no elements requires explicit braces"

#define ERR_INIT_OVERRIDE \
    "initializer overrides prior initialization"

#define ERR_BOP_OPERANDS \
    "invalid operands to binary expression ('%T' and '%T')"

#define ERR_COMPARISION_INCOMPATIBLE \
    "comparison of incompatible types ('%T' and '%T')"

#define ERR_COMPARISION_PI \
    "comparison of '%s' and '%s' is illegal in ISO C"

#define ERR_PTR_TO_INCOMPATIBLE_TYPES \
    "'%T' and '%T' are not pointers to compatible types"

#define INTEGER_MAX(type)    (TYPE_LIMITS(type).max.i)
#define UINTEGER_MAX(type)   (TYPE_LIMITS(type).max.u)
#define INIT(name)  .name = do_##name
#define events(func)  func

#define add_to_list(stmt)                       \
    do {                                        \
        *func.stmt = stmt;                      \
        func.stmt = &stmt->next;                \
    } while (0)

#define link_lvar(sym)                          \
    do {                                        \
        *func.lvars = sym;                      \
        func.lvars = &sym->local;               \
    } while (0)

#define isaddrop(op)  (OPKIND(op) == ADDRL || \
                       OPKIND(op) == ADDRG || \
                       OPKIND(op) == ADDRP)

#define check_designator(d)  ensure_designator(d) ? (d) : NULL

/*=================================================================*
 *                          Events                                 *
 *=================================================================*/

// declare a global variable
static void dclgvar(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_vardecl(n);
}

// declare a function
static void dclfun(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_funcdecl(n);
}

// declare/define a type: struct/union/enum/typedef
static void deftype(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_typedecl(n);
}

// define a local variable
static void deflvar(struct symbol *n)
{
    link_lvar(n);
}

// define a local static variable
static void defsvar(struct symbol *n)
{
    link_lvar(n);    
    if (opts.ast_dump || errors())
        return;
    IR->defvar(n);
}

// define a global variable
static void defgvar(struct symbol *n)
{
    if (opts.ast_dump) {
        ast_dump_vardecl(n);
        return;
    }
    if (errors())
        return;
    IR->defvar(n);
}

// define a function
static void defun(struct symbol *n)
{
    if (opts.ast_dump) {
        ast_dump_funcdef(n);
        return;
    }
    if (errors())
        return;
    IR->defun(n);
}

// a funcall
static void funcall(struct type *fty, struct tree **args)
{
    // compare the func.call and call
    // and set the larger to func.call
    // TODO:
}

/*=================================================================*
 *                           Private                               *
 *=================================================================*/

static void skip_balance(int l, int r, const char *name)
{
    int nests = 0;

    while (1) {
        if (token_is(EOI))
            break;
        if (token_is(r)) {
            if (nests-- == 0)
                break;
        } else if (token_is(l)) {
            nests++;
        }
        gettok();
    }

    if (token_is(r))
        gettok();
    else
        error("unclosed %s, missing '%s'", name, id2s(r));
}

static void skip_to_first(int (*first) (struct token *))
{
    while (1) {
        if (token_is(EOI))
            break;
        if (first(token))
            break;
        gettok();
    }
}

static void field_not_found_error(struct source src,
                                  struct type *ty, const char *name)
{
    if (isincomplete(ty))
        error_at(src, "incomplete definition of type '%T'", ty);
    else
        error_at(src, "'%T' has no field named '%s'", ty, name);
}

static bool istypedef(const char *id)
{
    assert(id);
    struct symbol *sym = lookup(id, identifiers);
    return sym && sym->sclass == TYPEDEF;
}

static struct symbol *mklocal(const char *name, struct type *ty, int sclass)
{
    struct symbol *sym;

    assert(cscope >= LOCAL);

    // `name' must be unique
    sym = install(name, &identifiers, cscope, FUNC);
    sym->type = ty;
    sym->sclass = sclass;
    sym->defined = true;

    if (sclass == STATIC)
        events(defsvar)(sym);
    else
        events(deflvar)(sym);

    return sym;
}

static struct symbol *mktmp(const char *name, struct type *ty, int sclass)
{
    struct symbol *sym;

    sym = mklocal(name, ty, sclass);
    sym->temporary = true;
    return sym;
}

/*=================================================================*
 *                        Sema-Expression                          *
 *=================================================================*/

static struct type *integer_constant(struct token *t)
{
    int base = t->u.lit.base;
    int suffix = t->u.lit.suffix;
    unsigned long n = t->u.lit.v.u;
    struct type *ty;

    // character constant
    if (is_char_cnst(t))
        return t->u.lit.wide ? wchartype : unsignedchartype;

    switch (suffix) {
    case UNSIGNED + LONG + LONG:
        ty = unsignedlonglongtype;
        break;
    case LONG + LONG:
        if (n > INTEGER_MAX(longlongtype) && base != 0)
            ty = unsignedlonglongtype;
        else
            ty = longlongtype;
        break;
    case UNSIGNED + LONG:
        if (n > UINTEGER_MAX(unsignedlongtype))
            ty = unsignedlonglongtype;
        else
            ty = unsignedlongtype;
        break;
    case LONG:
        if (base == 0) {
            if (n > INTEGER_MAX(longtype))
                ty = longlongtype;
            else
                ty = longtype;
        } else {
            if (n > INTEGER_MAX(longlongtype))
                ty = unsignedlonglongtype;
            else if (n > UINTEGER_MAX(unsignedlongtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(longtype))
                ty = unsignedlongtype;
            else
                ty = longtype;
        }
        break;
    case UNSIGNED:
        if (n > UINTEGER_MAX(unsignedlongtype))
            ty = unsignedlonglongtype;
        else if (n > UINTEGER_MAX(unsignedinttype))
            ty = unsignedlongtype;
        else
            ty = unsignedinttype;
        break;
    default:
        if (base == 0) {
            if (n > INTEGER_MAX(longtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(inttype))
                ty = longtype;
            else
                ty = inttype;
        } else {
            if (n > INTEGER_MAX(longlongtype))
                ty = unsignedlonglongtype;
            else if (n > UINTEGER_MAX(unsignedlongtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(longtype))
                ty = unsignedlongtype;
            else if (n > UINTEGER_MAX(unsignedinttype))
                ty = longtype;
            else if (n > INTEGER_MAX(inttype))
                ty = unsignedinttype;
            else
                ty = inttype;
        }
        break;
    }

    // overflow
    if (TYPE_OP(ty) == INT && n > INTEGER_MAX(longlongtype))
        error("integer constant overflow: %s", TOK_LIT_STR(t));

    return ty;
}

static struct type *float_constant(struct token *t)
{
    int suffix = t->u.lit.suffix;
    switch (suffix) {
    case FLOAT:
        return floattype;
    case LONG + DOUBLE:
        return longdoubletype;
    default:
        return doubletype;
    }
}

static void string_constant(struct token *t, struct symbol *sym)
{
    const char *s = TOK_LIT_STR(t);
    struct type *ty;
    if (t->u.lit.wide) {
        size_t len = strlen(s);
        wchar_t *ws = xmalloc(sizeof(wchar_t) * (len + 1));
        errno = 0;
        size_t wlen = mbstowcs(ws, s, len);
        if (errno == EILSEQ)
            error("invalid multibyte sequence: %s", s);
        free(ws);
        assert(wlen <= len + 1);
        ty = array_type(wchartype);
        TYPE_LEN(ty) = wlen;
        set_typesize(ty);
    } else {
        ty = array_type(chartype);
        TYPE_LEN(ty) = strlen(s) + 1;
        set_typesize(ty);
    }
    sym->type = ty;
}

/**
 * Object,Lvalue,Designator
 *
 * An _object_ is a region of memory that can be examined and stored into.
 *
 * An _lvalue_ is an expression that refers to an _object_ in such a way
 * that the object may be examined or altered.
 *
 * Only an _lvalue_ expression **may be** used on the left-hand side of an
 * assignment.
 *
 * An _lvalue_ dose **NOT** necessarily permit modification of the _object_
 * it designates.
 *
 * A _function_ designator is a value of function type. It is neither an
 * _object_ nor an _lvalue_.
 *
 * Functions and objects are often treated **differently** in C.
 */
static bool islvalue(struct tree *expr)
{
    if (OPKIND(expr->op) == INDIR)
        return true;
    if (expr->op == BFIELD)
        return true;
    if (isaddrop(expr->op) && isarray(expr->type))
        return true;
    return false;
}

static bool assignable(struct tree *expr, struct source src)
{
    struct type *ty = expr->type;
    if (!islvalue(expr)) {
        error_at(src, "expression is not assignable (not an lvalue)");
        return false;
    }
    if (isarray(ty)) {
        error_at(src, "array type '%T' is not assignable", ty);
        return false;
    }
    if (isconst(ty)) {
        error_at(src, "read-only variable is not assignable");
        return false;
    }
    return true;
}

/**
 *  Explicit Casting Conversions
 *
 *  Destination type            Permitted source type
 *  --------------------------------------------------
 *  any arith                   any arith
 *
 *  any integer                 any pointer
 *
 *  pointer to (object) T, or   (a) any integer type
 *  (void *)                    (b) (void *)
 *                              (c) pointer to (object) Q, for any Q
 *
 *  pointer to (function) T     (a) any integer type
 *                              (b) pointer to (function) Q, for any Q
 *
 *  struct or union             none; not a permitted cast
 *
 *  array or function           none; not a permitted cast
 *
 *  void                        any type
 */

static bool castable(struct type *dty, struct type *sty)
{
    if (isvoid(dty))
        return true;
    if (isarith(dty) && isarith(sty))
        return true;
    if (isint(dty) && isptr(sty))
        return true;
    if (isptrto(dty, FUNCTION)) {
        if (isint(sty) || isptrto(sty, FUNCTION))
            return true;
    } else if (isptr(dty)) {
        if (isint(sty))
            return true;
        if (isptr(sty) && !isptrto(sty, FUNCTION))
            return true;
    }

    return false;
}

static bool addable_ptr(struct tree *expr, struct source src)
{
    struct type *rty = rtype(expr->type);
    if (isfunc(rty) || isincomplete(rty)) {
        error_at(src, "increment/decrement of invalid type "
                 "'%T' (pointer to unknown size)",
                 expr->type);
        return false;
    }
    return true;
}

static bool increasable(struct tree *expr, struct source src)
{
    if (!isscalar(expr->type)) {
        error_at(src, ERR_TYPE, "scalar", expr->type);
        return false;
    }

    if (!assignable(expr, src))
        return false;

    if (isptr(expr->type))
        return addable_ptr(expr, src);
    else
        return true;
}

// constant 0 (int or pointer)
static bool isnullptr(struct tree *n)
{
    struct type *ty = n->type;

    return OPKIND(n->op) == CNST &&
        ((isint(ty) && n->s.value.i == 0) ||
         (isptrto(ty, VOID) && n->s.value.p == NULL));
}

/// conversion

static struct tree *rettype(struct type *ty, struct tree *n)
{
    struct tree *ret;

    ret = ast_expr(n->op, ty, n->kids[0], n->kids[1]);
    ret->s = n->s;

    return ret;
}

static struct tree *lvalue(struct tree *n)
{
    assert(OPKIND(n->op) == INDIR);
    return n->kids[0];
}

static struct tree *rvalue(struct tree *n)
{
    struct type *ty;
    assert(isptr(n->type));

    ty = unqual(rtype(n->type));
    
    assert(!isfunc(ty) && !isarray(ty));

    return ast_expr(mkop(INDIR, ty), ty, n, NULL);
}

static struct tree *cast_arith(struct type *ty, struct tree *n)
{
    struct type *sty, *dty;
    int sop, dop, op;

    sty = unqual(n->type);
    dty = unqual(ty);
    sop = sty->op == ENUM ? sty->type->op : sty->op;
    dop = dty->op == ENUM ? dty->type->op : dty->op;

    if (sop == dop && sty->size == dty->size)
        return n;

    if (sop == INT)
        op = CVI;
    else if (sop == UNSIGNED)
        op = CVU;
    else if (sop == FLOAT)
        op = CVF;
    else
        CC_UNAVAILABLE();

    return fold(mkop(op, ty), ty, n, NULL);
}

static struct tree *castip(struct type *ty, struct tree *n)
{    
    n = cast_arith(unsignedptrtype, n);
        
    return fold(mkop(CVU, ty), ty, n, NULL);
}

static struct tree *castpi(struct type *ty, struct tree *n)
{
    n = fold(mkop(CVP, unsignedptrtype), unsignedptrtype, n, NULL);

    return cast_arith(ty, n);
}

static struct tree *castpp(struct type *ty, struct tree *n)
{
    return rettype(ty, n);
}

static struct tree *cast2bool(struct type *ty, struct tree *n)
{
    if (isbool(n->type))
        return n;

    return condexpr(ty, n, cnsti(1, booltype), cnsti(0, booltype));
}

/// cast 'n' to type 'ty'
static struct tree *cast(struct type *ty, struct tree *n)
{
    struct type *sty, *dty;

    sty = unqual(n->type);
    dty = unqual(ty);
    
    if (isbool(dty)) {
        // cast to bool is special
        return cast2bool(ty, n);
    } else if (isint(dty)) {
        // cast to integer
        if (sty->op == INT ||
            sty->op == UNSIGNED ||
            sty->op == FLOAT ||
            sty->op == ENUM)
            return cast_arith(ty, n);
        if (sty->op == POINTER)
            return castpi(ty, n);
    } else if (isfloat(dty)) {
        // cast to floating
        if (sty->op == INT ||
            sty->op == UNSIGNED ||
            sty->op == FLOAT ||
            sty->op == ENUM)
            return cast_arith(ty, n);
    } else if (isptr(dty)) {
        // cast to pointer
        if (sty->op == POINTER)
            return castpp(ty, n);
        if (sty->op == INT ||
            sty->op == UNSIGNED ||
            sty->op == ENUM)
            return castip(ty, n);
    } else if (isstruct(dty) || isunion(dty)) {
        // cast to struct/union
        return n;
    } else if (isvoid(dty)) {
        // cast to void
        if (isvoid(sty))
            return n;
        else
            return ast_expr(RIGHT, ty, n, NULL);
    }

    CC_UNAVAILABLE();
}

static struct tree *explicit_cast(struct type *ty, struct tree *n)
{
    n = cast(ty, n);
    if (islvalue(n))
        n = ast_expr(RIGHT, ty, n, NULL);
    return n;
}

static struct tree *decay(struct tree *expr)
{
    assert(expr);
    switch (TYPE_KIND(expr->type)) {
    case FUNCTION:
        // FunctionToPointerDecay
        return rettype(ptr_type(expr->type), expr);

    case ARRAY:
        // ArrayToPointerDecay
        return rettype(ptr_type(rtype(expr->type)), expr);

    default:
        return expr;
    }
}

static struct tree *ltor(struct tree *expr)
{
    // LValueToRValue
    if (islvalue(expr))
        return rettype(unqual(expr->type), expr);
    else
        return expr;
}

// Universal Unary Conversion
static struct tree *conv(struct tree *expr)
{
    assert(expr);

    expr = ltor(expr);

    switch (TYPE_KIND(expr->type)) {
    case _BOOL:
    case CHAR:
    case SHORT:
        return cast(inttype, expr);

    case ENUM:
        return cast(rtype(expr->type), expr);

    case FUNCTION:
    case ARRAY:
        return decay(expr);

    default:
        return expr;
    }
}

// Default function argument conversion
static struct tree *conva(struct tree *expr)
{
    assert(expr);

    expr = ltor(expr);

    switch (TYPE_KIND(expr->type)) {
    case FLOAT:
        return cast(doubletype, expr);

    default:
        return conv(expr);
    }
}

// Universal Binary Conversion
static struct type *conv2(struct type *l, struct type *r)
{
    assert(isarith(l));
    assert(isarith(r));

    assert(TYPE_SIZE(l) >= TYPE_SIZE(inttype));
    assert(TYPE_SIZE(r) >= TYPE_SIZE(inttype));

    struct type *max = TYPE_RANK(l) > TYPE_RANK(r) ? l : r;
    if (isfloat(l) || isfloat(r) || TYPE_OP(l) == TYPE_OP(r))
        return max;

    struct type *u = TYPE_OP(l) == UNSIGNED ? l : r;
    struct type *s = TYPE_OP(l) == INT ? l : r;
    assert(unqual(s) == s);

    if (TYPE_RANK(u) >= TYPE_RANK(s))
        return u;

    if (TYPE_SIZE(u) < TYPE_SIZE(s)) {
        return s;
    } else {
        if (s == inttype)
            return unsignedinttype;
        else if (s == longtype)
            return unsignedlongtype;
        else
            return unsignedlonglongtype;
    }

    return l;
}

/**
 *  Assignment Conversions
 *
 *  Left side type              Permitted right side type
 *  ------------------------------------------------------
 *  any arith                   any arith
 *
 *  _Bool                       any pointer
 *
 *  struct or union             compatible struct or union
 *
 *  (void *)                    (a) the constant 0
 *                              (b) pointer to (object) T
 *                              (c) (void *)
 *
 *  pointer to (object) T       (a) the constant 0
 *                              (b) pointer to T2, where
 *                                  T and T2 are compatible
 *                              (c) (void *)
 *
 *  pointer to (function) F     (a) the constant 0
 *                              (b) pointer to F2, where
 *                                  F and F2 are compatible
 */

static struct tree *assignconv(struct type *dty, struct tree *expr)
{
    struct type *sty;

    expr = decay(expr);
    expr = ltor(expr);
    sty = expr->type;

    if (isarith(dty) && isarith(sty))
        goto ok;

    if (isbool(dty) && isptr(sty))
        goto ok;

    if ((isstruct(dty) && isstruct(sty)) ||
        (isunion(dty) && isunion(sty))) {
        if (!eqtype(unqual(dty), unqual(sty)))
            return NULL;

        goto ok;
    }

    if (isnullptr(expr) && isptr(dty))
        goto ok;

    if ((isptrto(dty, VOID) && isptr(sty)) ||
        (isptrto(sty, VOID) && isptr(dty))) {
        struct type *rty1, *rty2;

        rty1 = rtype(dty);
        rty2 = rtype(sty);
        if (isfunc(rty1) || isfunc(rty2))
            return NULL;
        if (!qual_contains(rty1, rty2))
            return NULL;

        goto ok;
    }

    if (isptr(dty) && isptr(sty)) {
        struct type *rty1, *rty2;

        rty1 = rtype(dty);
        rty2 = rtype(sty);
        if (!eqtype(unqual(rty1), unqual(rty2)))
            return NULL;
        if (!qual_contains(rty1, rty2))
            return NULL;

        goto ok;
    }

    // fail
    return NULL;
 ok:
    return cast(dty, expr);
}

// return NULL on error.
static struct tree **argsconv1(struct type **params, size_t nparams,
                               struct tree **args, size_t nargs,
                               bool oldstyle, struct source src)
{
    struct list *list = NULL;
    size_t ncmp = MIN(nparams, nargs);

    for (size_t i = 0; i < ncmp; i++) {
        struct tree *arg = args[i];
        struct type *dty = params[i];
        struct type *sty = arg->type;
        arg = assignconv(dty, arg);
        if (arg) {
            list = list_append(list, arg);
        } else {
            if (oldstyle) {
                warning_at(src, ERR_INCOMPATIBLE_CONV, sty, dty);
            } else {
                error_at(src, ERR_INCOMPATIBLE_CONV, sty, dty);
                return NULL;
            }
        }
    }
    for (size_t i = ncmp; i < nargs; i++) {
        struct tree *arg = args[i];
        list = list_append(list, conva(arg));
    }

    return ltoa(&list, FUNC);
}

static struct tree **argsconv(struct type *fty, struct tree **args,
                              struct source src)
{
    struct list *list = NULL;
    struct type **params = TYPE_PROTO(fty);
    size_t nparams = length(params);
    size_t nargs = length(args);
    bool oldstyle = TYPE_OLDSTYLE(fty);

    if (oldstyle) {
        if (nparams > nargs)
            warning_at(src, "too few arguments to function call");

        return argsconv1(params, nparams, args, nargs, oldstyle, src);
    }

    // prototype

    if (nparams == 0) {
        if (nargs > 0) {
            error_at(src,
                     "too many arguments to function call, "
                     "expected %d, have %d", nparams, nargs);
            return NULL;
        }
        return ltoa(&list, FUNC);
    }

    bool vargs = TYPE_VARG(fty);
    if (nparams <= nargs) {
        if (nparams < nargs && !vargs) {
            error_at(src,
                     "too many arguments to function call, "
                    "expected %d, have %d", nparams, nargs);
            return NULL;
        }

        return argsconv1(params, nparams, args, nargs, oldstyle, src);
    } else {
        if (vargs)
            error_at(src,
                     "too few arguments to function call, "
                     "expected at least %d, have %d", nparams, nargs);
        else
            error_at(src,
                     "too few arguments to function call, "
                     "expected %d, have %d", nparams, nargs);
        return NULL;
    }
}

static struct tree *mkiliteral(struct type *ty, long i)
{
    struct tree *expr;

    expr = ast_expr(mkop(CNST, ty), ty, NULL, NULL);
    expr->s.value.i = i;
    return expr;
}

static struct tree *arith_literal(struct token *t,
                                  struct type * (*cnst) (struct token *))
{
    struct type *ty;
    struct tree *expr;

    ty = cnst(t);
    expr = ast_expr(mkop(CNST, ty), ty, NULL, NULL);
    expr->s.value = token->u.lit.v;
    return expr;
}

static struct tree *string_literal(struct token *t,
                                   void (*cnst) (struct token *,
                                                 struct symbol *))
{
    const char *name = TOK_LIT_STR(t);
    struct symbol *sym = lookup(name, constants);

    if (!sym) {
        sym = install(name, &constants, CONSTANT, PERM);
        cnst(t, sym);
    }

    if (!sym->x.name) {
        sym->x.name = gen_string_label();
        sym->sclass = STATIC;
        sym->u.c.string = true;
        sym->defined = true;
    }

    return mkref(sym);
}

static struct tree *incr(int op, struct tree *expr, struct tree *cnst,
                         struct source src)
{
    return actions.assign('=', expr, actions.bop(op, expr, cnst, src), src);
}

// implicit function declaration: int id();
static struct symbol *implicit_func_decl(const char *id)
{
    struct type *ftype = func_type(inttype);
    struct list *list = NULL;
    ftype->u.f.oldstyle = true;
    ftype->u.f.proto = ltoa(&list, PERM);

    struct symbol *sym = install(id, &externals, GLOBAL, PERM);
    sym->sclass = EXTERN;
    sym->type = ftype;
    sym->src = source;

    events(dclfun)(sym);
    warning("implicit declaration of '%s'", id);

    return sym;
}

struct tree *mkref(struct symbol *sym)
{
    int op;
    struct type *ty = sym->type;
    struct tree *ret;

    if (has_static_extent(sym))
        op = ADDRG;
    else if (sym->scope == PARAM)
        op = ADDRP;
    else
        op = ADDRL;

    if (isfunc(ty))
        ret = ast_expr(mkop(op, funcptype), ty, NULL, NULL);
    else if (isarray(ty))
        ret = ast_expr(mkop(op, voidptype), ty, NULL, NULL);
    else
        ret = ast_expr(mkop(op, voidptype), ptr_type(ty), NULL, NULL);

    ret->s.sym = sym;
    use(sym);

    if (isptr(ret->type))
        return rvalue(ret);
    else
        return ret;
}

static struct tree *assign(struct symbol *sym, struct tree *r)
{
    return actions.assign('=', mkref(sym), r, sym->src);
}

// initialization assignment (return the ref)
static struct tree *iassign(struct symbol *sym, struct tree *r)
{
    struct type *ty;
    struct tree *ref, *l;

    assert(r);
    ty = sym->type;
    ref = l = mkref(sym);

    if (!isarray(ty))
        l = lvalue(l);

    actions.gen(ast_expr(mkop(ASGN, ty), ty, l, r));

    return ref;
}

static struct tree *condexpr(struct type *ty, struct tree *cond,
                             struct tree *then, struct tree *els)
{
    struct symbol *sym;
    struct tree *ret;

    if (OPKIND(cond->op) == CNST) {
        bool b;
        if (OPTYPE(cond->op) == P)
            b = cond->s.value.p;
        else
            b = cond->s.value.u;
        if (b)
            return explicit_cast(ty, then);
        else
            return explicit_cast(ty, els);
    }
    
    if (!isvoid(ty)) {
        sym = mktmp(gen_tmpname(), ty, REGISTER);
        then = assign(sym, then);
        els = assign(sym, els);
    } else {
        sym = NULL;
    }

    ret = ast_expr(COND, ty, cond, ast_expr(RIGHT, ty, then, els));
    ret->s.sym = sym;
    return ret;
}

static struct tree *member(struct tree *addr, const char *name,
                           struct source src)
{
    struct field *field;
    struct type *sty, *fty, *pfty;

    sty = rtype(addr->type);
    field = find_field(sty, name);
    if (!field) {
        field_not_found_error(src, sty, name);
        return NULL;
    }

    fty = direct(field)->type;
    if (opts.ansi) {
        // The result has the union of both sets of qualifiers.
        int q = qual_union(addr->type, fty);
        fty = qual(q, fty);
    }
    if (isarray(fty))
        pfty = fty;
    else
        pfty = ptr_type(fty);

    addr = fold(ADD+P, pfty,
                addr,
                cnsti(field->offset, unsignedptrtype));

    if (direct(field)->isbit) {
        // bit field
        addr = ast_expr(BFIELD, fty, rvalue(addr), NULL);
        addr->s.u.field = field;
    } else if (!isarray(fty)) {
        addr = rvalue(addr);
    }

    return addr;
}

// '*', '/'
static struct tree *bop_arith(int op, struct tree *l, struct tree *r,
                              struct source src)
{
    struct type *ty;

    if (!isarith(l->type)) {
        error_at(src, ERR_TYPE, "arith", l->type);
        return NULL;
    }
    if (!isarith(r->type)) {
        error_at(src, ERR_TYPE, "arith", r->type);
        return NULL;
    }

    ty = conv2(l->type, r->type);

    return fold(mkop(op, ty), ty, cast(ty, l), cast(ty, r));
}

// '%', '&', '^', '|', 'LSHIFT', 'RHIFT'
static struct tree *bop_int(int op, struct tree *l, struct tree *r,
                            struct source src)
{
    struct type *ty;
    
    if (!isint(l->type)) {
        error_at(src, ERR_TYPE, "integer", l->type);
        return NULL;
    }
    if (!isint(r->type)) {
        error_at(src, ERR_TYPE, "integer", r->type);
        return NULL;
    }

    ty = conv2(l->type, r->type);
    
    return fold(mkop(op, ty), ty, cast(ty, l), cast(ty, r));
}

// '+'
static struct tree *bop_add(int op, struct tree *l, struct tree *r,
                            struct source src)
{
    struct type *ty1 = l->type;
    struct type *ty2 = r->type;
    
    if (isarith(ty1) && isarith(ty2)) {
        struct type *ty = conv2(ty1, ty2);
        return fold(mkop(op, ty), ty, cast(ty, l), cast(ty, r));
    } else if (isptr(ty1) && isint(ty2)) {
        size_t size;
            
        if (!addable_ptr(l, src))
            return NULL;

        size = TYPE_SIZE(rtype(ty1));
        if (size > 1)
            r = actions.bop('*', r, cnsti(size, unsignedptrtype), src);

        return fold(mkop(op, ty1), ty1, l, cast(unsignedptrtype, r));
    } else if (isint(ty1) && isptr(ty2)) {
        size_t size;
            
        if (!addable_ptr(r, src))
            return NULL;

        size = TYPE_SIZE(rtype(ty2));
        if (size > 1)
            l = actions.bop('*', l, cnsti(size, unsignedptrtype), src);

        return fold(mkop(op, ty2), ty2, cast(unsignedptrtype, l), r);
    } else {
        error_at(src, ERR_BOP_OPERANDS, ty1, ty2);
        return NULL;
    }
}

// '-'
static struct tree *bop_sub(int op, struct tree *l, struct tree *r,
                            struct source src)
{
    struct type *ty1 = l->type;
    struct type *ty2 = r->type;

    if (isarith(ty1) && isarith(ty2)) {
        struct type *ty = conv2(ty1, ty2);
        return fold(mkop(op, ty), ty, cast(ty, l), cast(ty, r));
    } else if (isptr(ty1) && isint(ty2)) {
        size_t size;
            
        if (!addable_ptr(l, src))
            return NULL;

        size = TYPE_SIZE(rtype(ty1));
        if (size > 1)
            r = actions.bop('*', r, cnsti(size, unsignedptrtype), src);

        return fold(mkop(op, ty1), ty1, l, cast(unsignedptrtype, r));
    } else if (isptr(ty1) && isptr(ty2)) {        
        if (!addable_ptr(l, src) || !addable_ptr(r, src))
            return NULL;

        if (!compatible(rtype(ty1), rtype(ty2))) {
            error_at(src, ERR_PTR_TO_INCOMPATIBLE_TYPES, ty1, ty2);
            return NULL;
        }

        return fold(mkop(op, ty1), inttype, l, r);
    } else {
        error_at(src, ERR_BOP_OPERANDS, ty1, ty2);
        return NULL;
    }
}

// '>', '<', '>=', '<='
static struct tree *bop_rel(int op, struct tree *l, struct tree *r,
                            struct source src)
{
    struct type *ty, *ty1, *ty2;

    ty1 = l->type;
    ty2 = r->type;
    
    if (isarith(ty1) && isarith(ty2)) {
        // both arith
        ty = conv2(ty1, ty2);
    } else if (isptr(ty1) && isptr(ty2)) {
        // both ptr
        if (!compatible(rtype(ty1), rtype(ty2))) {
            error_at(src, ERR_PTR_TO_INCOMPATIBLE_TYPES, ty1, ty2);
            return NULL;
        }

        ty = unsignedptrtype;
    } else if (isptr(ty1) && isint(ty2)) {
        // ptr op int
        if (opts.ansi) {
            error_at(src, ERR_COMPARISION_PI,
                     TYPE_NAME(ty1), TYPE_NAME(ty2));
            return NULL;
        } else if (opts.Wall) {
            warning_at(src, ERR_COMPARISION_PI,
                       TYPE_NAME(ty1), TYPE_NAME(ty2));
        }

        ty = conv2(unsignedptrtype, ty2);
    } else if (isint(ty1) && isptr(ty2)) {
        // int op ptr
        if (opts.ansi) {
            error_at(src, ERR_COMPARISION_PI,
                     TYPE_NAME(ty1), TYPE_NAME(ty2));
            return NULL;
        } else if (opts.Wall) {
            warning_at(src, ERR_COMPARISION_PI,
                       TYPE_NAME(ty1), TYPE_NAME(ty2));
        }

        ty = conv2(ty1, unsignedptrtype);
    } else {
        error_at(src, ERR_COMPARISION_INCOMPATIBLE, ty1, ty2);
        return NULL;
    }

    return fold(mkop(op, ty), inttype, cast(ty, l), cast(ty, r));
}

// 'EQL', 'NEQ'
static struct tree *bop_eq(int op, struct tree *l, struct tree *r,
                           struct source src)
{
    struct type *ty, *ty1, *ty2;

    ty1 = l->type;
    ty2 = r->type;
    
    if (isarith(ty1) && isarith(ty2)) {
        // both arith
        ty = conv2(ty1, ty2);
    } else if (isptr(ty1) && isnullptr(r)) {
        // ptr NULL
        ty = unsignedptrtype;
    } else if (isnullptr(l) && isptr(ty2)) {
        // NULL ptr
        ty = unsignedptrtype;
    } else if (isptr(ty1) && isptrto(ty2, VOID)) {
        // ptr (void *)
        ty = unsignedptrtype;
    } else if (isptrto(ty1, VOID) && isptr(ty2)) {
        // (void *) ptr
        ty = unsignedptrtype;
    }  else if (isptr(ty1) && isptr(ty2)) {
        // both ptr
        if (!compatible(rtype(ty1), rtype(ty2))) {
            error_at(src, ERR_COMPARISION_INCOMPATIBLE, ty1, ty2);
            return NULL;
        }
        
        ty = unsignedptrtype;
    } else {
        error_at(src, ERR_COMPARISION_INCOMPATIBLE, ty1, ty2);
        return NULL;
    }

    return fold(mkop(op, ty), inttype, cast(ty, l), cast(ty, r));
}

static struct tree *bop_logical(int op, struct tree *l, struct tree *r,
                                struct source src)
{
    if (!isscalar(l->type)) {
        error_at(src, ERR_TYPE, "scalar", l->type);
        return NULL;
    }
    if (!isscalar(r->type)) {
        error_at(src, ERR_TYPE, "scalar", r->type);
        return NULL;
    }

    return fold(op, inttype, l, r);
}

/// actions-expr

static struct tree *do_comma(struct tree *l, struct tree *r,
                             struct source src)
{
    if (!l || !r)
        return NULL;

    l = decay(l);
    r = decay(r);
    l = ltor(l);
    r = ltor(r);

    return ast_expr(RIGHT, r->type, l, r);
}

static struct tree *do_assign(int t, struct tree *l, struct tree *r,
                              struct source src)
{
    struct type *ty1, *ty2, *retty;
    
    if (!l || !r)
        return NULL;

    if (!assignable(l, src))
        return NULL;

    ty1 = l->type;
    ty2 = r->type;

    if (t != '=') {
        // compound assignment
        int t2;
        switch (t) {
        case MULEQ:    t2 = '*'; break;
        case DIVEQ:    t2 = '/'; break;
        case MODEQ:    t2 = '%'; break;
        case ADDEQ:    t2 = '+'; break;
        case MINUSEQ:  t2 = '-'; break;
        case LSHIFTEQ: t2 = LSHIFT; break;
        case RSHIFTEQ: t2 = RSHIFT; break;
        case BANDEQ:   t2 = '&'; break;
        case BOREQ:    t2 = '|'; break;
        case XOREQ:    t2 = '^'; break;
        default:       assert(0 && "illegal compound assignment operator");
        }
        return actions.assign('=', l, actions.bop(t2, l, r, src), src);
    }

    retty = unqual(l->type);
    r = assignconv(retty, r);
    if (!r) {
        error_at(src, ERR_INCOMPATIBLE_CONV, ty2, ty1);
        return NULL;
    }

    if (l->op == BFIELD) {
        int n = 8 * TYPE_SIZE(l->s.u.field->type) - l->s.u.field->bitsize;
        r = actions.bop(RSHIFT,
                        actions.bop(LSHIFT, r, cnsti(n, inttype), src),
                        cnsti(n, inttype),
                        src);
    } else {
        l = lvalue(l);
    }

    return ast_expr(mkop(ASGN, retty), retty, l, r);
}

static struct tree *do_cond(struct tree *cond, struct tree *then,
                            struct tree *els, struct source src)
{
    struct type *ty, *ty1, *ty2;

    if (!cond || !then || !els)
        return NULL;
    
    cond = conv(cond);
    then = conv(then);
    els = conv(els);

    if (!isscalar(cond->type)) {
        error_at(src, ERR_TYPE, "scalar", cond->type);
        return NULL;
    }

    ty1 = then->type;
    ty2 = els->type;

    if (isarith(ty1) && isarith(ty2)) {
        ty = conv2(ty1, ty2);
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
               (isunion(ty1) && isunion(ty2))) {
        if (!eqtype(ty1, ty2))
            goto err_incompatible;

        ty = ty1;
    } else if (isvoid(ty1) && isvoid(ty2)) {
        ty = voidtype;
    } else if (isnullptr(then) && isptr(ty2)) {
        ty = ptr_type(compose(rtype(ty2), rtype(ty1)));
    } else if (isnullptr(els) && isptr(ty1)) {
        ty = ptr_type(compose(rtype(ty1), rtype(ty2)));
    } else if (isptrto(ty1, VOID) && isptr(ty2)) {
        if (isptrto(ty2, FUNCTION))
            goto err_incompatible;
        ty = ptr_type(compose(rtype(ty1), rtype(ty2)));
    } else if (isptrto(ty2, VOID) && isptr(ty1)) {
        if (isptrto(ty1, FUNCTION))
            goto err_incompatible;
        ty = ptr_type(compose(rtype(ty2), rtype(ty1)));
    } else if (isptr(ty1) && isptr(ty2)) {
        struct type *rty1 = rtype(ty1);
        struct type *rty2 = rtype(ty2);

        if (!eqtype(unqual(rty1), unqual(rty2)))
            goto err_incompatible;

        ty = ptr_type(compose(rty1, rty2));
    } else if ((isptr(ty1) && isint(ty2)) && !opts.ansi) {
        ty = ty1;
    } else if ((isint(ty1) && isptr(ty2)) && !opts.ansi) {
        ty = ty2;
    } else {
        error_at(src, "type mismatch in conditional expression: "
                 "'%T' and '%T'", ty1, ty2);
        return NULL;
    }

    return condexpr(ty, cond, then, els);

 err_incompatible:
    error_at(src,
             "imcompatible types '%T' and '%T' in conditional expression",
             ty1, ty2);
    return NULL;
}

static struct tree *do_bop(int t, struct tree *l, struct tree *r,
                           struct source src)
{
    if (!l || !r)
        return NULL;
    
    l = conv(l);
    r = conv(r);

    switch (t) {
    case '*':    return bop_arith(MUL, l, r, src);
    case '/':    return bop_arith(DIV, l, r, src);
    case '%':    return bop_int(MOD, l, r, src);
    case LSHIFT: return bop_int(SHL, l, r, src);
    case RSHIFT: return bop_int(SHR, l, r, src);
    case '&':    return bop_int(BAND, l, r, src);
    case '^':    return bop_int(XOR, l, r, src);
    case '|':    return bop_int(BOR, l, r, src);
    case '+':    return bop_add(ADD, l, r, src);
    case '-':    return bop_sub(SUB, l, r, src);
    case '>':    return bop_rel(GT, l, r, src);
    case '<':    return bop_rel(LT, l, r, src);
    case LEQ:    return bop_rel(LE, l, r, src);
    case GEQ:    return bop_rel(GE, l, r, src);
    case EQL:    return bop_eq(EQ, l, r, src);
    case NEQ:    return bop_eq(NE, l, r, src);
    case ANDAND: return bop_logical(AND, l, r, src);
    case OROR:   return bop_logical(OR, l, r, src);
    default:     assert(0 && "unknown binary operator");
    }
}

/// cast

static struct tree *do_cast(struct type *ty, struct tree *expr,
                            struct source src)
{
    if (!expr)
        return NULL;

    expr = decay(expr);
    if (!castable(ty, expr->type)) {
        error_at(src, ERR_INCOMPATIBLE_CONV, expr->type, ty);
        return NULL;
    }

    return explicit_cast(ty, expr);
}

/// unary

// '++', '--'
static struct tree *do_pre_increment(int t, struct tree *expr,
                                     struct source src)
{
    if (!expr)
        return NULL;
    if (!increasable(expr, src))
        return NULL;

    return incr(t == INCR ? '+' : '-', expr, cnsti(1, inttype), src);
}

// '+', '-'
static struct tree *do_minus_plus(int t, struct tree *expr,
                                  struct source src)
{
    if (!expr)
        return NULL;

    expr = conv(expr);

    if (!isarith(expr->type)) {
        error_at(src, ERR_TYPE, "arith", expr->type);
        return NULL;
    }

    // result is _NOT_ an lvalue
    if (t == '+') {
        if (islvalue(expr))
            return ast_expr(RIGHT, expr->type, expr, NULL);
        else
            return expr;
    } else {
        return fold(mkop(NEG, expr->type), expr->type, expr, NULL);
    }
}

// '~'
static struct tree *do_bitwise_not(struct tree *expr, struct source src)
{
    if (!expr)
        return NULL;

    expr = conv(expr);
    
    if (!isint(expr->type)) {
        error_at(src, ERR_TYPE, "integer", expr->type);
        return NULL;
    }

    return fold(mkop(BNOT, expr->type), expr->type, expr, NULL);
}

// '!'
static struct tree *do_logical_not(struct tree *expr, struct source src)
{
    if (!expr)
        return NULL;

    expr = conv(expr);
    
    if (!isscalar(expr->type)) {
        error_at(src, ERR_TYPE, "scalar", expr->type);
        return NULL;
    }

    return condexpr(inttype, expr, cnsti(0, inttype), cnsti(1, inttype));
}

/**
 * The usual conversions are _NOT_ applied to the operand of the '&'
 * operator, and its result is never an lvalue.
 */
// '&'
static struct tree *do_address(struct tree *expr, struct source src)
{
    struct type *ty;

    if (!expr)
        return NULL;

    ty = expr->type;
    if (!isfunc(ty)) {
        if (!islvalue(expr)) {
            error_at(src, "lvalue required as unary '&' operand");
            return NULL;
        }
        if (expr->op == BFIELD) {
            error_at(src, "address of bitfield requested");
            return NULL;
        }
    }

    if (isfunc(ty) || isarray(ty))
        expr = rettype(ptr_type(ty), expr);
    else
        expr = lvalue(expr);

    if (isaddrop(expr->op) && expr->s.sym->sclass == REGISTER) {
        error_at(src, "address of register variable requested");
        return NULL;
    }

    return expr;
}

// '*'
static struct tree *do_indirection(struct tree *expr, struct source src)
{
    struct type *ty, *rty;
    
    if (!expr)
        return NULL;

    expr = conv(expr);
    ty = expr->type;

    if (!isptr(ty)) {
        error_at(src, ERR_TYPE, "pointer", ty);
        return NULL;
    }

    rty = rtype(ty);
    if (isfunc(rty) || isarray(rty))
        return rettype(rty, expr);
    else
        return rvalue(expr);
}

// 'sizeof'
static struct tree *do_sizeofop(struct type *ty, struct tree *n,
                                struct source src)
{
    ty = n ? n->type : ty;
    if (!ty)
        return NULL;

    if (isfunc(ty) || isvoid(ty)) {
        error_at(src, "'sizeof' to a '%T' type is invalid", ty);
        return NULL;
    } else if (isincomplete(ty)) {
        error_at(src, "'sizeof' to an incomplete type '%T' is invalid", ty);
        return NULL;
    } else if (n && rightkid(n)->op == BFIELD) {
        error_at(src, "'sizeof' to a bitfield is invalid");
        return NULL;
    }

    return cnsti(TYPE_SIZE(ty), unsignedlongtype);
}

/// postfix

// 'base[index]' == '*(base+index)'
static struct tree *do_subscript(struct tree *base, struct tree *index,
                                 struct source src)
{
    if (!base || !index)
        return NULL;
    
    base = conv(base);
    index = conv(index);

    if ((isptr(base->type) && isint(index->type)) ||
        (isint(base->type) && isptr(index->type))) {
        struct type *ptr;
        struct tree *expr;

        ptr = isptr(base->type) ? base->type : index->type;
        if (isptrto(ptr, FUNCTION)) {
            error_at(src,
                     "subscript of pointer to function type '%T'",
                     rtype(ptr));
            return NULL;
        }

        expr = actions.bop('+', base, index, src);
        return rvalue(expr);
    } else {
        if (!isptr(base->type) && !isptr(index->type))
            error_at(src, "subscripted value is not an array or pointer");
        else
            error_at(src, "array subscript is not an integer");

        return NULL;
    }
}

static struct tree *do_funcall(struct tree *expr, struct tree **args,
                               struct source src)
{
    struct tree *ret;
    struct type *fty, *rty;

    if (!expr)
        return NULL;
    
    expr = conv(expr);
    
    if (!isptrto(expr->type, FUNCTION)) {
        error_at(src, "expect type 'function', not '%T'", expr->type);
        return NULL;
    }

    fty = rtype(expr->type);
    rty = rtype(fty);

    // check incomplete return type
    if (isrecord(rty) && isincomplete(rty)) {
        error_at(src, "calling function with incomplete return type '%T'",
                 rty);
        return NULL;
    }

    if (!(args = argsconv(fty, args, src)))
        return NULL;

    if (isrecord(rty)) {
        struct symbol *sym;
        struct tree *ref, *call;

        sym = mktmp(gen_tmpname(), rty, 0);
        ref = mkref(sym);
        call = ast_expr(mkop(CALL, rty), rty, expr, addrof(ref));
        call->s.u.args = args;
        ret = ast_expr(RIGHT, rty, call, ref);
    } else {
        ret = ast_expr(mkop(CALL, rty), rty, expr, NULL);
        ret->s.u.args = args;
    }

    events(funcall)(fty, args);

    return ret;
}

// '.', '->'
static struct tree *do_direction(int t, const char *name,
                                 struct tree *expr, struct source src)
{
    struct type *ty;

    if (!expr || !name)
        return NULL;

    ty = expr->type;
    if (t == '.') {
        struct tree *addr, *ret;
        
        if (!isrecord(ty)) {
            error_at(src, "expect type 'struct/union', not '%T'", ty);
            return NULL;
        }

        addr = addrof(expr);
        ret = member(addr, name, src);
        if (!ret)
            return NULL;
        if (!islvalue(expr))
            ret = ast_expr(RIGHT, ret->type, ret, NULL);

        return ret;
    } else {
        if (!isptr(ty) || !isrecord(rtype(ty))) {
            error_at(src,
                     "pointer to struct/union type expected, "
                     "not type '%T'", ty);
            return NULL;
        }

        return member(expr, name, src);
    }
}

static struct tree *do_post_increment(int t, struct tree *expr,
                                      struct source src)
{
    if (!expr)
        return NULL;
    if (!increasable(expr, src))
        return NULL;

    return ast_expr(RIGHT, expr->type,
                    ast_expr(RIGHT, expr->type,
                             expr,
                             incr(t == INCR ? '+' : '-',
                                  expr, cnsti(1, inttype), src)),
                    expr);
}

static struct tree *do_compound_literal(struct type *ty,
                                        struct tree *inits,
                                        struct source src)
{
    struct symbol *sym;
    
    if (cscope < LOCAL)
        return inits;

    sym = mktmp(gen_compound_label(), ty, 0);
    return iassign(sym, inits);
}

/// primary

static struct tree *do_iconst(struct token *tok)
{
    return arith_literal(tok, integer_constant);
}

static struct tree *do_fconst(struct token *tok)
{
    return arith_literal(tok, float_constant);
}

static struct tree *do_sconst(struct token *tok)
{
    return string_literal(tok, string_constant);
}

static struct tree *do_id(struct token *tok)
{
    const char *id = TOK_ID_STR(tok);
    struct symbol *sym;

    sym = lookup(id, identifiers);
    if (sym) {
        if (isenum(sym->type) && sym->sclass == ENUM)
            // enum ids
            return cnsti(sym->u.c.value.i, rtype(sym->type));
        else
            return mkref(sym);
    } else if (next_token_is('(')) {
        // lookup in externals
        sym = lookup(id, externals);
        if (sym == NULL) {
            // implicit function declaration: int id();
            sym = implicit_func_decl(id);
            return mkref(sym);
        } else if (isfunc(sym->type) || isptrto(sym->type, FUNCTION)) {
            warning("use of out-of-scope declaration of '%s', "
                    "previous declaration is here: %S",
                    id, sym->src);
            return mkref(sym);
        } else {
            error("use of '%s' does not match "
                  "previous declaration at: %S",
                  id, sym->src);
            return NULL;
        }
    } else {
        error("use of undeclared identifier '%s'", id);
        return NULL;
    }
}

static struct tree *do_paren(struct tree *expr, struct source src)
{
    expr->s.paren = true;
    return expr;
}

/// constant-expression:
///   conditional-expression
///
static long do_intexpr(struct tree *cond, struct type *ty,
                       struct source src)
{
    if (!cond)
        return 0;
    if (!ty)
        ty = cond->type;
    if (!isint(cond->type) || !isint(ty)) {
        error_at(src, "expression is not an integer constant expression");
        return 0;
    }
    if (!isiliteral(cond)) {
        error_at(src, "expression is not a compile-time constant");
        return 0;
    }
    return cond->s.value.i;
}

// if/do/while/for
static struct tree *do_bool_expr(struct tree *expr, struct source src)
{
    if (!expr)
        return NULL;
    // warning for assignment expression
    if (OPKIND(expr->op) == ASGN && !expr->s.paren)
        warning_at(src, "using the result of an assignment "
                    "as a condition without parentheses");

    return decay(ltor(expr));
}

// switch
static struct tree *do_switch_expr(struct tree *expr, struct source src)
{
    struct symbol *sym;
    
    if (!expr)
        return NULL;
    
    expr = conv(expr);
    if (!isint(expr->type)) {
        error_at(src, "statement requires expression of integer type "
                 "('%T' invalid)", expr->type);
        return NULL;
    }
    // make a tmp var
    sym = mktmp(gen_tmpname(), expr->type, REGISTER);
    return iassign(sym, expr);
}

/*=================================================================*
 *                        Sema-Statement                           *
 *=================================================================*/

static void ensure_return(struct tree *expr, bool isnull, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)
    if (expr == NULL)
        return;

    if (isvoid(rtype(func.type))) {
        if (!isnull && !isvoid(expr->type))
            error_at(src, "void function should not return a value");
    } else {
        if (!isnull) {
            struct type *ty1 = expr->type;
            struct type *ty2 = rtype(func.type);
            if (!(expr = assignconv(ty2, expr)))
                error_at(src, "returning '%T' from function "
                         "with incompatible result type '%T'",
                         ty1, ty2);
        } else {
            error_at(src, "non-void function should return a value");
        }
    }
}

static void ensure_gotos(void)
{
    for (struct goinfo *p = func.gotos; p; p = p->link) {
        struct symbol *sym = lookup(p->id, func.labels);
        if (!sym || !sym->defined)
            error_at(p->src, "use of undeclared label '%s'", p->id);
    }
}

static void warning_unused_lvars(struct symbol *lvar)
{
    for (struct symbol *sym = lvar; sym; sym = sym->local) {
        if (sym->refs == 0 && !sym->predefine &&
            !sym->temporary && !isfunc(sym->type))
            warning_at(sym->src, "unused variable '%s'", sym->name);
    }
}

/// actions-stmt

static void do_branch(struct tree *expr, int tlab, int flab)
{
    assert(tlab == 0 || flab == 0);
    struct stmt *stmt = ast_stmt(CBR);
    stmt->u.cbr.expr = expr;
    stmt->u.cbr.tlab = tlab;
    stmt->u.cbr.flab = flab;
    add_to_list(stmt);
}

static void do_jump(int label)
{
    struct stmt *stmt = ast_stmt(JMP);
    stmt->u.label = label;
    add_to_list(stmt);
}

static void do_ret(struct tree *expr, bool isnull, struct source src)
{
    ensure_return(expr, isnull, src);
    struct stmt *stmt = ast_stmt(RET);
    stmt->u.expr = expr;
    add_to_list(stmt);
}

static void do_label(int label)
{
    struct stmt *stmt = ast_stmt(LABEL);
    stmt->u.label = label;
    add_to_list(stmt);
}

static void do_gen(struct tree *expr)
{
    assert(expr && "null expr");
    struct stmt *stmt = ast_stmt(GEN);
    stmt->u.expr = expr;
    add_to_list(stmt);
}

/*=================================================================*
 *                        Sema-Initialization                      *
 *=================================================================*/

static void init_string(struct type *ty, struct tree *init,
                        struct source src)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(init->type);
    if (len1 > 0) {
        if (len1 < len2 - 1)
            warning_at(src,
                       "initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
        TYPE_LEN(ty) = len2;
        set_typesize(ty);
    }
}

// dty - arith/pointer/struct/union/array
static struct tree *ensure_init_compound(struct symbol *sym,
                                         struct tree *init,
                                         struct source src)
{
    struct type *dty = sym->type;
    struct type *sty = init->type;
    
    // TODO: 
    return init;
}

// dty - arith/pointer/struct/union/array
static struct tree *ensure_init_assign(struct symbol *sym,
                                       struct tree *init,
                                       struct source src)
{
    struct type *dty = sym->type;
    struct type *sty = init->type;
    
    if (isarray(dty)) {
        if (isstring(dty) && issliteral(init)) {
            init_string(dty, init, src);
        } else {
            error_at(src, "array initializer must be an initializer list "
                     "or string literal");
            return NULL;
        }
    } else {
        init = assignconv(dty, init);
        if (init == NULL)
            error_at(src, "initializing '%T' with an expression of "
                     "incompatible type '%T'", dty, sty);
    }

    return init;
}

static struct tree *ensure_init(int level, int sclass, struct symbol *sym,
                                struct tree *init, struct source src)
{
    struct type *ty = sym->type;
    
    if (!(isscalar(ty) || isarray(ty) || isrecord(ty))) {
        error_at(src, "'%s' cannot have an initializer", TYPE_NAME(ty));
        return NULL;
    }

    if (istag(ty) && isincomplete(ty)) {
        error_at(src, "variable '%s' has incomplete type '%T'",
                 sym->name, ty);
        return NULL;
    }

    if (sclass == EXTERN) {
        if (level == GLOBAL) {
            warning_at(src, "'extern' variable has an initializer");
        } else {
            error_at(src, "'extern' variable cannot have an initializer");
            return NULL;
        }
    }

    if (OPKIND(init->op) == COMPOUND)
        return ensure_init_compound(sym, init, src);
    else
        return ensure_init_assign(sym, init, src);
}

static bool ensure_designator(struct desig *d)
{
    if (isincomplete(d->type)) {
        int id;
        if (d->id == DESIG_FIELD)
            id = STRUCT;
        else if (d->id == DESIG_INDEX)
            id = ARRAY;
        else
            id = TYPE_KIND(d->type);

        error_at(d->src,
                 "%s designator of incomplete type '%T'",
                 id2s(id), d->type);
        return false;
    }
    return true;
}

static void offset_init1(struct desig *desig, struct tree *expr,
                         struct init **ilist)
{
    struct init *p, *init;
    
    // check override
    for (; (p = *ilist); ilist = &p->link) {
        if (p->offset > desig->offset)
            break;
        if (p->offset == desig->offset) {
            if (desig->id == DESIG_FIELD &&
                desig->u.field->isbit) {
                // bitfield
                if (p->boff < desig->u.field->bitoff)
                    continue;
                else if (p->boff > desig->u.field->bitoff)
                    break;
                // fall through
            }

            // overlapped
            warning_at(desig->src, ERR_INIT_OVERRIDE);
            p = p->link;    // remove from the list
            break;
        }
    }

    // insert
    init = NEWS0(struct init, FUNC);
    init->type = desig->type;
    init->offset = desig->offset;
    init->body = expr;
    if (desig->id == DESIG_FIELD) {
        assert(!isindirect(desig->u.field));
        init->boff = desig->u.field->bitoff;
        init->bsize = desig->u.field->bitsize;
    }
    init->link = p;
    *ilist = init;
}

static void scalar_init(struct desig *desig, struct tree *expr,
                        struct init **ilist)
{
    // TODO: 
}

static void union_init(struct desig *desig, struct tree *expr,
                       struct init **ilist)
{
    // TODO: 
}

static void struct_init(struct desig *desig, struct tree *expr,
                        struct init **ilist)
{
    // TODO: 
}

static void string_init(struct desig *desig, struct tree *expr,
                        struct init **ilist)
{
    // TODO: override check
    offset_init1(desig, expr, ilist);
}

static void offset_init(struct desig *desig, struct tree *expr,
                        struct init **ilist)
{
    assert(!isarray(desig->type));

    struct type *ty = desig->type;

    if (isincomplete(ty) || isfunc(ty)) {
        error_at(desig->src, "'%s' cannot have an initializer",
                 TYPE_NAME(ty));
        return;
    }

    //possible: scalar/struct/union
    if (isscalar(ty))
        scalar_init(desig, expr, ilist);
    else if (isstruct(ty))
        struct_init(desig, expr, ilist);
    else if (isunion(ty))
        union_init(desig, expr, ilist);
    else
        CC_UNAVAILABLE();
}

static struct desig *next_designator1(struct desig *desig, bool initial)
{
    assert(desig);

    switch (desig->id) {
    case DESIG_FIELD:
        {
            struct desig *prev = desig->prev;

            assert(prev);
            assert(isrecord(prev->type));

            struct field *field = desig->u.field->link;
            // skip indirect field
            while (field && isindirect(field))
                field = field->link;
            if (field) {
                struct desig *d = new_desig_field(field, source);
                d->offset = prev->offset + field->offset;
                d->prev = copy_desig(prev);
                return check_designator(d);
            } else {
                return next_designator1(prev, false);
            }
        }
        break;

    case DESIG_INDEX:
        {
            struct desig *prev = desig->prev;

            assert(prev);
            assert(isarray(prev->type));

            size_t len = TYPE_LEN(prev->type);
            long idx = desig->u.index;
            if (len == 0 || idx < len - 1) {
                struct type *rty = desig->type;
                struct desig *d = new_desig_index(idx+1, source);
                d->type = rty;
                d->offset = desig->offset + TYPE_SIZE(rty);
                d->prev = copy_desig(prev);
                return check_designator(d);
            } else {
                return next_designator1(prev, false);
            }
        }
        break;

    case DESIG_NONE:
        assert(desig->prev == NULL);
        if (!initial) {
            error("excess elements in %s initializer",
                  TYPE_NAME(desig->type));
            return NULL;
        }
        if (isrecord(desig->type)) {
            struct field *first = TYPE_FIELDS(desig->type);
            if (first) {
                struct desig *d = new_desig_field(first, source);
                d->offset = desig->offset + first->offset;
                d->prev = copy_desig(desig);
                return d;
            } else if (isincomplete(desig->type)) {
                error("initialize incomplete type '%T'", desig->type);
                return NULL;
            } else {
                // empty record
                error("excess elements in %s initializer",
                      TYPE_NAME(desig->type));
                return NULL;
            }
        } else if (isarray(desig->type)) {
            struct type *rty = rtype(desig->type);
            struct desig *d = new_desig_index(0, source);
            d->type = rty;
            d->offset = desig->offset;
            d->prev = copy_desig(desig);
            return d;
        } else {
            return desig;
        }
        break;
    }

    CC_UNAVAILABLE();
}

/// actions-init

static void do_element_init(struct desig **pdesig, struct tree *expr,
                            struct init **pinit)
{
    struct desig *desig = *pdesig;
    if (!desig || !expr)
        return;
    
    if (isstruct(desig->type) || isunion(desig->type)) {
        struct field *first = TYPE_FIELDS(desig->type);
        if (first == NULL) {
            // empty record
            if (iszinit(expr))
                offset_init(desig, expr, pinit);
            else
                error_at(desig->src, ERR_INIT_EMPTY_RECORD);
        } else if (eqtype(unqual(desig->type), unqual(expr->type))) {
            offset_init(desig, expr, pinit);
        } else {
            // set to first field
            struct desig *d = new_desig_field(first, source);
            d->offset = desig->offset + first->offset;
            d->prev = desig;
            *pdesig = d;
            actions.element_init(pdesig, expr, pinit);
        }
    } else if (isarray(desig->type)) {
        if (isstring(desig->type) && issliteral(expr)) {
            // string
            string_init(desig, expr, pinit);
        } else {
            // set to first index
            struct type *rty = rtype(desig->type);
            struct desig *d = new_desig_index(0, source);
            d->type = rty;
            d->offset = desig->offset;
            d->prev = desig;
            *pdesig = d;
            actions.element_init(pdesig, expr, pinit);
        }
    } else {
        // scalar type
        if (desig->braces)
            warning_at(desig->src,
                       "too many braces around scalar initializer");

        offset_init(desig, expr, pinit);
    }
}

static struct desig *do_designator(struct desig *desig, struct desig **ds)
{
    assert(desig && ds);

    desig = copy_desig(desig);

    for (int i = 0; ds[i]; i++) {
        struct desig *d = ds[i];
        switch (d->id) {
        case DESIG_FIELD:
            {
                const char *name = d->u.name;
                assert(name);
                if (!isrecord(desig->type)) {
                    error_at(d->src,
                             "%s designator cannot initialize non-%s type '%T'",
                             id2s(STRUCT), id2s(STRUCT), desig->type);
                    return NULL;
                }
                struct field *field = find_field(desig->type, name);
                if (!field) {
                    field_not_found_error(d->src, desig->type, name);
                    return NULL;
                }
                // indirect
                if (isindirect(field)) {
                    for (int i = 0; field->of[i]; i++) {
                        struct field *p = field->of[i];
                        struct desig *d = new_desig_field(p, p->src);
                        d->offset = desig->offset + p->offset;
                        d->prev = desig;
                        desig = d;
                    }
                    field = direct(field);
                }
                d->offset = desig->offset + field->offset;
                d->type = field->type;
                d->u.field = field;
                d->prev = desig;
                desig = d;

                // check incomplete type
                if (!ensure_designator(d))
                    return NULL;
            }
            break;

        case DESIG_INDEX:
            {
                if (!isarray(desig->type)) {
                    error_at(d->src,
                             "%s designator cannot initialize non-%s type '%T'",
                             id2s(ARRAY), id2s(ARRAY), desig->type);
                    return NULL;
                }
                size_t len = TYPE_LEN(desig->type);
                if (len && d->u.index >= len) {
                    error_at(d->src,
                             "array designator index [%ld] "
                             "exceeds array bounds (%lu)",
                             d->u.index, len);
                    return NULL;
                }
                struct type *rty = rtype(desig->type);
                d->offset = desig->offset + d->u.index * TYPE_SIZE(rty);
                d->type = rty;
                d->prev = desig;
                desig = d;

                // check incomplete type
                if (!ensure_designator(d))
                    return NULL;
            }
            break;

        default:
            assert(0 && "unexpected designator id");
        }
    }

    return desig;
}

static struct tree *do_initializer_list(struct type *ty, struct init *ilist)
{
    struct tree *n = ast_expr(COMPOUND, ty, NULL, NULL);
    // TODO: incomplete array type
    // TODO: merge bitfields
    n->s.u.ilist = ilist;
    return n;
}

/*=================================================================*
 *                        Sema-Declaration                         *
 *=================================================================*/

/*
 * Check for:
 * - `array of function`
 * - `function returning array`
 * - `function returning function`
 * recursively. Above cases are always invalid.
 */

/**
 *  1. Array qualifiers may appear only when in a function parameter.
 *
 *  2. Array qualifiers 'const', 'volatile', 'restrict', 'static' may
 *     appear within the _outermost_ brackets.
 *
 *  3. 'static' is an optimization hint, asserting that the actual array
 *     argument will be non-null and will have the declared size and
 *     type upon entry to the function.
 *
 *  4. The star modifier '*' or non-constant expression describe a
 *     variable length array. The '*' can only appear in array parameter
 *     declarations within function prototypes that are not part of
 *     a function definition.
 */
static void ensure_func_array(struct type *ty, bool param, bool outermost,
                              struct source src)
{
    if (isarray(ty)) {
        struct type *rty = rtype(ty);

        if (isfunc(rty))
            error_at(src, ERR_ARRAY_OF_FUNC);

        if (TYPE_A_STAR(ty) && !param)
            error_at(src,
                     "star modifier used outside of function prototype");

        if (TYPE_A_CONST(ty) ||
            TYPE_A_RESTRICT(ty) ||
            TYPE_A_VOLATILE(ty) ||
            TYPE_A_STATIC(ty)) {
            if (!param)
                error_at(src, "type qualifier used in array declarator "
                         "outside of function prototype");

            if (!outermost)
                error_at(src, "type qualifier used "
                         "in non-outermost array type derivation");
        }

        ensure_func_array(rty, param, false, src);
        set_typesize(ty);       // calculate array size
    } else if (isfunc(ty)) {
        struct type *rty = rtype(ty);
        if (isarray(rty))
            error_at(src, ERR_FUNC_RET_ARRAY, rty);
        else if (isfunc(rty))
            error_at(src, ERR_FUNC_RET_FUNC, rty);

        ensure_func_array(rty, false, true, src);
    } else if (isptr(ty)) {
        struct type *rty = rtype(ty);
        ensure_func_array(rty, param, false, src);
    }
}

static void finish_type(struct type *ty, bool param, struct source src)
{
    ensure_func_array(ty, param, true, src);
    // array has incomplte element type is always illegal.
    if (isarray(ty)) {
        struct type *rty = rtype(ty);
        if (isincomplete(rty))
            error_at(src, ERR_INCOMPLETE_ELEMENT, rty);
    }
}

static void check_func_array_in_funcdef(struct type *ty, struct source src)
{
    if (isarray(ty)) {
        struct type *rty = rtype(ty);
        if (TYPE_A_STAR(ty))
            error_at(src, "variable length array "
                     "must be bound in function definition");
        check_func_array_in_funcdef(rty, src);
    } else if (isptr(ty)) {
        struct type *rty = rtype(ty);
        check_func_array_in_funcdef(rty, src);
    }
}

static void ensure_inline(struct type *ty, int fspec, struct source src)
{
    if (fspec == INLINE) {
        if (isfunc(ty))
            TYPE_INLINE(ty) = 1;
        else
            error_at(src, ERR_INLINE);
    }
}

static void ensure_bitfield(struct field *p)
{
    struct type *ty = p->type;
    int bitsize = p->bitsize;
    int bits = BITS(TYPE_SIZE(ty));

    if (!isint(ty)) {
        if (p->name)
            error_at(p->src,
                     "bit-field '%s' has non-integral type '%T'",
                     p->name, ty);
        else
            error_at(p->src,
                     "anonymous bit-field has non-integral type '%T'",
                     ty);

        p->type = inttype;
    }

    if (bitsize < 0) {
        if (p->name)
            error_at(p->src,
                     "bit-field '%s' has negative width '%d'",
                     p->name, bitsize);
        else
            error_at(p->src,
                     "anonymous bit-field has negative width '%d'",
                     bitsize);
    }

    if (bitsize == 0 && p->name)
        error_at(p->src,
                 "named bit-field '%s' has zero width",
                 p->name);

    if (bitsize > bits) {
        if (p->name)
            error_at(p->src, "size of bit-field '%s' (%d bits) "
                     "exceeds size of its type (%d bits)",
                     p->name, bitsize, bits);
        else
            error_at(p->src, "anonymous bit-field (%d bits) "
                     "exceeds size of its type (%d bits)",
                     bitsize, bits);
    }
}

static void ensure_field(struct field *p, bool one)
{
    struct type *ty = p->type;

    finish_type(ty, false,  p->src);

    if (isarray(ty)) {
        if (isincomplete(ty)) {
            if (one)
                error_at(p->src,
                         "flexible array cannot be the only member");
            else if (p->link)   // NOT the last field
                error_at(p->src,
                         "field has incomplete type '%T'", ty);
        }
    } else if (isfunc(ty)) {
        error_at(p->src, "field has invalid type '%s'", TYPE_NAME(ty));
        // fix to pointer
        p->type = ptr_type(ty);
    } else if (isincomplete(ty)) {
        error_at(p->src, "field has incomplete type '%T'", ty);
    }
}

static void ensure_fields(struct symbol *sym)
{
    struct field *first = sym->u.s.flist;
    bool one = first && first->link == NULL;

    for (struct field *p = first; p; p = p->link) {
        if (isindirect(p))
            continue;
        if (p->isbit)
            ensure_bitfield(p);
        else
            ensure_field(p, one);
    }
}

static void check_main_func(struct type *ftype, const char *name,
                            struct source src)
{
    assert(isfunc(ftype));
    assert(name);

    if (strcmp(name, "main"))
        return;

    struct type *rty = rtype(ftype);
    struct type **proto = TYPE_PROTO(ftype);
    size_t len = length(proto);

    if (rty != inttype && rty != voidtype)
        error_at(src, "return type of 'main' is not 'int'");

    for (int i = 0; i < MIN(3, len); i++) {
        struct type *ty = proto[i];
        if (i == 0) {
            if (ty != inttype)
                error_at(src, "first parameter of 'main' is not 'int'");
        } else if (i == 1 || i == 2) {
            if (!isptrto(ty, POINTER) ||
                !isptrto(rtype(ty), CHAR))
                error_at(src, "%s parameter of 'main' is not 'char **'",
                         i == 1 ? "second" : "third");
        }
    }
    if (len == 1 || len > 3)
        error_at(src,
                 "expect 0, 2 or 3 parameters for 'main', have %d",
                 len);
}

static void check_params_in_funcdef(struct symbol *params[])
{
    for (int i = 0; params[i]; i++) {
        struct symbol *sym = params[i];
        struct type *ty = sym->type;
        // parameter name is required in prototype
        if (sym->anonymous)
            error_at(sym->src, "parameter name omitted");
        // get the original type without decay
        if (isptr(ty) && TYPE_P_DECAY(ty))
            ty = TYPE_P_DECAY(ty);

        // check variable length array (star modifier)
        check_func_array_in_funcdef(ty, sym->src);

        // check incomplete type
        if (isenum(ty) || isstruct(ty) || isunion(ty)) {
            if (!TYPE_TSYM(ty)->defined)
                error_at(sym->src,
                         "variable has incomplete type '%T'",
                         ty);
        }
    }
}

static void oldparam(struct symbol *sym, void *context)
{
    struct symbol **params = context;

    assert(sym->name);

    // _NOT_ a variable
    if (sym->sclass == TYPEDEF || isfunc(sym->type)) {
        warning_at(sym->src, "empty declaraion");
        return;
    }

    for (int j = 0; params[j]; j++) {
        struct symbol *s = params[j];
        assert(s->name);
        if (s->name == sym->name) {
            // replace id with declared symbol
            params[j] = sym;
            return;
        }
    }

    // _NOT_ found in id list
    error_at(sym->src, "parameter named '%s' is missing", sym->name);
}

static void mkfuncdecl(struct symbol *sym, struct type *ty, int sclass,
                       struct source src)
{
    sym->type = ty;
    sym->src = src;
    sym->defined = true;
    sym->sclass = sclass;
}

static void predefined_ids(void)
{
    /**
     * Predefined identifier: __func__
     * The identifier __func__ is implicitly declared by C99
     * implementations as if the following declaration appeared
     * after the opening brace of each function definition:
     *
     * static const char __func__[] = "function-name";
     *
     */
    struct type *type = array_type(qual(CONST, chartype));
    // initializer
    struct tree *literal = cnsts(func.name);
    init_string(type, literal, source);

    struct symbol *sym = mklocal("__func__", type, STATIC);
    sym->predefine = true;
    sym->u.init = literal;
}

static void func_body(struct symbol *sym)
{
    struct stmt *stmt = NULL;
    struct symbol *lvars = NULL;

    func.gotos = NULL;
    func.labels = new_table(NULL, LOCAL);
    func.type = sym->type;
    func.name = sym->name;
    func.xcall = NULL;
    func.stmt = &stmt;
    func.lvars = &lvars;

    // compound statement
    compound_stmt(predefined_ids, 0, 0, NULL);
    // check goto labels
    ensure_gotos();
    // warning unused vars
    warning_unused_lvars(lvars);

    // save
    sym->u.f.xcall = func.xcall;
    sym->u.f.stmt = stmt;
    sym->u.f.lvars = lvars;

    free_table(func.labels);
    memset(&func, 0, sizeof(struct func));
}

static void doglobal(struct symbol *sym, void *context)
{
    // typedefs and enum ids are _defined_
    if (sym->defined || sym->sclass == EXTERN || isfunc(sym->type))
        return;

    events(defgvar)(sym);
}

static void warning_unused_global(struct symbol *sym, void *context)
{
    if (sym->sclass != STATIC || sym->refs)
        return;

    // only `STATIC' and 'non-refed' symbol is counted.
    if (isfunc(sym->type)) {
        if (sym->anonymous)
            warning_at(sym->src, "unused function");
        else
            warning_at(sym->src, "unused function '%s'", sym->name);
    } else {
        if (sym->anonymous)
            warning_at(sym->src, "unused variable");
        else
            warning_at(sym->src, "unused variable '%s'", sym->name);
    }
}

/// actions-decl

static void do_array_index(struct type *atype, struct tree *assign,
                           struct source src)
{
    if (!assign)
        return;

    if (isint(assign->type)) {
        TYPE_A_ASSIGN(atype) = assign;
        // try evaluate the length
        if (isiliteral(assign)) {
            TYPE_LEN(atype) = assign->s.value.i;
            if (assign->s.value.i < 0)
                error_at(src, "array has negative size");
        } else {
            error_at(src, "expect constant expression");
        }
    } else {
        error_at(src, "size of array has non-integer type '%T'",
                 assign->type);
    }
}

static struct symbol **do_prototype(struct type *ftype,
                                    struct symbol *params[])
{
    for (int i = 0; params[i]; i++) {
        struct symbol *p = params[i];
        struct type *ty = p->type;
        if (isvoid(ty)) {
            if (i == 0) {
                if (!p->anonymous) {
                    error_at(p->src,
                             "argument may not have 'void' type");
                    p->type = inttype;
                } else if (isqual(ty)) {
                    error_at(p->src, "'void' as parameter must not "
                             "have type qualifier");
                    p->type = inttype;
                } else if (TYPE_VARG(ftype)) {
                    error_at(p->src, "'void' must be the "
                             "first and only parameter if specified");
                    p->type = inttype;
                }
            } else {
                error_at(p->src, "'void' must be the "
                         "first and only parameter if specified");
                p->type = inttype;
            }
        }
    }

    // make it empty
    if (length(params) == 1 && isvoid(params[0]->type))
        params[0] = NULL;

    return params;
}

static struct symbol *do_enum_id(const char *name, int val,
                                 struct symbol *sym, struct source src)
{
    struct symbol *s = lookup(name, identifiers);
    if (s && is_current_scope(s))
        error_at(src, ERR_REDEFINITION, name, s->src);

    s = install(name, &identifiers, cscope, cscope < LOCAL ? PERM : FUNC);
    s->type = sym->type;
    s->src = src;
    s->sclass = ENUM;
    s->u.c.value.i = val;
    s->defined = true;
    return s;
}

static void do_direct_field(struct symbol *sym, struct field *field)
{
    struct field **pp = &sym->u.s.flist;
    struct field *p;

    while ((p = *pp)) {
        if (field->name && field->name == p->name)
            error_at(field->src, ERR_DUPLICATE_MEMBER,
                     field->name, p->src);
        pp = &p->link;
    }

    *pp = field;
}

static void do_indirect_field(struct symbol *sym, struct field *field)
{
    struct field *first = TYPE_FIELDS(field->type);
    struct field **pp;
    struct field *indir = NULL;
    struct field **indirp = &indir;

    actions.direct_field(sym, field);

    for (struct field *q = first; q; q = q->link) {
        struct field *p;

        pp = &sym->u.s.flist;
        while ((p = *pp)) {
            if (q->name && q->name == p->name)
                error_at(q->src, ERR_DUPLICATE_MEMBER,
                         q->name, p->src);
            pp = &p->link;
        }
        if (isindirect(q)) {
            struct field *n = new_indirect_field(q->indir);
            struct list *list = list_append(NULL, field);
            for (int i = 0; q->of[i]; i++)
                list = list_append(list, q->of[i]);
            n->of = ltoa(&list, PERM);
            n->offset = q->offset;
            *indirp = n;
            indirp = &n->link;
        } else if (q->name) {
            struct field *n = new_indirect_field(q);
            struct list *list = list_append(NULL, field);
            n->of = ltoa(&list, PERM);
            n->offset = q->offset;
            *indirp = n;
            indirp = &n->link;
        }
    }

    if (indir)
        *pp = indir;
}

static void do_enumdecl(struct symbol *sym, struct symbol *ids[])
{
    sym->defined = true;
    sym->u.s.ids = ids;
    events(deftype)(sym);
}

static void do_recorddecl(struct symbol *sym)
{
    ensure_fields(sym);
    sym->defined = true;
    set_typesize(sym->type);
    events(deftype)(sym);
}

static void do_tagdecl(struct type *ty, int sclass, int fspec,
                       struct source src)
{
    if (isstruct(ty) || isunion(ty)) {
        // anonymous record (can't be referenced)
        if (TYPE_TSYM(ty)->anonymous)
            warning_at(src, "declaration does not declare anything");
    }
    if (sclass)
        warning_at(src, "'%s' ignored on this declaration", id2s(sclass));
    if (isconst(ty))
        warning_at(src, "'%s' ignored on this declaration", id2s(CONST));
    if (isvolatile(ty))
        warning_at(src, "'%s' ignored on this declaration", id2s(VOLATILE));
    if (isrestrict(ty))
        warning_at(src, "'%s' ignored on this declaration", id2s(RESTRICT));
    if (fspec == INLINE)
        error_at(src, ERR_INLINE);
}

static struct symbol *do_globaldecl(const char *id, struct type *ty,
                                    int sclass, int fspec,
                                    struct tree *init, struct source src)
{
    struct symbol *sym;

    assert(id);
    assert(cscope == GLOBAL);

    if (sclass == AUTO || sclass == REGISTER) {
        error_at(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }

    finish_type(ty, false, src);

    if (isfunc(ty))
        check_main_func(ty, id, src);

    ensure_inline(ty, fspec, src);

    sym = lookup(id, identifiers);
    if (!sym || sym->scope != GLOBAL) {
        sym = install(id, &identifiers, GLOBAL, PERM);
        sym->type = ty;
        sym->src = src;
        sym->sclass = sclass;
    } else if (eqtype(ty, sym->type)) {
        if (sclass == STATIC && sym->sclass != STATIC)
            error_at(src, "static declaration of '%s' "
                     "follows non-static declaration", id);
        else if (sym->sclass == STATIC && sclass != STATIC)
            error_at(src, "non-static declaration of '%s' "
                     "follows static declaration", id);

        if (sclass != EXTERN)
            sym->sclass = sclass;
    } else {
        error_at(src, ERR_CONFLICTING_TYPES, sym->name, sym->src);
    }

    if (init) {
        init = ensure_init(GLOBAL, sclass, sym, init, src);
        if (sym->defined)
            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
        sym->u.init = init;
        sym->defined = true;
    }

    // check incomplete type after intialized
    if (isincomplete(ty) && sym->defined)
        error_at(src, ERR_INCOMPLETE_VAR, id, ty);

    // actions
    if (sym->u.init)
        events(defgvar)(sym);
    else if (isfunc(ty))
        events(dclfun)(sym);
    else
        events(dclgvar)(sym);

    return sym;
}

static struct symbol *do_localdecl(const char *id, struct type *ty,
                                   int sclass, int fspec,
                                   struct tree *init, struct source src)
{
    struct symbol *sym;

    assert(id);
    assert(cscope >= LOCAL);

    if (sclass == 0)
        sclass = isfunc(ty) ? EXTERN : AUTO;

    finish_type(ty, false, src);

    if (isfunc(ty)) {
        check_main_func(ty, id, src);
        if (sclass != EXTERN) {
            error_at(src, "function declared in block scope "
                     "cannot have '%s' storage class",
                     id2s(sclass));
            sclass = EXTERN;
        }
    }

    ensure_inline(ty, fspec, src);

    sym = lookup(id, identifiers);
    if (sclass == EXTERN) {
        if (!sym || !is_current_scope(sym) || eqtype(ty, sym->type)) {
            struct symbol *p = lookup(id, globals);
            if (p == NULL || eqtype(ty, p->type)) {
                p = lookup(id, externals);
                if (p && !eqtype(ty, p->type))
                    error_at(src, ERR_REDEFINITION, p->name, p->src);
            } else {
                error_at(src, ERR_REDEFINITION, p->name, p->src);
            }
        } else {
            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
        }
    } else {
        if (sym && is_current_scope(sym))
            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
    }

    sym = install(id, &identifiers, cscope, sclass == EXTERN ? PERM : FUNC);
    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;
    if (sclass != EXTERN)
        sym->defined = true;

    if (sclass == EXTERN) {
        struct symbol *p = install(id, &externals, GLOBAL, PERM);
        p->type = ty;
        p->src = src;
        p->sclass = EXTERN;
    }

    if (init) {
        init = ensure_init(LOCAL, sclass, sym, init, src);
        // gen assign expr
        if (init && sclass != STATIC)
            iassign(sym, init);
    }

    // check incomplete type after initialized
    if (isincomplete(ty) && sym->defined)
        error_at(src, ERR_INCOMPLETE_VAR, id, ty);

    // actions
    if (isfunc(ty))
        events(dclfun)(sym);
    else if (sclass == EXTERN)
        events(dclgvar)(sym);
    else if (sclass == STATIC)
        events(defsvar)(sym);
    else
        events(deflvar)(sym);

    return sym;
}

// id maybe NULL
static struct symbol *do_paramdecl(const char *id, struct type *ty,
                                   int sclass, int fspec,
                                   struct tree *init, struct source src)
{
    struct symbol *sym;
    bool nonnull = false;

    if (sclass && sclass != REGISTER) {
        error_at(src, "invalid storage class specifier '%s' "
                 "in function declarator", id2s(sclass));
        sclass = 0;
    }

    if (fspec == INLINE)
        error_at(src, ERR_INLINE);

    finish_type(ty, true, src);

    if (isfunc(ty)) {
        struct type *fty = ty;
        ty = ptr_type(fty);
        ty->u.p.decay = fty;
    } else if (isarray(ty)) {
        struct type *aty = ty;

        ty = ptr_type(rtype(ty));
        ty->u.p.decay = aty;
        // apply array qualifiers
        if (TYPE_A_CONST(aty))
            ty = qual(CONST, ty);
        if (TYPE_A_VOLATILE(aty))
            ty = qual(RESTRICT, ty);
        if (TYPE_A_RESTRICT(aty))
            ty = qual(VOLATILE, ty);
        if (TYPE_A_STATIC(aty))
            nonnull = true;
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!TYPE_TSYM(ty)->defined || TYPE_TSYM(ty)->scope == cscope)
            warning_at(src, "declaration of '%T' "
                       "will not be visible outside of this function",
                       ty);
    }

    if (id) {
        sym = lookup(id, identifiers);
        if (sym && sym->scope == cscope)
            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
        sym = install(id, &identifiers, cscope, FUNC);
    } else {
        sym = anonymous(&identifiers, cscope, FUNC);
    }

    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;
    sym->nonnull = nonnull;
    sym->defined = true;

    return sym;
}

// level: GLOBAL/PARAM/LOCAL
static void do_typedefdecl(const char *id, struct type *ty,
                           int fspec, int level, struct source src)
{
    struct symbol *sym;

    assert(id);

    if (level == PARAM)
        error_at(src, "invalid storage class specifier '%s' "
                 "in function declarator", id2s(TYPEDEF));

    if (fspec == INLINE)
        error_at(src, ERR_INLINE);

    finish_type(ty, level == PARAM, src);

    sym = lookup(id, identifiers);
    if (sym && is_current_scope(sym))
        error_at(src, ERR_REDEFINITION, sym->name, sym->src);

    sym = install(id, &identifiers, cscope, cscope < LOCAL ? PERM : FUNC);
    sym->type = ty;
    sym->src = src;
    sym->sclass = TYPEDEF;
    sym->defined = true;

    events(deftype)(sym);
}

// id maybe NULL
void funcdef(const char *id, struct type *ty, int sclass, int fspec,
             struct symbol *params[], struct source src)
{
    struct symbol *sym;

    assert(cscope == PARAM);

    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }

    finish_type(ty, false, src);

    if (id) {
        sym = lookup(id, identifiers);
        if (!sym || sym->scope != GLOBAL) {
            sym = install(id, &identifiers, GLOBAL, PERM);
            mkfuncdecl(sym, ty, sclass, src);
        } else if (eqtype(ty, sym->type) && !sym->defined) {
            if (sclass == STATIC && sym->sclass != STATIC)
                error_at(src, "static declaaration of '%s' "
                         "follows non-static declaration", id);
            else
                mkfuncdecl(sym, ty, sclass, src);
        } else {
            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
        }

        check_main_func(ty, id, src);
    } else {
        sym = anonymous(&identifiers, GLOBAL, PERM);
        mkfuncdecl(sym, ty, sclass, src);
    }

    if (fspec == INLINE)
        TYPE_INLINE(ty) = INLINE;

    // old style function parameters declaration
    if (TYPE_OLDSTYLE(ty)) {
        int i;
        struct type **proto;

        foreach(identifiers, PARAM, oldparam, params);

        for (i = 0; params[i]; i++) {
            struct symbol *p = params[i];
            if (!p->defined)
                params[i] = actions.paramdecl(p->name, inttype,
                                              0, 0, NULL, p->src);
            // check void
            if (isvoid(p->type)) {
                error_at(p->src, "argument may not have 'void' type");
                p->type = inttype;
            }
        }

        proto = newarray(sizeof(struct type *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = params[i]->type;

        proto[i] = NULL;
        TYPE_PROTO(ty) = proto;
    }

    TYPE_PARAMS(ty) = params;
    check_params_in_funcdef(params);

    if (token_is('{')) {
        // function definition
        func_body(sym);
        exit_scope();
        events(defun)(sym);
    } else {
        // oldstyle
        assert(TYPE_OLDSTYLE(ty));
        error("expect function body after function declarator");
    }
}

/*=================================================================*
 *                          Public                                 *
 *=================================================================*/

int first_decl(struct token *t)
{
    return t->kind == STATIC || first_typename(t);
}

int first_stmt(struct token *t)
{
    return t->kind == IF || first_expr(t);
}

int first_expr(struct token *t)
{
    return t->kind == ID;
}

int first_typename(struct token * t)
{
    return t->kind == INT || t->kind == CONST ||
        (t->id == ID && istypedef(TOK_ID_STR(t)));
}

void skip_to_brace(void)
{
    skip_balance('{', '}', "brace");
}

void skip_to_bracket(void)
{
    skip_balance('(', ')', "bracket");
}

void skip_to_squarebracket(void)
{
    skip_balance('[', ']', "square bracket");
}

void skip_to_decl(void)
{
    skip_to_first(first_decl);
}

void skip_to_stmt(void)
{
    skip_to_first(first_stmt);
}

void skip_to_expr(void)
{
    skip_to_first(first_expr);
}

struct symbol *tag_symbol(int t, const char *tag, struct source src)
{
    struct type *ty = tag_type(t);
    struct symbol *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && is_current_scope(sym)) {
            if (TYPE_OP(sym->type) == t && !sym->defined)
                return sym;

            error_at(src, ERR_REDEFINITION, sym->name, sym->src);
        }

        sym = install(tag, &tags, cscope, PERM);
    } else {
        sym = anonymous(&tags, cscope, PERM);
    }

    sym->type = ty;
    sym->src = src;
    ty->u.s.tsym = sym;

    return sym;
}

struct desig *next_designator(struct desig *desig)
{
    if (!desig)
        return NULL;

    return next_designator1(desig, true);
}

struct tree *cnsti(long i, struct type *ty)
{
    return mkiliteral(ty, i);
}

struct tree *cnsts(const char *string)
{
    struct token t = {
        .id = SCONSTANT,
        .u.lit.str = format("\"%s\"", string)
    };
    return string_literal(&t, string_constant);
}

void check_case_duplicates(struct cse *cse, struct swtch *swtch)
{
    assert(cse && swtch);

    for (struct cse *c = swtch->cases; c; c = c->link) {
        if (c->value == cse->value) {
            error_at(cse->src, "duplicate case value '%lld', "
                     "previous case defined here: %S",
                     cse->value, c->src);
            break;
        }
    }
}

void mark_goto(const char *id, struct source src)
{
    struct goinfo *p = NEW(sizeof(struct goinfo), FUNC);
    p->id = id;
    p->src = src;
    p->link = func.gotos;
    func.gotos = p;
}

/// init/finalize

static void init(int argc, char *argv[])
{
    IR->init(argc, argv);
}

static void finalize(void)
{
    foreach(identifiers, GLOBAL, warning_unused_global, NULL);
    if (opts.ast_dump || errors())
        return;
    foreach(identifiers, GLOBAL, doglobal, NULL);
    IR->finalize();
}

struct actions actions = {
    .init = init,
    .finalize = finalize,

    // decl
    INIT(enumdecl),
    INIT(recorddecl),
    INIT(tagdecl),
    INIT(globaldecl),
    INIT(localdecl),
    INIT(paramdecl),
    INIT(typedefdecl),

    INIT(array_index),
    INIT(prototype),
    INIT(enum_id),
    INIT(direct_field),
    INIT(indirect_field),

    // expr
    INIT(comma),
    INIT(assign),
    INIT(cond),
    INIT(bop),
    INIT(cast),
    INIT(pre_increment),
    INIT(minus_plus),
    INIT(bitwise_not),
    INIT(logical_not),
    INIT(address),
    INIT(indirection),
    INIT(sizeofop),
    INIT(subscript),
    INIT(funcall),
    INIT(direction),
    INIT(post_increment),
    INIT(id),
    INIT(iconst),
    INIT(fconst),
    INIT(sconst),
    INIT(paren),
    INIT(compound_literal),

    INIT(intexpr),
    INIT(bool_expr),
    INIT(switch_expr),

    // stmt
    INIT(branch),
    INIT(jump),
    INIT(ret),
    INIT(label),
    INIT(gen),

    // init
    INIT(element_init),
    INIT(designator),
    INIT(initializer_list),
};
