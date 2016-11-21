#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include "cc.h"
#include "color.h"

#define kVarDecl            "VarDecl"
#define kTypedefDecl        "TypedefDecl"
#define kFuncDecl           "FuncDecl"
#define kStructDecl         "StructDecl"
#define kUnionDecl          "UnionDecl"
#define kEnumDecl           "EnumDecl"
#define kFieldDecl          "FieldDecl"
#define kIndirectFieldDecl  "IndirectFieldDecl"
#define kField              "Field"
#define kEnumConstDecl      "EnumConstantDecl"

static const char *opkindstr[] = {
    "null",
    /// comma
    "RIGHT",
    /// cond
    "COND",
    /// constant
    "CNST",
    /// address
    "ADDRG",
    "ADDRP",
    "ADDRL",
    /// indirection
    "INDIR",
    /// binary
    "ASGN",
    "MUL",
    "DIV",
    "ADD",
    "SUB",
    "MOD",
    "SHL",
    "SHR",
    "BAND",
    "BOR",
    "XOR",
    "EQ",
    "NE",
    "GT",
    "GE",
    "LT",
    "LE",
    "AND",
    "OR",
    /// unary
    "NEG",
    "BNOT",
    /// postfix
    "INITS",
    "CALL",
    "BFIELD",
    /// conversion
    "CVI",
    "CVU",
    "CVF",
    "CVP",
};

static const char *optypestr[] = {
    "", "I", "U", "F", "P", "S"
};

static void print_symbol1(FILE *fp, struct symbol *sym,
                          int level, const char *prefix);
static void print_expr1(FILE *fp, struct tree *expr, int level);
static const char *type2s(struct type *ty);
static const char *desig2s(struct desig *desig);

void vfprint(FILE *fp, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++) {
        if (*fmt == '%') {
            switch (*++fmt) {
            case 'c':
                fprintf(fp, "%c", (char)va_arg(ap, int));
                break;
            case 'd':
            case 'i':
                fprintf(fp, "%d", va_arg(ap, int));
                break;
            case 'u':
                fprintf(fp, "%u", va_arg(ap, unsigned int));
                break;
            case 'x':
                fprintf(fp, "%x", va_arg(ap, int));
                break;
            case 'X':
                fprintf(fp, "%X", va_arg(ap, int));
                break;
            case 'o':
                fprintf(fp, "%o", va_arg(ap, int));
                break;
            case 's':
                fputs(va_arg(ap, char *), fp);
                break;
            case 'p':
                fprintf(fp, "%p", va_arg(ap, void *));
                break;
            case 'f':
                fprintf(fp, "%f", va_arg(ap, double));
                break;
                // lu, ld, llu, lld
            case 'l':
                if (fmt[1] == 'd') {
                    fmt++;
                    fprintf(fp, "%ld", va_arg(ap, long));
                } else if (fmt[1] == 'u') {
                    fmt++;
                    fprintf(fp, "%lu", va_arg(ap, unsigned long));
                } else if (fmt[1] == 'l' && fmt[2] == 'd') {
                    fmt += 2;
                    fprintf(fp, "%lld", va_arg(ap, long long));
                } else if (fmt[1] == 'l' && fmt[2] == 'u') {
                    fmt += 2;
                    fprintf(fp, "%llu", va_arg(ap, unsigned long long));
                } else {
                    putc(*fmt, fp);
                }
                break;
                // Lf
            case 'L':
                if (fmt[1] == 'f') {
                    fmt++;
                    fprintf(fp, "%Lf", va_arg(ap, long double));
                } else {
                    putc(*fmt, fp);
                }
                break;
                /// customize
                // type
            case 'T':
                {
                    struct type *ty = va_arg(ap, struct type *);
                    fprintf(fp, "%s", type2s(ty));
                }
                break;
                // source
            case 'S':
                {
                    struct source src = va_arg(ap, struct source);
                    fprintf(fp, "%s:%u:%u", src.file, src.line, src.column);
                }
                break;
                // token
            case 't':
                {
                    struct token *t = va_arg(ap, struct token *);
                    fprintf(fp, "%s", tok2s(t));
                }
                break;
                // desig
            case 'D':
                {
                    struct desig *d = va_arg(ap, struct desig *);
                    fprintf(fp, "%s", desig2s(d));
                }
                break;
            default:
                putc(*fmt, fp);
                break;
            }
        } else {
            putc(*fmt, fp);
        }
    }
}

void fprint(FILE *fp, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(fp, fmt, ap);
    va_end(ap);
}

void print(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(stdout, fmt, ap);
    va_end(ap);
}

static void print_level(FILE *fp, int level)
{
    for (int i = 0; i < level; i++)
        fprint(fp, "  ");
}

static void print_ty(FILE *fp, struct type *ty)
{
    if (ty) {
        if (isfunc(ty) || isptr(ty) || isarray(ty))
            fprint(fp, RED_BOLD("'%s' "), TYPE_NAME(ty));

        fprint(fp, GREEN("'%T' "), ty);

        if (isarray(ty) || isstruct(ty) || isunion(ty)) {
            fprint(fp, "<" YELLOW("size=%ld") "> ", TYPE_SIZE(ty));
        } else if (isfunc(ty)) {
            fprint(fp, "%s ", TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
            if (TYPE_INLINE(ty))
                fprint(fp, "inline ");
        }
    }
}

static void print_field1(FILE *fp, struct field *field,
                         int level, const char *prefix)
{
    const char *name = field->name;
    struct type *ty = field->type;

    print_level(fp, level);
    fprint(fp, GREEN("%s ") YELLOW("%p "), prefix, field);

    if (field->isbit)
        fprint(fp, "<" RED("offset=%lu, bitoff=%d, bits=%d" "> "),
               field->offset, field->bitoff, field->bitsize);
    else
        fprint(fp, "<" GREEN("offset=%lu") "> ", field->offset);

    print_ty(fp, ty);

    if (name)
        fprint(fp, CYAN_BOLD("%s"), name);
    else
        fprint(fp, "anonymous");

    fprint(fp, "\n");
}

static void print_field(FILE *fp, struct field *field, int level)
{
    if (isindirect(field)) {
        print_level(fp, level);
        fprint(fp, GREEN("%s ") YELLOW("%p "), kIndirectFieldDecl, field);
        fprint(fp, "<" GREEN("offset=%lu") "> ", field->offset);
        fprint(fp, CYAN_BOLD("%s"), field->indir->name);
        fprint(fp, "\n");

        for (int i = 0; field->of[i]; i++)
            print_field1(fp, field->of[i], level + 1, kField);

        print_field1(fp, field->indir, level + 1, kField);
    } else {
        print_field1(fp, field, level, kFieldDecl);
    }
}

static void print_type1(FILE *fp, struct symbol *sym, int level)
{
    struct type *ty = sym->type;
    
    print_level(fp, level);
    if (sym->sclass == TYPEDEF)
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p ") CYAN_BOLD("%s "),
               kTypedefDecl, sym, sym->name);
    else if (isstruct(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kStructDecl, sym);
    else if (isunion(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kUnionDecl, sym);
    else if (isenum(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kEnumDecl, sym);
    else
        CC_UNAVAILABLE();
    print_ty(fp, ty);
    fprint(fp, "<" YELLOW("%s:line:%u col:%u") "> ",
           sym->src.file, sym->src.line, sym->src.column);
    fprint(fp, "\n");
    if (sym->sclass != TYPEDEF) {
        if (isstruct(ty) || isunion(ty)) {
            struct field *first = sym->u.s.flist;
            for (struct field *p = first; p; p = p->link)
                print_field(fp, p, level + 1);
        } else if (isenum(ty)) {
            struct symbol **ids = sym->u.s.ids;
            for (int i = 0; ids[i]; i++)
                print_symbol1(fp, ids[i], level + 1, kEnumConstDecl);
        }
    }
}

static void print_type(FILE *fp, struct symbol *sym)
{
    print_type1(fp, sym, 0);
}

static void print_symbol1(FILE *fp, struct symbol *sym,
                          int level, const char *prefix)
{
    print_level(fp, level);

    fprint(fp, GREEN_BOLD("%s "), prefix);
    fprint(fp, YELLOW("%p ") CYAN_BOLD("%s "), sym, sym->name);
    
    if (sym->defined)
        fprint(fp, "<" YELLOW("defined") "> ");

    struct type *ty = sym->type;
    print_ty(fp, ty);
    // location
    fprint(fp, "<" YELLOW("%s:line:%u col:%u") "> ",
           sym->src.file, sym->src.line, sym->src.column);
    // scope
    if (sym->scope >= LOCAL)
        fprint(fp, "<" YELLOW("scope:%d") ">", sym->scope);
    fprint(fp, "\n");

    if (isfuncdef(sym)) {
        //NOTE: print in ast_dump_symbol
    } else if (sym->sclass != ENUM) {
        // skip enum id
        struct tree *init = sym->u.init;
        if (init)
            print_expr1(fp, init, level + 1);
    }
}

static void print_symbol(FILE *fp, struct symbol *sym, const char *prefix)
{
    print_symbol1(fp, sym, 0, prefix);
}

static void print_init1(FILE *fp, struct tree *init, int level)
{
    for (struct init *p = init->s.u.ilist; p; p = p->link) {
        print_level(fp, level);
        if (p->desig->kind == DESIG_FIELD &&
            p->desig->u.field->isbit)
            fprint(fp, "<"
                   GREEN("offset=%lu, boff=%lu, bsize=%lu, type='%T'")
                   ">\n",
                   p->desig->offset,
                   p->desig->u.field->bitoff,
                   p->desig->u.field->bitsize,
                   p->desig->type);
        else
            fprint(fp, "<" GREEN("offset=%lu, type='%T'") ">\n",
                   p->desig->offset, p->desig->type);;

        if (p->body)
            print_expr1(fp, p->body, level + 1);
    }
}

static void print_args1(FILE *fp, struct tree **args, int level)
{
    assert(args);
    for (int i = 0; args[i]; i++) {
        print_level(fp, level);
        fprint(fp, "ARG[%d]: \n", i);
        print_expr1(fp, args[i], level + 1);
    }
}

static void print_expr1(FILE *fp, struct tree *expr, int level)
{
    print_level(fp, level);
    fprint(fp, PURPLE_BOLD("%s%s ") YELLOW("%p "),
           opkindstr[opindex(expr->op)], optypestr[OPTYPE(expr->op)], expr);
    fprint(fp, GREEN("'%T' "), expr->type);

    if (issliteral(expr)) {
        fprint(fp, CYAN_BOLD("\"%s\""), expr->s.sym->name);
    } else if (isiliteral(expr)) {
        if (TYPE_OP(expr->type) == INT)
            fprint(fp, RED("%ld"), expr->s.value.i);
        else
            fprint(fp, RED("%lu"), expr->s.value.u);
    } else if (isfliteral(expr)) {
        if (TYPE_KIND(expr->type) == FLOAT)
            fprint(fp, RED("%f"), (float)expr->s.value.d);
        else if (TYPE_KIND(expr->type) == DOUBLE)
            fprint(fp, RED("%f"), (double)expr->s.value.d);
        else
            fprint(fp, RED("%Lf"), (long double)expr->s.value.d);
    } else if (ispliteral(expr)) {
        fprint(fp, RED("%p"), expr->s.value.p);
    } else if (expr->s.sym) {
        fprint(fp, CYAN_BOLD("%s"), expr->s.sym->name);
    }

    fprint(fp, "\n");
    if (expr->kids[0])
        print_expr1(fp, expr->kids[0], level + 1);
    if (expr->kids[1])
        print_expr1(fp, expr->kids[1], level + 1);

    if (OPKIND(expr->op) == CALL)
        // print arguments
        print_args1(fp, expr->s.u.args, level + 1);
    else if (iscpliteral(expr))
        // print compound literal
        print_init1(fp, COMPOUND_SYM(expr)->u.init, level + 1);
    else if (OPKIND(expr->op) == INITS)
        print_init1(fp, expr, level + 1);
}

static void print_stmt1(FILE *fp, struct stmt *stmt, int level)
{
    if (stmt->id != GEN)
        print_level(fp, level);

    switch (stmt->id) {
    case LABEL:
        fprint(fp, ".L%d:\n", stmt->u.label);
        break;

    case GEN:
        assert(stmt->u.expr && "null expr in gen node");
        print_expr1(fp, stmt->u.expr, level);
        break;

    case JMP:
        fprint(fp, "goto .L%d\n", stmt->u.label);
        break;

    case CBR:
        if (stmt->u.cbr.tlab)
            fprint(fp, "iftrue goto .L%d\n", stmt->u.cbr.tlab);
        else if (stmt->u.cbr.flab)
            fprint(fp, "iffalse goto .L%d\n", stmt->u.cbr.flab);

        if (stmt->u.cbr.expr)
            print_expr1(fp, stmt->u.cbr.expr, level + 1);
        break;

    case RET:
        fprint(fp, "ret\n");
        if (stmt->u.expr)
            print_expr1(fp, stmt->u.expr, level + 1);
        break;

    default:
        assert(0 && "unknown stmt type");
    }
}

void print_expr(FILE *fp, struct tree *expr)
{
    print_expr1(fp, expr, 0);
}

void ast_dump_vardecl(struct symbol *n)
{
    print_symbol(stdout, n, kVarDecl);
}

void ast_dump_funcdecl(struct symbol *n)
{
    print_symbol(stdout, n, kFuncDecl);
}

void ast_dump_funcdef(struct symbol *n)
{
    print_symbol(stdout, n, kFuncDecl);
    for (struct symbol *sym = n->u.f.lvars; sym; sym = sym->local) {
        if (!sym->temporary && !(sym->predefine && sym->refs == 0))
            print_symbol1(stdout, sym, 1, kVarDecl);
    }
    for (struct stmt *stmt = n->u.f.stmt; stmt; stmt = stmt->next)
        print_stmt1(stdout, stmt, 1);
}

void ast_dump_typedecl(struct symbol *n)
{
    print_type(stdout, n);
}

/// Convert type node to string.

static const char *qualstr(int q)
{
    switch (q) {
    case CONST:
        return "const ";
    case VOLATILE:
        return "volatile ";
    case RESTRICT:
        return "restrict ";
    case CONST+VOLATILE:
        return "const volatile ";
    case CONST+RESTRICT:
        return "const restrict ";
    case VOLATILE+RESTRICT:
        return "volatile restrict ";
    case CONST+VOLATILE+RESTRICT:
        return "const volatile restrict ";
    default:
        return "";
    }
}

static void dotype2s(struct type **stack, int *spp, char **bpp, char *be)
{
    struct type *ty, *rty;
    struct type **params;
    int sp;
    char *bp;

    if ((sp = *spp) < 0)
        return;

    bp = *bpp;
    ty = stack[sp];
    switch (TYPE_KIND(ty)) {
    case POINTER:
        // leading space
        rty = rtype(ty);
        if (isptr(rty) || isarray(rty) || isfunc(rty))
            snprintf(bp, be - bp, "*%s", qualstr(ty->kind));
        else
            snprintf(bp, be - bp, " *%s", qualstr(ty->kind));
        *bpp += strlen(bp);
        *spp -= 1;
        break;
    case FUNCTION:
        *spp -= 1;
        if (sp - 1 >= 0 &&
            (isptr(stack[sp-1]) ||
             isfunc(stack[sp-1]) ||
             isarray(stack[sp-1]))) {
            snprintf(bp, be - bp, " (");
            bp += strlen(bp);
            dotype2s(stack, spp, &bp, be);
            snprintf(bp, be - bp, ")");
            bp += strlen(bp);
        }
        snprintf(bp, be - bp, " (");
        bp += strlen(bp);
        params = TYPE_PROTO(ty);
        for (int i = 0; params[i]; i++) {
            struct type *pty = params[i];
            const char *s = type2s(pty);
            snprintf(bp, be - bp, "%s", s);
            bp += strlen(bp);
            if (params[i+1]) {
                snprintf(bp, be - bp, ", ");
                bp += strlen(bp);
            }
        }
        if (TYPE_VARG(ty)) {
            snprintf(bp, be - bp, ", ...");
            bp += strlen(bp);
        }
        snprintf(bp, be - bp, ")");
        bp += strlen(bp);
        *bpp = bp;
        break;
    case ARRAY:
        *spp -= 1;
        if (sp - 1 >= 0 && isptr(stack[sp-1])) {
            snprintf(bp, be - bp, " (");
            bp += strlen(bp);
            dotype2s(stack, spp, &bp, be);
            snprintf(bp, be - bp, ")");
            bp += strlen(bp);
        } else if (sp - 1 >= 0 && isarray(stack[sp-1])) {
            dotype2s(stack, spp, &bp, be);
        }
        if (TYPE_LEN(ty) > 0)
            snprintf(bp, be - bp, "[%lu]", TYPE_LEN(ty));
        else
            snprintf(bp, be - bp, "[]");
        bp += strlen(bp);
        *bpp = bp;
        break;
    default:
        snprintf(bp, be - bp, "%s%s", qualstr(ty->kind), TYPE_NAME(ty));
        if (istag(ty) && !TYPE_TSYM(ty)->anonymous) {
            bp += strlen(bp);
            snprintf(bp, be - bp, " %s", TYPE_TSYM(ty)->name);
        }
        *spp -= 1;
        *bpp += strlen(bp);
        break;
    }
    dotype2s(stack, spp, bpp, be);
}

static const char *type2s(struct type *ty)
{
    char buf[1024];
    char *bp = buf;
    unsigned int alloc = 10;
    struct type **stack = xmalloc(alloc);
    int sp = -1;

    while (ty) {
        if (++sp >= alloc) {
            alloc = alloc * 2 + 1;
            stack = xrealloc(stack, alloc * sizeof(struct type *));
        }
        stack[sp] = ty;
        if (!(isptr(ty) || isarray(ty) || isfunc(ty)))
            break;
        ty = rtype(ty);
    }
    *bp = 0;
    dotype2s(stack, &sp, &bp, bp + ARRAY_SIZE(buf));
    free(stack);
    return strip(buf);
}

// for debug
static const char *desig2s(struct desig *desig)
{
    const char *s = "";
    
    assert(desig);

    for (struct desig *d = desig; d;) {
        switch (d->kind) {
        case DESIG_NONE:
            assert(d->prev == NULL);
            if (d->all) {
                d = d->all;
                continue;
            } else {
                s = format("<%s>%s", type2s(d->type), s);
            }
            break;

        case DESIG_FIELD:
            s = format(".%s%s", d->u.field->name, s);
            break;

        case DESIG_INDEX:
            s = format("[%ld]%s", d->u.index, s);
            break;

        default:
            assert(0 && "unknown designator type");
        }
        d = d->prev;
    }
    
    return s;
}
