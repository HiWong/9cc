#include "cc.h"
#include "sys.h"
#include <stdint.h>

/**
 *  X86_64 registers
 *
 *  64-bit  32-bit  16-bit  8-bit
 *  rax     eax     ax      al,ah
 *  rbx     ebx     bx      bl,bh
 *  rcx     ecx     cx      cl,ch
 *  rdx     edx     dx      dl,dh
 *  rbp     ebp     bp
 *  rsp     esp     sp
 *  rsi     esi     si
 *  rdi     edi     di
 *  rip     eip     ip
 *  r8~r15
 *
 *  Segment registers
 *
 *  cs,ds,es,fs,gs,ss
 */
enum {
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSP,
    RSI,
    RDI,
    R8,
    R9,
    REGS
};
struct reg {
    const char *r;
    const char *r64;
    const char *r32;
    const char *r16;
    const char *r8;
    bool using;
};
static struct reg *registers[REGS];
static struct reg *int_regs[6];
static struct reg *float_regs[8];

static FILE *outfp;
static struct dict *compound_lits;
static struct dict *string_lits;

static void emit_initializer(node_t * t);
static void emit_stmt(node_t * n);
static void emit_expr(node_t * n);
static const char *func_ret_label;

static struct reg *make_reg(struct reg *r)
{
    struct reg *reg = zmalloc(sizeof(struct reg));
    memcpy(reg, r, sizeof(struct reg));
    return reg;
}

static void init_regs(void)
{
    registers[RAX] = make_reg(&(struct reg){
            .r = "%rax",
            .r64 = "%rax",
            .r32 = "%eax",
            .r16 = "%ax",
            .r8 = "%al"
        });
    registers[RBX] = make_reg(&(struct reg){
            .r = "%rbx",
            .r64 = "%rbx",
            .r32 = "%ebx",
            .r16 = "%bx",
            .r8 = "%bl"
        });
    registers[RCX] = make_reg(&(struct reg){
            .r = "%rcx",
            .r64 = "%rcx",
            .r32 = "%ecx",
            .r16 = "%cx",
            .r8 = "%cl"
        });
    registers[RDX] = make_reg(&(struct reg){
            .r = "%rdx",
            .r64 = "%rdx",
            .r32 = "%edx",
            .r16 = "%dx",
            .r8 = "%dl"
        });
    registers[RBP] = make_reg(&(struct reg){
            .r = "%rbp",
            .r64 = "%rbp",
            .r32 = "%ebp",
            .r16 = "%bp"
        });
    registers[RSP] = make_reg(&(struct reg){
            .r = "%rsp",
            .r64 = "%rsp",
            .r32 = "%esp",
            .r16 = "%sp"
        });
    registers[RSI] = make_reg(&(struct reg){
            .r = "%rsi",
            .r64 = "%rsi",
            .r32 = "%esi",
            .r16 = "%si"
        });
    registers[RDI] = make_reg(&(struct reg){
            .r = "%rdi",
            .r64 = "%rdi",
            .r32 = "%edi",
            .r16 = "%di"
        });
    registers[R8] = make_reg(&(struct reg){
            .r = "%r8d",
            .r64 = "%r8d"
        });
    registers[R9] = make_reg(&(struct reg){
            .r = "%r9d",
            .r64 = "%r9d"
        });

    // init integer regs
    int_regs[0] = registers[RDI];
    int_regs[1] = registers[RSI];
    int_regs[2] = registers[RDX];
    int_regs[3] = registers[RCX];
    int_regs[4] = registers[R8];
    int_regs[5] = registers[R9];

    // init floating regs
    for (int i = 0; i < ARRAY_SIZE(float_regs); i++)
        float_regs[i]->r = format("%%xmm%d", i);
}

/**
 * According to the ABI, the first 6 integer or pointer
 * arguments to a function are passed in registers:
 * rdi, rsi, rdx, rcx, r8, r9
 */
static const char *get_intreg(node_t *ty)
{
    for (int i = 0; i < ARRAY_SIZE(int_regs); i++) {
        struct reg *r = int_regs[i];
        if (r->using)
            continue;
        r->using = true;
        switch (TYPE_SIZE(ty)) {
        case 1:
            return r->r8;
        case 2:
            return r->r16;
        case 4:
            return r->r32;
        case 8:
            return r->r64;
        default:
            cc_assert(0);
            return NULL;
        }
    }
    return NULL;
}

static const char *get_floatreg(void)
{
    for (int i = 0; i < ARRAY_SIZE(float_regs); i++) {
        struct reg *r = float_regs[i];
        if (r->using)
            continue;
        r->using = true;
        return r->r;
    }
    return NULL;
}

static const char *mov(node_t *ty)
{
    switch (TYPE_SIZE(ty)) {
    case 1:
        return "movb";
    case 2:
        return "movw";
    case 4:
        return "movl";
    case 8:
        return "movq";
    default:
        cc_assert(0);
        return NULL;
    }
}

static void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(outfp, "\t");
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static void emit_noindent(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

static const char *emit_string_literal_label(const char *name)
{
    const char *label = dict_get(string_lits, name);
    if (!label) {
        label = gen_sliteral_label();
        dict_put(string_lits, name, (void *)label);
    }
    return label;
}

static const char *emit_compound_literal_label(node_t * n)
{
    const char *label = gen_compound_label();
    dict_put(compound_lits, label, n);
    return label;
}

static void emit_zero(size_t bytes)
{
    emit(".zero %llu", bytes);
}

static const char *get_ptr_label(node_t * n)
{
    const char *label = NULL;
    switch (AST_ID(n)) {
    case STRING_LITERAL:
        label = emit_string_literal_label(SYM_NAME(EXPR_SYM(n)));
        break;
    case REF_EXPR:
        label = SYM_X_LABEL(EXPR_SYM(n));
        break;
    case BINARY_OPERATOR:
        label = get_ptr_label(EXPR_OPERAND(n, 0));
        break;
    case UNARY_OPERATOR:
        cc_assert(EXPR_OP(n) == '&');
        label = get_ptr_label(EXPR_OPERAND(n, 0));
        break;
    case INITS_EXPR:
        label = emit_compound_literal_label(n);
        break;
    default:
        die("unkown ptr node: %s", node2s(n));
    }
    return label;
}

static void emit_address_initializer(node_t * init)
{
    node_t *ty = AST_TYPE(init);
    if (isiliteral(init)) {
        emit(".quad %llu", ILITERAL_VALUE(init));
    } else {
        const char *label = get_ptr_label(init);
        if (AST_ID(init) == BINARY_OPERATOR) {
            node_t *r = EXPR_OPERAND(init, 1);
            int op = EXPR_OP(init);
            if (op == '+') {
                if (TYPE_OP(AST_TYPE(r)) == INT) {
                    long long i = ILITERAL_VALUE(r);
                    if (i < 0)
                        emit(".quad %s%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                    else
                        emit(".quad %s+%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                } else {
                    emit(".quad %s+%llu", label,
                         ILITERAL_VALUE(r) *
                         TYPE_SIZE(rtype(ty)));
                }
            } else {
                if (TYPE_OP(AST_TYPE(r)) == INT) {
                    long long i = ILITERAL_VALUE(r);
                    if (i < 0)
                        emit(".quad %s+%lld", label,
                             -i * TYPE_SIZE(rtype(ty)));
                    else
                        emit(".quad %s-%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                } else {
                    emit(".quad %s-%llu", label,
                         ILITERAL_VALUE(r) *
                         TYPE_SIZE(rtype(ty)));
                }
            }
        } else {
            emit(".quad %s", label);
        }
    }
}

static void emit_arith_initializer(node_t * init)
{
    node_t *ty = AST_TYPE(init);
    switch (TYPE_KIND(ty)) {
    case _BOOL:
    case CHAR:
        emit(".byte %d", ILITERAL_VALUE(init));
        break;
    case SHORT:
        emit(".short %d", ILITERAL_VALUE(init));
        break;
    case INT:
    case UNSIGNED:
        emit(".long %d", ILITERAL_VALUE(init));
        break;
    case LONG:
    case LONG + LONG:
        emit(".quad %llu", ILITERAL_VALUE(init));
        break;
    case FLOAT:
        {
            float f = FLITERAL_VALUE(init);
            emit(".long %d", *(uint32_t *) & f);
        }
        break;
    case DOUBLE:
    case LONG + DOUBLE:
        {
            double d = FLITERAL_VALUE(init);
            emit(".quad %llu", *(uint64_t *) & d);
        }
        break;
    default:
        die("unknown type '%s'", type2s(ty));
    }
}

static void emit_array_initializer(node_t * n)
{
    if (issliteral(n)) {
        const char *label = get_ptr_label(n);
        emit(".quad %s", label);
    } else {
        cc_assert(AST_ID(n) == INITS_EXPR);
        int i;
        for (i = 0; i < LIST_LEN(EXPR_INITS(n)); i++)
            emit_initializer(EXPR_INITS(n)[i]);
        if (TYPE_LEN(AST_TYPE(n)) - i > 0)
            emit_zero((TYPE_LEN(AST_TYPE(n)) -
                       i) * TYPE_SIZE(rtype(AST_TYPE(n))));
    }
}

static void emit_struct_initializer(node_t * n)
{
    cc_assert(AST_ID(n) == INITS_EXPR);
    node_t *ty = AST_TYPE(n);
    node_t **fields = TYPE_FIELDS(ty);
    node_t **inits = EXPR_INITS(n);
    for (int i = 0; i < LIST_LEN(inits); i++) {
        node_t *init = inits[i];
        node_t *field = fields[i];
        size_t offset = FIELD_OFFSET(field);
        if (FIELD_ISBIT(field)) {
            int old_bits = 0;
            unsigned long long old_byte = 0;
            for (; i < LIST_LEN(inits); i++) {
                node_t *next =
                    i <
                    LIST_LEN(inits) - 1 ? fields[i + 1] : NULL;
                field = fields[i];
                init = inits[i];
                if (next
                    && FIELD_OFFSET(field) !=
                    FIELD_OFFSET(next))
                    break;
                int bits = FIELD_BITSIZE(field);
                unsigned long long byte = 0;
                if (isiliteral(init))
                    byte = ILITERAL_VALUE(init);
                while (bits + old_bits >= 8) {
                    unsigned char val;
                    unsigned char l =
                        byte & ~(~0 << (8 - old_bits));
                    unsigned char r =
                        old_byte & ~(~0 << old_bits);
                    val = (l << old_bits) | r;
                    old_bits = 0;
                    old_byte = 0;
                    bits -= 8 - old_bits;
                    byte >>= 8 - old_bits;
                    emit(".byte %d", val);
                    offset += 1;
                }
                old_bits += bits;
                old_byte += byte;
            }
            if (old_bits) {
                unsigned char r = old_byte & ~(~0 << old_bits);
                emit(".byte %d", r);
                offset += 1;
            }
        } else {
            node_t *fty = FIELD_TYPE(field);
            if (TYPE_SIZE(fty)) {
                if (AST_ID(init) == VINIT_EXPR)
                    emit_zero(TYPE_SIZE(fty));
                else
                    emit_initializer(init);
                offset += TYPE_SIZE(fty);
            }
        }
        // pack
        node_t *next = i < LIST_LEN(inits) - 1 ? fields[i + 1] : NULL;
        size_t end;
        if (next)
            end = FIELD_OFFSET(next);
        else
            end = TYPE_SIZE(ty);
        if (end - offset)
            emit_zero(end - offset);
    }
}

static void emit_initializer(node_t * init)
{
    node_t *ty = AST_TYPE(init);
    if (isarith(ty))
        emit_arith_initializer(init);
    else if (isptr(ty))
        emit_address_initializer(init);
    else if (isarray(ty))
        emit_array_initializer(init);
    else
        emit_struct_initializer(init);
}

static void emit_data(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    const char *label = glabel(SYM_X_LABEL(sym));
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    emit(".data");
    if (TYPE_ALIGN(ty) > 1)
        emit(".align %d", TYPE_ALIGN(ty));
    emit_noindent("%s:", label);
    emit_initializer(DECL_BODY(n));
}

static void emit_bss(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) == STATIC)
        emit(".lcomm %s,%llu,%d", glabel(SYM_X_LABEL(sym)), TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
    else
        emit(".comm  %s,%llu,%d", glabel(SYM_X_LABEL(sym)), TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
}

static void emit_bop_plus(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    if (isint(AST_TYPE(l)) && isint(AST_TYPE(r))) {
        // ADDI
        emit_expr(l);
        emit_expr(r);
    } else if (isfloat(AST_TYPE(l)) && isfloat(AST_TYPE(r))) {
        
    } else {
        
    }
}

static void emit_bop(node_t *n)
{
    int op = EXPR_OP(n);
    switch (op) {
    case '=':
    case ',':
        // int
    case '%':
    case '|':
    case '&':
    case '^':
    case LSHIFT:
    case RSHIFT:
        // arith
    case '*':
    case '/':
        // scalar
    case '<':
    case '>':
    case GEQ:
    case LEQ:
    case EQ:
    case NEQ:
        break;
    case '+':
        emit_bop_plus(n);
        break;
    case '-':
    case AND:
    case OR:
        break;
    default:
        cc_assert(0);
    }
}

static void emit_uop(node_t *n)
{
    int op = EXPR_OP(n);
    switch (op) {
    case INCR:
    case DECR:
    case '*':
    case '&':
    case '+':
    case '-':
    case '~':
    case '!':
    case SIZEOF:
        break;
    default:
        cc_assert(0);
    }
}

static void emit_compound_literal(node_t *n)
{
    
}

static void emit_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *name = SYM_NAME(sym);
    const char *label = emit_string_literal_label(name);
    EXPR_X_ADDR(n) = format("$%s", label);
}

static void emit_float_literal(node_t *n)
{
    
}

static void emit_integer_literal(node_t *n)
{
    const char *addr = format("$%llu", ILITERAL_VALUE(n));
    EXPR_X_ADDR(n) = addr;
}

static void emit_ref_expr(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (isfunc(ty) || has_static_extent(sym))
        EXPR_X_ADDR(n) = glabel(SYM_X_LABEL(sym));
    else
        EXPR_X_ADDR(n) = format("-%ld(%rbp)", SYM_X_LOFF(sym));
}

static void emit_funcall(node_t *n)
{
    node_t *node = EXPR_OPERAND(n, 0);
    node_t **args = EXPR_ARGS(n);

    emit_expr(node);

    for (int i = 0; i < LIST_LEN(args); i++) {
        node_t *arg = args[i];
        emit_expr(arg);
        node_t *ty = AST_TYPE(arg);
        if (isint(ty) || isptr(ty)) {
            const char *reg = get_intreg(ty);
            EXPR_X_REG(arg) = reg;
        } else if (isfloat(ty)) {
            
        } else if (isstruct(ty)) {
            
        } else if (isunion(ty)) {
            
        } else {
            die("unknown argument type: %s", type2s(ty));
        }
    }

    for (int i = LIST_LEN(args) - 1; i >= 0; i--) {
        node_t *arg = args[i];
        node_t *ty = AST_TYPE(arg);
        emit("%s %s, %s", mov(ty), EXPR_X_ADDR(arg), EXPR_X_REG(arg));
    }
    
    emit("callq %s", EXPR_X_ADDR(node));
}

static void arith2arith(node_t *dty, node_t *l)
{
    
}

static void ptr2arith(node_t *dty, node_t *l)
{
    
}

static void ptr2ptr(node_t *dty, node_t *l)
{
    
}

static void arith2ptr(node_t *dty, node_t *l)
{
    
}

static void func2ptr(node_t *dty, node_t *l)
{
    
}

static void array2ptr(node_t *dty, node_t *l)
{
    
}

static void emit_conv(node_t *n)
{
    node_t *dty = AST_TYPE(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *sty = AST_TYPE(l);
    emit_expr(l);
    if (isarith(dty)) {
        if (isarith(sty))
            arith2arith(dty, l);
        else if (isptr(sty))
            ptr2arith(dty, l);
        else
            cc_assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            ptr2ptr(dty, l);
        else if (isarith(sty))
            arith2ptr(dty, l);
        else if (isfunc(sty))
            func2ptr(dty, l);
        else if (isarray(sty))
            array2ptr(dty, l);
        else
            cc_assert(0);
    } else {
        
    }
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static void emit_cond(node_t *n)
{
    
}

static void emit_member(node_t *n)
{
    
}

static void emit_subscript(node_t *n)
{
    
}

static void emit_expr(node_t * n)
{
    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
        emit_bop(n);
        break;
    case UNARY_OPERATOR:
        emit_uop(n);
        break;
    case PAREN_EXPR:
        emit_expr(EXPR_OPERAND(n, 0));
        break;
    case COND_EXPR:
        emit_cond(n);
        break;
    case MEMBER_EXPR:
        emit_member(n);
        break;
    case SUBSCRIPT_EXPR:
        emit_subscript(n);
        break;
    case CAST_EXPR:
    case CONV_EXPR:
        emit_conv(n);
        break;
    case CALL_EXPR:
        emit_funcall(n);
        break;
    case REF_EXPR:
        if (EXPR_OP(n) == ENUM)
            emit_integer_literal(n);
        else
            emit_ref_expr(n);
        break;
    case INTEGER_LITERAL:
        emit_integer_literal(n);
        break;
    case FLOAT_LITERAL:
        emit_float_literal(n);
        break;
    case STRING_LITERAL:
        emit_string_literal(n);
        break;
    case COMPOUND_LITERAL:
        emit_compound_literal(n);
        break;
    case INITS_EXPR:
    case VINIT_EXPR:
    default:
        die("unknown node (%s)'%s'", nname(n), node2s(n));
        break;
    }
}

static void emit_decl_init(node_t * init, size_t offset)
{

}

static void emit_decl(node_t * n)
{
    if (DECL_BODY(n))
        emit_decl_init(DECL_BODY(n), SYM_X_LOFF(DECL_SYM(n)));
}

static void emit_compound(node_t * n)
{
    cc_assert(AST_ID(n) == AST_COMPOUND);
    for (int i = 0; i < LIST_LEN(STMT_LIST(n)); i++)
        emit_stmt(STMT_LIST(n)[i]);
}

static void emit_label(const char *label)
{
    emit_noindent("%s:", label);
}

static void emit_jmp(const char *label)
{
    emit("jmp %s", label);
}

static void emit_je(const char *label)
{
    emit("je %s", label);
}

static void emit_if(node_t * n)
{
    emit_expr(STMT_COND(n));
    const char *ne = gen_label();
    emit_je(ne);
    if (STMT_THEN(n))
        emit_stmt(STMT_THEN(n));
    if (STMT_ELSE(n)) {
        const char *end = gen_label();
        emit_jmp(end);
        emit_label(ne);
        emit_stmt(STMT_ELSE(n));
        emit_label(end);
    } else {
        emit_label(ne);
    }
}

static void emit_stmt(node_t * n)
{
    if (isexpr(n)) {
        emit_expr(n);
    } else if (isdecl(n)) {
        emit_decl(n);
    } else {
        switch (AST_ID(n)) {
        case AST_IF:
            emit_if(n);
            break;
        case AST_COMPOUND:
            emit_compound(n);
            break;
        case AST_RETURN:
            emit_expr(STMT_OPERAND(n));
            emit_jmp(func_ret_label);
            break;
        case AST_LABEL:
            emit_label(STMT_LABEL(n));
            break;
        case AST_JUMP:
            emit_jmp(STMT_LABEL(n));
            break;
        default:
            // null statement
            cc_assert(AST_ID(n) == NULL_STMT);
            break;
        }
    }
}

static void emit_funcdef(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    const char *label = glabel(SYM_X_LABEL(sym));
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    size_t sub = 0;
    for (int i = 0; i < LIST_LEN(DECL_X_LVARS(n)); i++) {
        node_t *lvar = DECL_X_LVARS(n)[i];
        node_t *sym = DECL_SYM(lvar);
        size_t offset = sub + TYPE_SIZE(SYM_TYPE(sym));
        SYM_X_LOFF(sym) = offset;
        sub = ROUNDUP(offset, 8);
    }
    func_ret_label = gen_label();
    emit_noindent("%s:", label);
    emit("pushq %%rbp");
    emit("movq %%rsp, %%rbp");
    if (sub)
        emit("sub $%lld, %%rbp", sub);
    emit_stmt(DECL_BODY(n));
    emit_label(func_ret_label);
    emit("popq %%rbp");
    emit("ret");
}

static void emit_compound_literals(void)
{
    if (vec_len(compound_lits->keys))
        emit(".data");
    for (int i = 0; i < vec_len(compound_lits->keys); i++) {
        const char *label = vec_at(compound_lits->keys, i);
        node_t *init = dict_get(compound_lits, label);
        emit_noindent("%s:", label);
        emit_initializer(init);
    }
}

static void emit_string_literals(void)
{
    if (vec_len(string_lits->keys))
        emit(".section .rodata");
    for (int i = 0; i < vec_len(string_lits->keys); i++) {
        const char *name = vec_at(string_lits->keys, i);
        const char *label = dict_get(string_lits, name);
        emit_noindent("%s:", label);
        emit(".asciz %s", name);
    }
}

static void gen_init(FILE *fp)
{
    outfp = fp;
    compound_lits = dict_new();
    string_lits = dict_new();
    init_regs();
}

void gen(node_t * tree, FILE * fp)
{
    cc_assert(errors == 0 && fp);
    gen_init(fp);
    node_t **exts = DECL_EXTS(tree);
    for (int i = 0; i < LIST_LEN(exts); i++) {
        node_t *n = exts[i];
        if (isfuncdef(n)) {
            emit_funcdef(n);
        } else if (isvardecl(n)) {
            if (DECL_BODY(n))
                emit_data(n);
            else
                emit_bss(n);
        }
    }
    emit_compound_literals();
    emit_string_literals();
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
