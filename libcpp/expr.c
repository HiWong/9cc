#include "lex.h"
#include "internal.h"

static struct cpp_num expr(void);
static struct cpp_num cond(void);

#define is_assign_op(op)    (((op) == '=') || ((op) >= MULEQ && (op) <= RSHIFTEQ))

struct cpp_num {
    union value v;
};


/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static struct cpp_num primary(void)
{
    int t = token->id;
    struct cpp_num num;
    
    switch (t) {
    case ICONSTANT:
        num.v.i = token->u.lit.v.i;
        expect(t);
        break;

    case FCONSTANT:
        cpp_error("floating constant in preprocessor expression");
        num.v.i = 0;
        expect(t);
        break;

    case '(':
        expect('(');
        num = expr();
        expect(')');
        break;

    case ID:
    case SCONSTANT:
    default:
        cpp_error("token '%s' is not valid in preprocessor expression", tok2s(token));
        num.v.i = 0;
        expect(t);
        break;
    }

    return num;
}

/// postfix-expression:
///   primary-expression
///   postfix-expression '[' expression ']'
///   postfix-expression '(' argument-expression-list[opt] ')'
///   postfix-expression '.' identifier
///   postfix-expression '->' identifier
///   postfix-expression '++'
///   postfix-expression '--'
///   '(' type-name ')' '{' initializer-list '}'
///   '(' type-name ')' '{' initializer-list ',' '}'
///
static struct cpp_num postfix(void)
{
    
}

/// unary-expression:
///   postfix-expression
///   '++' unary-expression
///   '--' unary-expression
///   unary-operator cast-expression
///   'sizeof' unary-expression
///   'sizeof' '(' type-name ')'
///
static struct cpp_num unary(void)
{
    
}

/// cast-expression:
///   unary-expression
///   '(' type-name ')' cast-expression
///
static struct cpp_num cast(void)
{
    
}

/// multiplicative-expression:
///   cast-expression
///   multiplicative-expression '*' cast-expression
///   multiplicative-expression '/' cast-expression
///   multiplicative-expression '%' cast-expression
///
static struct cpp_num multiple(void)
{
    
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static struct cpp_num additive(void)
{
    
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static struct cpp_num shift(void)
{
    
}

/// relational-expression:
///   shift-expression
///   relational-expression '<' shift-expression
///   relational-expression '>' shift-expression
///   relational-expression '<=' shift-expression
///   relational-expression '>=' shift-expression
///
static struct cpp_num relational(void)
{
    
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   euqality-expression '!=' relational-expression
///
static struct cpp_num equality(void)
{
    
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static struct cpp_num and(void)
{
    
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static struct cpp_num exclusive_or(void)
{
    
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static struct cpp_num inclusive_or(void)
{
    
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static struct cpp_num logical_and(void)
{
    
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static struct cpp_num logical_or(void)
{
    
}

static struct cpp_num cond1(struct cpp_num num)
{
    struct cpp_num num, then, els;

    expect('?');
    then = expr();
    expect(':');
    

    return num;
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static struct cpp_num cond(void)
{
    struct cpp_num num;

    num = logical_or();
    if (token->id == '?')
        return cond1(num);

    return num;
}

/// assignment-expression:
///   conditional-expression
///   unary-expression assignment-operator assignment-expression
///
/// assignment-operator:
///   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
///
static struct cpp_num assign(void)
{
    struct cpp_num num;

    num = logical_or();
    if (token->id == '?')
        return cond1(num);
    if (is_assign_op(token->id)) {
        cpp_error("assignment is not allowed in preprocessor expression");
        expect(token->id);
        num = assign();
    }

    return num;
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
static struct cpp_num expr(void)
{
    struct cpp_num num;

    num = assign();
    while (token->id == ',') {
        expect(',');
        num = assign();
    }

    return num;
}

static int intexpr(void)
{
    struct cpp_num num;

    num = expr();
    return num.v.i;
}

/// constant-expression:
///   conditional-expression
///
bool eval_cpp_const_expr(void)
{
    gettok();
    return intexpr();
}
