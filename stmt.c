#include "cc.h"

static union node * statement(union node *context);
static union node * _compound_stmt(union node *context);

static union node * expr_stmt()
{
    union node *ret = NULL;
    
    if (token->id == ';') {
        // do nothing
    } else if (firstexpr(token)) {
	union node *e = expression();
	if (e)
	    ret = ast_stmt(EXPR_STMT, e, NULL);
    } else {
        error("missing statement before '%s'", token->name);
    }
    
    expect(';');
    
    return ret;
}

static union node * if_stmt(union node *context)
{
    union node *ret;
    union node *expr;
    union node *stmt1;
    
    expect(IF);
    expect('(');
    expr = expression();
    expect(')');
    
    stmt1 = statement(context);
    ret = ast_stmt(IF_STMT, expr, stmt1);
    
    if (token->id == ELSE) {
        expect(ELSE);
        ret = ast_stmt(ELSE_STMT, ret, statement(context));
    }
    
    return ret;
}

static union node * while_stmt(union node *context)
{
    union node *expr;
    union node *ret;
    
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = ast_stmt(WHILE_STMT, expr, NULL);
    STMT_UP(ret) = context;
    AST_KID(ret, 1) = statement(ret);
    STMT_UP(ret) = NULL;
    
    return ret;
}

static union node * do_while_stmt(union node *context)
{
    union node *stmt;
    union node *expr;
    union node *ret;
    
    expect(DO);
    
    ret = ast_stmt(DO_WHILE_STMT, NULL, NULL);
    STMT_UP(ret) = context;
    stmt = statement(ret);
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    expect(';');
    AST_KID(ret, 0) = stmt;
    AST_KID(ret, 1) = expr;
    STMT_UP(ret) = NULL;
    
    return ret;
}

static union node * for_stmt(union node *context)
{
    union node *ret = ast_stmt(FOR_STMT, NULL, NULL);
    
    expect(FOR);
    expect('(');
    
    enter_scope();
    
    if (token->id == ';') {
        expect(';');
    } else {
        if (firstdecl(token)) {
            // declaration
            STMT_DECL(ret) = declaration();
        } else {
            // expression
            STMT_INIT(ret) = expression();
            expect(';');
        }
    }
    
    if (token->id != ';')
        STMT_COND(ret) = expression();
    
    expect(';');
    
    if (token->id != ')')
        STMT_CTRL(ret) = expression();
    
    expect(')');
    
    STMT_UP(ret) = context;
    AST_KID(ret, 0) = statement(ret);
    STMT_UP(ret) = NULL;
    
    exit_scope();
    
    return ret;
}

static union node * switch_stmt(union node *context)
{
    union node *expr;
    union node *ret;
    
    expect(SWITCH);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = ast_stmt(SWITCH_STMT, expr, NULL);
    STMT_UP(ret) = context;
    AST_KID(ret, 1) = statement(ret);
    STMT_UP(ret) = NULL;
    
    return ret;
}

static union node * case_stmt(union node *context)
{
    int in_sw = 0;
    int val;
    union node *stmt;
    struct source src = source;
    
    expect(CASE);
    val = intexpr();
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = STMT_UP(context);
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'case' statement is not in a switch statement.");
    
    // always parse even if not in a switch statement
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    
    union node *ret = ast_stmt(CASE_STMT, stmt, NULL);
    STMT_CASE_INDEX(ret) = val;
    return ret;
}

static union node * default_stmt(union node *context)
{
    int in_sw = 0;
    union node *stmt;
    struct source src = source;
    
    expect(DEFAULT);
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = STMT_UP(context);
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'default' statement is not in a switch statement.");
    
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    else
        return ast_stmt(DEFAULT_STMT, stmt, NULL);
}

static union node * label_stmt(union node *context)
{
    union node *label;
    union node *stmt;
    
    label = ast_expr(REF_EXPR, ID, NULL, NULL);
    expect(ID);
    expect(':');
    stmt = statement(context);
    
    return ast_stmt(LABEL_STMT, label, stmt);
}

static union node * goto_stmt()
{
    union node *expr = NULL;
    
    expect(GOTO);
    if (token->id == ID)
        expr = ast_expr(REF_EXPR, ID, NULL, NULL);
    expect(ID);
    expect(';');
    
    return ast_stmt(GOTO_STMT, expr, NULL);
}

static union node * break_stmt(union node *context)
{
    int in_iter_sw = 0;
    struct source src = source;
    union node *ret;
    
    expect(BREAK);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context) || is_switch_stmt(context)) {
            in_iter_sw = 1;
            break;
        } else {
            context = STMT_UP(context);
        }
    }
    
    if (!in_iter_sw) {
        errorf(src, "'break' statement is not in a loop or switch statement.");
        return NULL;
    }
    
    ret = ast_stmt(BREAK_STMT, NULL, NULL);
    STMT_UP(ret) = context;
    
    return ret;
}

static union node * continue_stmt(union node *context)
{
    int in_iter = 0;
    union node *ret;
    struct source src = source;
    
    expect(CONTINUE);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context)) {
            in_iter = 1;
            break;
        } else {
            context = STMT_UP(context);
        }
    }
    
    if (!in_iter) {
        errorf(src, "'continue' statement is not in a loop statement.");
        return NULL;
    }
    
    ret = ast_stmt(CONTINUE_STMT, NULL, NULL);
    STMT_UP(ret) = context;
    
    return ret;
}

static union node * return_stmt()
{
    expect(RETURN);
    
    return ast_stmt(RETURN_STMT, expr_stmt(), NULL);;
}

static union node * statement(union node *context)
{
    switch (token->id) {
        case '{':       return _compound_stmt(context);
        case IF:        return if_stmt(context);
        case SWITCH:    return switch_stmt(context);
        case WHILE:     return while_stmt(context);
        case DO:        return do_while_stmt(context);
        case FOR:       return for_stmt(context);
        case GOTO:      return goto_stmt();
        case CONTINUE:  return continue_stmt(context);
        case BREAK:     return break_stmt(context);
        case RETURN:    return return_stmt();
        case CASE:      return case_stmt(context);
        case DEFAULT:   return default_stmt(context);
        case ID:
            if (lookahead()->id == ':')
                return label_stmt(context);
            // go through
        default:
            return expr_stmt();
    }
}

static union node * _compound_stmt(union node *context)
{
    union node *ret = ast_stmt(COMPOUND_STMT, NULL, NULL);
    struct vector *v = vec_new();
    
    expect('{');
    enter_scope();
    
    while (firstdecl(token) || firstexpr(token) || firststmt(token)) {
        if (firstdecl(token))
            // declaration
            vec_add_array(v, (void **)declaration());
        else
            // statement
            vec_push_safe(v, statement(context));
    }
    
    STMT_BLKS(ret) = (union node **)vtoa(v);
    expect('}');
    exit_scope();
    
    return ret;
}

union node * compound_stmt()
{
    return _compound_stmt(NULL);
}
