#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.h"
};

const char *nname(struct node * node)
{
    if (node == NULL)
	return "<NULL>";

    assert(node->id > BEGIN_NODE_ID && node->id < END_NODE_ID);
    
    return node_names[node->id];
}

const char * node_print_function(void *data)
{
    struct node *p = data;
    return nname(p);
}

struct node * concat_node(struct node *l, struct node *r)
{
    struct node *node = alloc_node_node();
    node->id = CONCAT_NODE;
    node->kids[0] = l;
    node->kids[1] = r;
    return node;
}

struct expr * expr_node(int id, int op, struct expr *l, struct expr *r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    struct expr * expr = alloc_expr_node();
    expr->node.id = id;
    expr->op = op;
    expr->node.kids[0] = NODE(l);
    expr->node.kids[1] = NODE(r);
    return expr;
}

struct stmt * stmt_node(int id, struct node *l, struct node *r)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    struct stmt * stmt = alloc_stmt_node();
    stmt->node.id = id;
    stmt->node.kids[0] = l;
    stmt->node.kids[1] = r;
    return stmt;
}

int is_iteration_stmt(struct stmt *stmt)
{
    if (!stmt)
	return 0;

    return stmt->node.id == FOR_STMT ||
	stmt->node.id == WHILE_STMT ||
	stmt->node.id == DO_WHILE_STMT;
}

int is_switch_stmt(struct stmt *stmt)
{
    if (!stmt)
	return 0;

    return stmt->node.id == SWITCH_STMT;
}

struct decl * decl_node(int id, int scope)
{
    assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    struct decl * decl = alloc_decl_node();
    decl->node.id = id;
    decl->scope = scope;
    return decl;
}
