#include <assert.h>
#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///

static void parse_initializer_list(struct desig *, struct list **);

static struct desig *next_designator1(struct desig *desig, bool initial)
{
    assert(desig);
    
    switch (desig->id) {
    case DESIG_FIELD:
        {
            struct desig *prev = desig->prev;

            assert(prev);
            assert(isrecord(prev->type));

            struct field **fields = TYPE_FIELDS(prev->type);
            size_t len = length(fields);
            int idx = indexof_field(prev->type, desig->u.field);
            assert(idx >= 0);
            if (idx < len - 1) {
                struct field *field = fields[idx+1];
                struct desig *d = new_desig_field(field, source);
                d->offset = prev->offset + field->offset;
                d->prev = copy_desig(prev);
                return d;
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
                return d;
            } else {
                return next_designator1(prev, false);
            }
        }
        break;

    case DESIG_NONE:
        assert(desig->prev == NULL);
        if (!initial) {
            error("excess elements in %s initializer", TYPE_NAME(desig->type));
            return NULL;
        }
        if (isrecord(desig->type)) {
            struct field *field = TYPE_FIELDS(desig->type)[0];
            struct desig *d = new_desig_field(field, source);
            d->offset = desig->offset + field->offset;
            d->prev = copy_desig(desig);
            return d;
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
    
    CC_UNAVAILABLE
}

static struct desig *next_designator(struct desig *desig)
{
    if (!desig)
        return NULL;

    return next_designator1(desig, true);
}

static struct desig *parse_designator(struct desig *desig)
{
    struct list *list = NULL;
    
    assert(token->id == '.' || token->id == '[');
    
    do {
        if (token->id == '.') {
            expect('.');
            struct source src = source;
            // only create list item when desig != NULL
            if (desig) {
                if (token->id == ID)
                    list = list_append(list, new_desig_name(TOK_ID_STR(token), src));
                else
                    // create a desig with name of NULL
                    list = list_append(list, new_desig_name(NULL, src));
            }
            expect(ID);
        } else {
            expect('[');
            struct source src = source;
            long index = intexpr();
            match(']', skip_to_squarebracket);
            // only create list item when desig != NULL
            if (desig)
                list = list_append(list, new_desig_index(index, src));
        }
    } while (token->id == '.' || token->id == '[');

    expect('=');

    return desig ? actions.designator(desig, ltoa(&list, FUNC)) : NULL;
}

static void parse_initializer(struct desig **pdesig, struct list **plist)
{
    if (token->id == '{') {
        // begin a new root designator
        struct desig *desig = *pdesig;
        struct desig *d;

        if (desig->id == DESIG_NONE) {
            d = desig;
            d->braces++;
        } else {
            d = new_desig(DESIG_NONE);
            d->type = desig->type;
            d->offset = desig->offset;
            d->src = desig->src;
            d->all = desig;         // all link
        }
        
        parse_initializer_list(d, plist);
    } else {
        actions.element_init(pdesig, assign_expr(), plist);
    }
}

static void parse_initializer_list(struct desig *desig, struct list **plist)
{
    struct desig *d = desig;
    
    expect('{');
    
    for (;;) {
        if (token->id == '.' || token->id == '[')
            d = parse_designator(desig);
        else
            d = next_designator(d);

        parse_initializer(&d, plist);

        if (token->id != ',')
            break;

        expect(',');

        if (token->id == '}')
            break;
    }

    match('}', skip_to_brace);
}

/// initializer:
///   assignment-expression
///   '{' initializer-list '}'
///   '{' initializer-list ',' '}'
///
struct expr *initializer(struct type * ty)
{
    if (token->id == '{')
        return initializer_list(ty);
    else
        return assign_expr();
}

/// initializer-list:
///   designation[opt] initializer
///   initializer-list ',' designation[opt] initializer
///
/// designation:
///   designator-list '='
///
/// designator-list:
///   designator
///   designator-list designator
///
/// designator:
///   '[' constant-expression ']'
///   '.' identifier
///
struct expr *initializer_list(struct type * ty)
{
    if (ty) {
        struct desig desig = {.id = DESIG_NONE, .type = ty, .offset = 0, .src = source};
        struct list *list = NULL;

        parse_initializer_list(&desig, &list);        

        return actions.initializer_list(ty, ltoa(&list, FUNC));
    } else {
        parse_initializer_list(NULL, NULL);
        return NULL;
    }
}
