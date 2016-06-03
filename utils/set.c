#include "utils.h"

static char value;

struct set *set_new(void)
{
    struct set *set = zmalloc(sizeof(struct set));
    set->map = map_newf(NULL);
    return set;
}

struct set *set_new1(const void *element)
{
    struct set *set = set_new();
    set_add(set, element);
    return set;
}

void set_free(struct set *set)
{
    map_free(set->map);
    free(set);
}

struct set *set_copy(struct set *set)
{
    struct set *s = set_new();
    if (set) {
        for (struct map_entry *entry = set->map->all; entry; entry = entry->all)
            set_add(s, entry->key);
    }
    return s;
}

void set_add(struct set *set, const void *element)
{
    assert(element);
    map_put(set->map, element, &value);
}

void set_remove(struct set *set, const void *element)
{
    assert(element);
    map_put(set->map, element, NULL);
}

struct set *set_substract(struct set *set1, struct set *set2)
{
    if (!set1) {
        return set_new();
    } else if (!set2) {
        return set_copy(set1);
    } else {
        struct set *set = set_new();
        for (struct map_entry *entry = set1->map->all; entry; entry = entry->all) {
            if (!set_has(set2, entry->key))
                set_add(set, entry->key);
        }
        return set;
    }
}

bool set_has(struct set *set, const void *element)
{
    assert(element);
    return set ? map_get(set->map, element) != NULL : false;
}

struct set *set_union(struct set *set1, struct set *set2)
{
    struct set *set = set_new();
    if (set1) {
        for (struct map_entry *entry = set1->map->all; entry; entry = entry->all)
            set_add(set, entry->key);
    }
    if (set2) {
        for (struct map_entry *entry = set2->map->all; entry; entry = entry->all)
            set_add(set, entry->key);
    }
    return set;
}

struct set *set_intersection(struct set *set1, struct set *set2)
{
    if (!set1 || !set2) {
        return set_new();
    } else {
        struct set *set = set_new();
        for (struct map_entry *entry = set1->map->all; entry; entry = entry->all) {
            if (set_has(set2, entry->key))
                set_add(set, entry->key);
        }
        return set;
    }
}

struct vector *set_objects(struct set *set)
{
    return set ? map_keys(set->map) : NULL;
}

size_t set_size(struct set *set)
{
    return set ? set->map->size : 0;
}

bool set_equal(struct set *set1, struct set *set2)
{
    if (set_size(set1) != set_size(set2))
        return false;
    if (!set1 || !set2)
        return true;
    for (struct map_entry *entry = set1->map->all; entry; entry = entry->all) {
        if (!set_has(set2, entry->key))
            return false;
    }
    return true;
}

bool set_empty(struct set *set)
{
    return set_size(set) == 0;
}

void set_clear(struct set *set)
{
    map_free(set->map);
    set->map = map_newf(NULL);
}
