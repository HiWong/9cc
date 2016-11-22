#ifndef LIST_H
#define LIST_H

struct list {
    void *x;
    struct list *link;
};

extern struct list *append(struct list *list, void *x);

extern size_t listlen(struct list *list);

extern void *ltoa(struct list **list, unsigned int area);

#endif
