
#include "lisp.h"
#include "ldb.h"

boolean search_for_type(type, start, count)
int type;
lispobj **start;
int *count;
{
    lispobj obj, *addr;

    while ((*count == -1 || (*count > 0)) && valid_addr(*start)) {
        obj = **start;
        addr = *start;
        if (*count != -1)
            *count -= 2;

        if (TypeOf(obj) == type)
            return TRUE;

        (*start) += 2;
    }
    return FALSE;
}


boolean search_for_symbol(name, start, count)
char *name;
lispobj **start;
int *count;
{
    struct symbol *symbol;
    struct vector *symbol_name;

    while (search_for_type(type_SymbolHeader, start, count)) {
        symbol = (struct symbol *)PTR((lispobj)*start);
        symbol_name = (struct vector *)PTR(symbol->name);
        if (valid_addr(symbol_name) && TypeOf(symbol_name->header) == type_SimpleString && strcmp((char *)symbol_name->data, name) == 0)
            return TRUE;
        (*start) += 2;
    }
    return FALSE;
}
