/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/parse.c,v 1.2 1990/03/08 17:28:21 wlott Exp $ */
#include <stdio.h>

#include "ldb.h"
#include "lisp.h"
#include "vars.h"
#include "parse.h"


static void skip_ws(ptr)
char **ptr;
{
    while (**ptr <= ' ' && **ptr != '\0')
        (*ptr)++;
}

static boolean string_to_long(token, value)
char *token;
long *value;
{
    int base, digit;
    long num;
    char *ptr;

    if (token == 0)
        return FALSE;

    if (token[0] == '0')
        if (token[1] == 'x') {
            base = 16;
            token += 2;
        }
        else {
            base = 8;
            token++;
        }
    else if (token[0] == '#') {
        switch (token[1]) {
            case 'x':
            case 'X':
                base = 16;
                token += 2;
                break;            
            case 'o':
            case 'O':
                base = 8;
                token += 2;
                break;
            default:
                return FALSE;
        }
    }
    else
        base = 10;

    num = 0;
    ptr = token;
    while (*ptr != '\0') {
        if (*ptr >= 'a' && *ptr <= 'f')
            digit = *ptr + 10 - 'a';
        else if (*ptr >= 'A' && *ptr <= 'F')
            digit = *ptr + 10 - 'A';
        else if (*ptr >= '0' && *ptr <= '9')
            digit = *ptr - '0';
        else
            return FALSE;
        if (digit < 0 || digit >= base)
            return FALSE;

        ptr++;
        num = num * base + digit;
    }

    *value = num;
    return TRUE;
}

static boolean lookup_variable(name, result)
char *name;
lispobj *result;
{
    struct var *var = lookup_by_name(name);

    if (var == NULL)
        return FALSE;
    else {
        *result = var_value(var);
        return TRUE;
    }
}


boolean more_p(ptr)
char **ptr;
{
    skip_ws(ptr);

    if (**ptr == '\0')
        return FALSE;
    else
        return TRUE;
}

char *parse_token(ptr)
char **ptr;
{
    char *token;

    skip_ws(ptr);
    
    if (**ptr == '\0')
        return NULL;

    token = *ptr;

    while (**ptr > ' ')
        (*ptr)++;

    if (**ptr != '\0') {
        **ptr = '\0';
        (*ptr)++;
    }

    return token;
}

#if 0
static boolean number_p(token)
char *token;
{
    char *okay;

    if (token == NULL)
        return FALSE;

    okay = "abcdefABCDEF987654321d0";

    if (token[0] == '0')
        if (token[1] == 'x' || token[1] == 'X')
            token += 2;
        else {
            token++;
            okay += 14;
        }
    else if (token[0] == '#') {
        switch (token[1]) {
            case 'x':
            case 'X':
                break;
            case 'o':
            case 'O':
                okay += 14;
                break;
            default:
                return FALSE;
        }
    }
    else
        okay += 12;

    while (*token != '\0')
        if (index(okay, *token++) == NULL)
            return FALSE;
    return TRUE;
}
#endif

long parse_number(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long result;

    if (token == NULL) {
        printf("Expected a number.\n");
        throw_to_monitor();
    }
    else if (string_to_long(token, &result))
        return result;
    else {
        printf("Invalid number: ``%s''\n", token);
        throw_to_monitor();
    }
}

char *parse_addr(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long result;

    if (token == NULL) {
        printf("Expected an address.\n");
        throw_to_monitor();
    }
    else if (token[0] == '$') {
        if (!lookup_variable(token+1, (lispobj *)&result)) {
            printf("Unknown variable: ``%s''\n", token);
            throw_to_monitor();
        }
        result &= ~7;
    }
    else {
        if (!string_to_long(token, &result)) {
            printf("Invalid number: ``%s''\n", token);
            throw_to_monitor();
        }
        result &= ~3;
    }

    if (!valid_addr(result)) {
        printf("Invalid address: 0x%x\n", result);
        throw_to_monitor();
    }

    return (char *)result;
}

static boolean lookup_symbol(name, result)
char *name;
lispobj *result;
{
    int count;

    /* Search read only space */
    *result = 0x20000000;
    count = 1024;
    if (search_for_symbol(name, result, &count)) {
        *result |= type_OtherPointer;
        return TRUE;
    }

    /* Search static space */
    *result = 0x30000000;
    count = 1024;
    if (search_for_symbol(name, result, &count)) {
        *result |= type_OtherPointer;
        return TRUE;
    }

    /* Search dynamic space */
    *result = 0x40000000;
    count = (SymbolValue(SAVED_ALLOCATION_POINTER) - 0x40000000) / 4;
    if (search_for_symbol(name, result, &count)) {
        *result |= type_OtherPointer;
        return TRUE;
    }

    return FALSE;
}

lispobj parse_lispobj(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long pointer;
    lispobj result;

    if (token == NULL) {
        printf("Expected an object.\n");
        throw_to_monitor();
    }
    else if (token[0] == '$') {
        if (!lookup_variable(token+1, &result)) {
            printf("Unknown variable: ``%s''\n", token);
            throw_to_monitor();
        }
    }
    else if (token[0] == '@') {
        if (string_to_long(token+1, &pointer)) {
            pointer &= ~3;
            if (valid_addr(pointer))
                result = *(lispobj *)pointer;
            else {
                printf("Invalid address: ``%s''\n", token+1);
                throw_to_monitor();
            }
        }
        else {
            printf("Invalid address: ``%s''\n", token+1);
            throw_to_monitor();
        }
    }
    else if (string_to_long(token, (long *)&result))
        ;
    else if (lookup_symbol(token, &result))
        ;
    else {
        printf("Invalid lisp object: ``%s''\n", token);
        throw_to_monitor();
    }

    return result;
}
