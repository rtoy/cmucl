#include <stdio.h>
#include <X11/Intrinsic.h>

#include "global.h"
#include "oid.h"

/* OID generation. */
static unsigned int current_oid;

static void init_oids()
{
  current_oid = (unsigned int)0;
}

static unsigned int next_oid()
{
  if (current_oid == 0xffffffff) 
    fatal_error("next_oid: ran out of oids"); /* This should be smarter.*/
  return (++current_oid);
}

/* For expediency of implementation, the table of pointer-to-OID
   associations will be a doubly-linked list. */
typedef struct oblist oblist_t;

struct oblist {
  oblist_t *next;
  oblist_t *prev;
  void *obj;
  int oid;
};

static oblist_t *oblist = NULL;

static oblist_t *find_node_if(int(*fn)(oblist_t*,void*), void *datum)
{
  oblist_t *ret = NULL;
  oblist_t *node = oblist;
  while (node != NULL) {
    if (fn(node, datum)==1) {
      ret = node;
      break;
    }
    node = node->next;
  }
  return ret;
}

static int obj_eql(oblist_t *node, void *obj) {
  return (node->obj == obj);
}

static int id_eql(oblist_t *node, void *oidp) {
  return (node->oid == *(int*)oidp);
}

void *find_object(unsigned int oid)
{
  oblist_t *node = find_node_if(&id_eql, &oid);
  if (node != NULL)
    return node->obj;
  else
    return NULL;
}

unsigned int intern_object(void *obj)
{
  oblist_t *node = find_node_if(&obj_eql, obj);

  if (node != NULL)
    return node->oid;

  if (oblist == NULL)
    init_oids();

  if ((node = (void*)XtCalloc(1, sizeof(oblist_t))) == NULL)
    fatal_error("intern_object: out of memory");

  node->next = oblist;
  node->prev = NULL;
  if (node->next) node->next->prev = node;
  node->obj = obj;
  node->oid = next_oid();
  oblist = node;
  return node->oid;
}

static int unintern_if(int(*fn)(oblist_t*,void*), void *datum) 
{
  oblist_t *node;

  node = find_node_if(fn,datum);
  if (node == NULL)
    return 0;
  
  if (node == oblist) oblist = node->next;
  if (node->prev) (node->prev)->next = node->next;
  if (node->next) (node->next)->prev = node->prev;

  XtFree((char*)node);
  return 1;
}


void unintern_object(void * obj)
{
  if (unintern_if(&obj_eql, obj)==0) {
    if( global_will_trace ) {
      printf("unintern_object: couldn't unintern %p\n",obj);
      fflush(stdout);}
  }
}

void maybe_unintern_object(void * obj)
{
  unintern_if(&obj_eql, obj);
}
