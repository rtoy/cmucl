/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/motif/server/list.c,v 1.4 2007/10/19 09:57:22 cshapiro Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include <Xm/List.h>
#include <Xm/Command.h>
#include <Xm/SelectioB.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"


/* Functions for interacting with XmList widgets */

void find_items_resources(Widget w, String *items, String *count)
{
  if( XtClass(w) == xmListWidgetClass ) {
    *items = XmNitems;
    *count = XmNitemCount;
  }
  else if( XtClass(w) == xmSelectionBoxWidgetClass ) {
    *items = XmNlistItems;
    *count = XmNlistItemCount;
  }
  else if( XtClass(w) == xmCommandWidgetClass ) {
    *items = XmNhistoryItems;
    *count = XmNhistoryItemCount;
  }
  else
    *items = *count = NULL;
}

void RSetItems(message_t message)
{
  Widget w;
  StringTable items;
  String items_name,count_name;
  Arg args[2];

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);

  find_items_resources(w,&items_name,&count_name);
  if( !items_name ) {
    XtWarning("Invalid widget class in SET-ITEMS.  Ignoring request.");
    return;
  }

  XtSetArg(args[0], items_name, items.data);
  XtSetArg(args[1], count_name, items.length);
  XtSetValues(w, args, 2);
}

void RGetItems(message_t message)
{
  Widget w;
  StringTable items;
  String items_name,count_name;
  Arg args[2];
  message_t reply=prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);

  find_items_resources(w,&items_name,&count_name);
  if( !items_name ) {
    XtWarning("Invalid widget class in GET-ITEMS.  Ignoring request.");
    return;
  }

  XtSetArg(args[0], items_name, &items.data);
  XtSetArg(args[1], count_name, &items.length);
  XtGetValues(w, args, 2);

  message_write_xm_string_table(reply, &items, xm_string_table_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

void RXmListAddItem(message_t message)
{
  Widget w;
  XmString item;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItem(w,item,pos);
}

void RXmListAddItemUnselected(message_t message)
{
  Widget w;
  XmString item;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItemUnselected(w,item,pos);
}

void RXmListDeleteItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListDeleteItem(w,item);
}


void RXmListDeletePos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeletePos(w,pos);
}

void RXmListDeselectAllItems(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmListDeselectAllItems(w);
}

void RXmListDeselectItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListDeselectItem(w,item);
}

void RXmListDeselectPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeselectPos(w,pos);
}

void RXmListSelectItem(message_t message)
{
  Widget w;
  XmString item;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmListSelectItem(w,item,notify);
}

void RXmListSelectPos(message_t message)
{
  Widget w;
  int pos;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmListSelectPos(w,pos,notify);
}

void RXmListSetBottomItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListSetBottomItem(w,item);
}

void RXmListSetBottomPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetBottomPos(w,pos);
}

void RXmListSetHorizPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetHorizPos(w,pos);
}

void RXmListSetItem(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  XmListSetItem(w,item);
}

void RXmListSetPos(message_t message)
{
  Widget w;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&pos,XtRInt);
  XmListSetPos(w,pos);
}

void RXmListAddItems(message_t message)
{
  Widget w;
  StringTable items;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);
  toolkit_read_value(message,&pos,XtRInt);
  XmListAddItems(w,(XmString *)items.data,items.length,pos);
}

void RXmListDeleteAllItems(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmListDeleteAllItems(w);
}

void RXmListDeleteItems(message_t message)
{
  Widget w;
  StringTable items;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&items,XmRXmStringTable);
  XmListDeleteItems(w,(XmString *)items.data,items.length);
}

void RXmListDeleteItemsPos(message_t message)
{
  Widget w;
  int count,pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&count,XtRInt);
  toolkit_read_value(message,&pos,XtRInt);
  XmListDeleteItemsPos(w,count,pos);
}

void RXmListItemExists(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  reply_with_boolean(message,XmListItemExists(w,item));
}

void RXmListItemPos(message_t message)
{
  Widget w;
  XmString item;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&item,XmRXmString);
  reply_with_integer(message,XmListItemPos(w,item));
}

void RXmListReplaceItems(message_t message)
{
  Widget w;
  StringTable old,new;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&old,XmRXmStringTable);
  toolkit_read_value(message,&new,XmRXmStringTable);
  XmListReplaceItems(w,(XmString *)old.data,old.length,(XmString *)new.data);
}

void RXmListReplaceItemsPos(message_t message)
{
  Widget w;
  StringTable new;
  int pos;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&new,XmRXmStringTable);
  toolkit_read_value(message,&pos,XtRInt);
  XmListReplaceItemsPos(w,(XmString *)new.data,new.length,pos);
}

void RXmListSetAddMode(message_t message)
{
  Widget w;
  Boolean state;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&state,XtRBoolean);
  XmListSetAddMode(w,state);
}

void RXmListGetSelectedPos(message_t message)
{
  Widget w;
  IntList pos;
  Boolean result;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);

  result = XmListGetSelectedPos(w,&pos.data,&pos.length);
  if( !result )
    pos.length = 0;

  message_write_int_list(reply,&pos,int_list_tag);
  message_write_boolean(reply,result,boolean_tag);

  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
  if( result )
    register_garbage(pos.data,GarbageData);
}
