/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/motif/server/motif.c,v 1.5 2007/10/19 09:57:22 cshapiro Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/Command.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/SelectioB.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/ScrollBar.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"

void RXmUpdateDisplay(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmUpdateDisplay(w);
}

void RXmIsMotifWMRunning(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_boolean(message,XmIsMotifWMRunning(w));
}

void RXmScrolledWindowSetAreas(message_t message)
{
  Widget w;
  Widget hscroll,vscroll,work_region;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&hscroll,XtRWidget);
  toolkit_read_value(message,&vscroll,XtRWidget);
  toolkit_read_value(message,&work_region,XtRWidget);
  XmScrolledWindowSetAreas(w,hscroll,vscroll,work_region);
}

void RXmTrackingLocate(message_t message)
{
  Widget w;
  Cursor cursor;
  Boolean confine_to;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&cursor,XtRCursor);
  toolkit_read_value(message,&confine_to,XtRBoolean);
  XmTrackingLocate(w,cursor,confine_to);
}

void RXmMenuPosition(message_t message)
{
  Widget w;
  XButtonPressedEvent *event;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&event,ExtREvent);

  XmMenuPosition(w,event);
}



/* Functions for accessing XmCommand widgets */

void RXmCommandAppendValue(message_t message)
{
  Widget widget;
  XmString command;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&command,XmRXmString);
  XmCommandAppendValue(widget,command);
}

void RXmCommandError(message_t message)
{
  Widget widget;
  XmString error;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&error,XmRXmString);
  XmCommandError(widget,error);
}

void RXmCommandSetValue(message_t message)
{
  Widget w;
  XmString value;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&value,XmRXmString);
  XmCommandSetValue(w,value);
}



/* Functions for dealing with XmScale widgets */

void RXmScaleGetValue(message_t message)
{
  Widget widget;
  int value;

  toolkit_read_value(message,&widget,XtRWidget);
  XmScaleGetValue(widget,&value);
  reply_with_integer(message,value);
}

void RXmScaleSetValue(message_t message)
{
  Widget widget;
  int value;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&value,XtRInt);
  XmScaleSetValue(widget,value);
}



/* Functions for dealing with XmToggleButton widgets */

void RXmToggleButtonGetState(message_t message)
{
  Widget w;
  int state;

  toolkit_read_value(message,&w,XtRWidget);
  state = XmToggleButtonGetState(w);
  reply_with_boolean(message,state);
}

void RXmToggleButtonSetState(message_t message)
{
  Widget w;
  Boolean state,notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&state,XtRBoolean);
  toolkit_read_value(message,&notify,XtRBoolean);
  XmToggleButtonSetState(w,state,notify);
}



/* Functions for using Tab groups */

void RXmAddTabGroup(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmAddTabGroup(w);
}

void RXmRemoveTabGroup(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XmRemoveTabGroup(w);
}

void RXmProcessTraversal(message_t message)
{
  Widget w;
  int direction;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&direction,XtRInt);
  reply_with_boolean(message,XmProcessTraversal(w,direction));
}



/* Functions for getting children from Command/[File]SelectionBox/MessageBox */

#define DEFINE_CHILD_QUERY(query_func)                 \
  Widget w;                                            \
  int which_child;                                     \
                                                       \
  toolkit_read_value(message,&w,XtRWidget);            \
  toolkit_read_value(message,&which_child,XtREnum);    \
  reply_with_widget(message,query_func(w,which_child))

void RXmMessageBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmMessageBoxGetChild);
}

void RXmSelectionBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmSelectionBoxGetChild);
}

void RXmFileSelectionBoxGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmFileSelectionBoxGetChild);
}

void RXmCommandGetChild(message_t message)
{
  DEFINE_CHILD_QUERY(XmCommandGetChild);
}



/* Functions for getting/setting values of XmScrollBar widgets */

void RXmScrollBarGetValues(message_t message)
{
  Widget widget;
  int value,slider_size,increment,page_increment;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&widget,XtRWidget);

  XmScrollBarGetValues(widget,&value,&slider_size,&increment,&page_increment);

  message_write_int(reply,value,int_tag);
  message_write_int(reply,slider_size,int_tag);
  message_write_int(reply,increment,int_tag);
  message_write_int(reply,page_increment,int_tag);

  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

void RXmScrollBarSetValues(message_t message)
{
  Widget w;
  int value,slider_size,increment,page_increment;
  Boolean notify;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&value,XtRInt);
  toolkit_read_value(message,&slider_size,XtRInt);
  toolkit_read_value(message,&increment,XtRInt);
  toolkit_read_value(message,&page_increment,XtRInt);
  toolkit_read_value(message,&notify,XtRBoolean);

  XmScrollBarSetValues(w,value,slider_size,increment,page_increment,notify);
}
