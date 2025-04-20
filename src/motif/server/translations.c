/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/motif/server/translations.c,v 1.3 2007/10/19 09:57:22 cshapiro Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"


extern int end_callback_loop;

void LispActionProc(Widget w,XEvent *event,String *params,Cardinal *num_params)
{
  message_t reply;
  int exit_value;

  if( *num_params == 0 )
    XtAppError(app_context,"LispActionProc:  Lisp function not specified.");
  else if( *num_params > 1 )
    XtAppWarning(app_context,"LispActionProc:  ignoring extra arguments.");

  reply = message_new(next_serial++);
  message_add_packet(reply);
  message_put_dblword(reply,ACTION_REPLY);
  message_write_widget(reply,w,widget_tag);
  message_write_int(reply,event->type,int_tag);
  message_write_string(reply,params[0],string_tag);

  message_send(client_socket,reply);
  message_free(reply);

  exit_value = end_callback_loop++;
  while( exit_value<end_callback_loop )
    XtAppProcessEvent(app_context,XtIMAlternateInput);
}



void RXtParseTranslationTable(message_t message)
{
  String table;
  XtTranslations t;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&table,XtRString);
  t = XtParseTranslationTable(table);
  message_write_translation_table(reply,t,translation_table_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

void RXtAugmentTranslations(message_t message)
{
  Widget w;
  XtTranslations t;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&t,XtRTranslationTable);

  XtAugmentTranslations(w,t);
}

void RXtOverrideTranslations(message_t message)
{
  Widget w;
  XtTranslations t;

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&t,XtRTranslationTable);

  XtOverrideTranslations(w,t);
}

void RXtUninstallTranslations(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XtUninstallTranslations(w);
}

void RXtParseAcceleratorTable(message_t message)
{
  String table;
  XtAccelerators accel;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&table,XtRString);
  accel = XtParseAcceleratorTable(table);
  message_write_accelerator_table(reply,accel,accelerator_table_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm = False;
}

void RXtInstallAccelerators(message_t message)
{
  Widget dest,src;

  toolkit_read_value(message,&dest,XtRWidget);
  toolkit_read_value(message,&src,XtRWidget);
  XtInstallAccelerators(dest,src);
}

void RXtInstallAllAccelerators(message_t message)
{
  Widget dest,src;

  toolkit_read_value(message,&dest,XtRWidget);
  toolkit_read_value(message,&src,XtRWidget);
  XtInstallAllAccelerators(dest,src);
}
