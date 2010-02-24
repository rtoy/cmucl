/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/motif/server/widgets.c,v 1.5 2007/10/19 09:57:22 cshapiro Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/Form.h>

#include "global.h"
#include "datatrans.h"
#include "types.h"
#include "tables.h"
#include "requests.h"

void RXtAppCreateShell(message_t message)
{
  Widget shell;
  ResourceList resources;

  resources.class = applicationShellWidgetClass;
  resources.parent = NULL;
  toolkit_read_value(message,&resources,ExtRResourceList);
  shell = XtAppCreateShell(global_app_name,global_app_class,
			   applicationShellWidgetClass, display,
			   resources.args,resources.length);

  reply_with_widget(message,shell);
}

void RXtRealizeWidget(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtRealizeWidget(widget);
}

void RXtUnrealizeWidget(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtUnrealizeWidget(widget);
}

void RXtMapWidget(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtMapWidget(widget);
}

void RXtUnmapWidget(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtUnmapWidget(widget);
}

void RXtSetSensitive(message_t message)
{
  Widget widget;
  Boolean sensitivity;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&sensitivity,XtRBoolean);
  XtSetSensitive(widget,sensitivity);
}

void RXtCreateWidget(message_t message)
{
  String name;
  WidgetClass class;
  Widget w,parent;
  ResourceList resources;

  toolkit_read_value(message,&name,XtRString);
  toolkit_read_value(message,&class,XtRWidgetClass);
  toolkit_read_value(message,&parent,XtRWidget);

  resources.class = class;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  w = XtCreateWidget(name,class,parent,
		     resources.args,resources.length);
  reply_with_widget(message,w);
}

void RXtCreateManagedWidget(message_t message)
{
  String name;
  WidgetClass class;
  Widget w,parent;
  ResourceList resources;

  toolkit_read_value(message,&name,XtRString);
  toolkit_read_value(message,&class,XtRWidgetClass);
  toolkit_read_value(message,&parent,XtRWidget);

  resources.class = class;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  w = XtCreateManagedWidget(name,class,parent,
			    resources.args,resources.length);
  reply_with_widget(message,w);
}

void RXtDestroyWidget(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtDestroyWidget(widget);
}

void RXtCreatePopupShell(message_t message)
{
  String name;
  WidgetClass class;
  Widget parent,widget;
  ResourceList resources;

  toolkit_read_value(message,&name,XtRString);
  toolkit_read_value(message,&class,XtRWidgetClass);
  toolkit_read_value(message,&parent,XtRWidget);

  resources.class = class;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  widget = XtCreatePopupShell(name,class,parent,
			      resources.args,resources.length);

  reply_with_widget(message,widget);
}

void RXtPopup(message_t message)
{
  Widget widget;
  XtGrabKind grab_kind;

  toolkit_read_value(message,&widget,XtRWidget);
  toolkit_read_value(message,&grab_kind,ExtRGrabKind);
  XtPopup(widget,grab_kind);
}

void RXtPopdown(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtPopdown(widget);
}

void RXtManageChild(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtManageChild(widget);
}

void RXtUnmanageChild(message_t message)
{
  Widget widget;

  toolkit_read_value(message,&widget,XtRWidget);
  XtUnmanageChild(widget);
}

void RXtManageChildren(message_t message)
{
  MyWidgetList list;

  toolkit_read_value(message,&list,XtRWidgetList);
  XtManageChildren(list.widgets,list.length);
}

void RXtUnmanageChildren(message_t message)
{
  MyWidgetList list;

  toolkit_read_value(message,&list,XtRWidgetList);
  XtUnmanageChildren(list.widgets,list.length);
}

void RXtIsManaged(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_boolean(message,XtIsManaged(w));
}

void RXtPopupSpringLoaded(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  XtPopupSpringLoaded(w);
}

void RXtIsRealized(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_boolean(message,XtIsRealized(w));
}

void RXtWindow(message_t message)
{
  Widget w;
  message_t reply=prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);
  message_write_xid(reply,XtWindow(w),window_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}

void RXtName(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_string(message,XtName(w));
}

void RXtIsSensitive(message_t message)
{
  Widget w;

  toolkit_read_value(message,&w,XtRWidget);
  reply_with_boolean(message,XtIsSensitive(w));
}

#define XT_IS_REQUEST(WHAT) \
void RXtIs##WHAT(message_t message) \
{\
    Widget w;\
    toolkit_read_value(message, &w, XtRWidget);\
    reply_with_boolean(message, XtIs##WHAT(w));\
   }

XT_IS_REQUEST(ApplicationShell)
XT_IS_REQUEST(Composite)
XT_IS_REQUEST(Constraint)
XT_IS_REQUEST(Object)
XT_IS_REQUEST(OverrideShell)
XT_IS_REQUEST(RectObj)
XT_IS_REQUEST(Shell)
XT_IS_REQUEST(TopLevelShell)
XT_IS_REQUEST(TransientShell)
XT_IS_REQUEST(VendorShell)
XT_IS_REQUEST(WMShell)

void RXtNameToWidget(message_t message)
{
   String name;
   Widget reference;

   toolkit_read_value(message,&reference,XtRWidget);
   toolkit_read_value(message,&name,XtRString);

   reply_with_widget(message, XtNameToWidget(reference, name));
   }

void RXtParent(message_t message)
{
   Widget widget;
   toolkit_read_value(message, &widget, XtRWidget);
   reply_with_widget(message, XtParent(widget));
   }

void RXtTranslateCoords(message_t message)
{
  Widget w;
  int x,y,root_x,root_y;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&w,XtRWidget);
  toolkit_read_value(message,&x,XtRInt);
  toolkit_read_value(message,&y,XtRInt);

  XtTranslateCoords(w,x,y,(Position*)&root_x,(Position*)&root_y);

  message_write_int(reply,root_x,int_tag);
  message_write_int(reply,root_y,int_tag);
  message_send(client_socket,reply);
  message_free(reply);

  must_confirm = False;
}



void RXmCreateMenuBar(message_t message)
{
  String name;
  Widget widget,parent;
  ResourceList resources;

  toolkit_read_value(message,&parent,XtRWidget);
  toolkit_read_value(message,&name,XtRString);

  resources.class = xmRowColumnWidgetClass;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  widget = XmCreateMenuBar(parent,name,resources.args,resources.length);
  reply_with_widget(message,widget);
}

void RXmCreateOptionMenu(message_t message)
{
  String name;
  Widget widget,parent;
  ResourceList resources;

  toolkit_read_value(message,&parent,XtRWidget);
  toolkit_read_value(message,&name,XtRString);

  resources.class = xmRowColumnWidgetClass;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  widget = XmCreateOptionMenu(parent,name,resources.args,resources.length);
  reply_with_widget(message,widget);
}

void RXmCreateRadioBox(message_t message)
{
  String name;
  Widget widget,parent;
  ResourceList resources;

  toolkit_read_value(message,&parent,XtRWidget);
  toolkit_read_value(message,&name,XtRString);

  resources.class = xmRowColumnWidgetClass;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  widget = XmCreateRadioBox(parent,name,resources.args,resources.length);
  reply_with_widget(message,widget);
}


void dialog_maker(message_t message,Widget (*buildfunc)(),WidgetClass class)
{
  String name;
  Widget this,shell,parent;
  ResourceList resources;

  toolkit_read_value(message,&parent,XtRWidget);
  toolkit_read_value(message,&name,XtRString);
  resources.class = class;
  resources.parent = parent;
  toolkit_read_value(message,&resources,ExtRResourceList);

  this = (*buildfunc)(parent,name,resources.args,resources.length);
  shell = XtParent(this);
  reply_with_widgets(message,this,shell);
}

void RXmCreateWarningDialog(message_t message)
{
  dialog_maker(message,XmCreateWarningDialog,xmMessageBoxWidgetClass);
}

void RXmCreateBulletinBoardDialog(message_t message)
{
  dialog_maker(message,XmCreateBulletinBoardDialog,xmBulletinBoardWidgetClass);
}

void RXmCreateErrorDialog(message_t message)
{
  dialog_maker(message,XmCreateErrorDialog,xmMessageBoxWidgetClass);
}

void RXmCreateFileSelectionDialog(message_t message)
{
  dialog_maker(message,XmCreateFileSelectionDialog,xmFileSelectionBoxWidgetClass);
}

void RXmCreateFormDialog(message_t message)
{
  dialog_maker(message,XmCreateFormDialog,xmFormWidgetClass);
}

void RXmCreateInformationDialog(message_t message)
{
  dialog_maker(message,XmCreateInformationDialog,xmMessageBoxWidgetClass);
}

void RXmCreateMessageDialog(message_t message)
{
  dialog_maker(message,XmCreateMessageDialog,xmMessageBoxWidgetClass);
}

void RXmCreatePopupMenu(message_t message)
{
  dialog_maker(message,XmCreatePopupMenu,xmRowColumnWidgetClass);
}

void RXmCreatePromptDialog(message_t message)
{
  dialog_maker(message,XmCreatePromptDialog,xmSelectionBoxWidgetClass);
}

void RXmCreatePulldownMenu(message_t message)
{
  dialog_maker(message,XmCreatePulldownMenu,xmRowColumnWidgetClass);
}

void RXmCreateQuestionDialog(message_t message)
{
  dialog_maker(message,XmCreateQuestionDialog,xmMessageBoxWidgetClass);
}

void RXmCreateScrolledList(message_t message)
{
  dialog_maker(message,XmCreateScrolledList,xmListWidgetClass);
}

void RXmCreateScrolledText(message_t message)
{
  dialog_maker(message,XmCreateScrolledText,xmTextWidgetClass);
}

void RXmCreateSelectionDialog(message_t message)
{
  dialog_maker(message,XmCreateSelectionDialog,xmSelectionBoxWidgetClass);
}

void RXmCreateWorkingDialog(message_t message)
{
  dialog_maker(message,XmCreateWorkingDialog,xmMessageBoxWidgetClass);
}



/* Some random functions */

void RXCreateFontCursor(message_t message)
{
  int shape;
  Cursor crs;
  message_t reply = prepare_reply(message);

  toolkit_read_value(message,&shape,XtRInt);
  crs = XCreateFontCursor(display,shape);

  message_write_xid(reply,crs,cursor_tag);
  message_send(client_socket,reply);
  message_free(reply);
  must_confirm=False;
}
