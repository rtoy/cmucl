extern message_t prepare_reply(message_t message);
extern void reply_with_widget(message_t,Widget);
extern void reply_with_widgets(message_t,Widget,Widget);
extern void reply_with_integer(message_t,long);
extern void reply_with_boolean(message_t,Boolean);
extern void reply_with_font_list(message_t,XmFontList);
extern void reply_with_xmstring(message_t,XmString);
extern void reply_with_string(message_t,String);
