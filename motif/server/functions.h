/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/motif/server/functions.h,v 1.4 2007/10/19 09:57:22 cshapiro Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

/* In requests.c */
extern void QuitServer(message_t);

/* in callbacks.c */
extern void TerminateCallback(message_t);
extern void RXtAddCallback(message_t);
extern void RXtRemoveCallback(message_t);
extern void RXmAddProtocolCallback(message_t);
extern void RXmRemoveProtocolCallback(message_t);
extern void RReturnTextCallbackDoit(message_t);

/* in events.c */
extern void RTransportEvent(message_t);
extern void RXtAddEventHandler(message_t);
extern void RXtRemoveEventHandler(message_t);

/* in widgets.c */
extern void RXtAppCreateShell(message_t);
extern void RXtCreateManagedWidget(message_t);
extern void RXtCreateWidget(message_t);
extern void RXtDestroyWidget(message_t);
extern void RXtRealizeWidget(message_t);
extern void RXtUnrealizeWidget(message_t);
extern void RXtMapWidget(message_t);
extern void RXtUnmapWidget(message_t);
extern void RXtSetSensitive(message_t);
extern void RXtCreatePopupShell(message_t);
extern void RXtPopup(message_t);
extern void RXtManageChild(message_t);
extern void RXtUnmanageChild(message_t);
extern void RXtManageChildren(message_t);
extern void RXtUnmanageChildren(message_t);
extern void RXtPopdown(message_t);
extern void RXtIsManaged(message_t);
extern void RXtPopupSpringLoaded(message_t);
extern void RXtIsRealized(message_t);
extern void RXtWindow(message_t);
extern void RXtName(message_t);
extern void RXtIsSensitive(message_t);
extern void RXtIsApplicationShell(message_t);
extern void RXtIsComposite(message_t);
extern void RXtIsConstraint(message_t);
extern void RXtIsObject(message_t);
extern void RXtIsOverrideShell(message_t);
extern void RXtIsRectObj(message_t);
extern void RXtIsShell(message_t);
extern void RXtIsTopLevelShell(message_t);
extern void RXtIsTransientShell(message_t);
extern void RXtIsVendorShell(message_t);
extern void RXtIsWMShell(message_t);
extern void RXtNameToWidget(message_t);
extern void RXtParent(message_t);
extern void RXtTranslateCoords(message_t);
extern void RXmCreateMenuBar(message_t);
extern void RXmCreateOptionMenu(message_t);
extern void RXmCreateRadioBox(message_t);
extern void RXmCreateWarningDialog(message_t);
extern void RXmCreateBulletinBoardDialog(message_t);
extern void RXmCreateErrorDialog(message_t);
extern void RXmCreateFileSelectionDialog(message_t);
extern void RXmCreateFormDialog(message_t);
extern void RXmCreateInformationDialog(message_t);
extern void RXmCreateMessageDialog(message_t);
extern void RXmCreatePopupMenu(message_t);
extern void RXmCreatePromptDialog(message_t);
extern void RXmCreatePulldownMenu(message_t);
extern void RXmCreateQuestionDialog(message_t);
extern void RXmCreateScrolledList(message_t);
extern void RXmCreateScrolledText(message_t);
extern void RXmCreateSelectionDialog(message_t);
extern void RXmCreateWorkingDialog(message_t);
extern void RXCreateFontCursor(message_t);

/* In resources.c */
extern void RXtSetValues(message_t);
extern void RXtGetValues(message_t);

/* In motif.c */
extern void RXmUpdateDisplay(message_t);
extern void RXmIsMotifWMRunning(message_t);
extern void RXmCommandAppendValue(message_t);
extern void RXmCommandError(message_t);
extern void RXmCommandSetValue(message_t);
extern void RXmScaleGetValue(message_t);
extern void RXmScaleSetValue(message_t);
extern void RXmToggleButtonGetState(message_t);
extern void RXmToggleButtonSetState(message_t);
extern void RXmAddTabGroup(message_t);
extern void RXmRemoveTabGroup(message_t);
extern void RXmProcessTraversal(message_t);
extern void RXmMessageBoxGetChild(message_t);
extern void RXmSelectionBoxGetChild(message_t);
extern void RXmFileSelectionBoxGetChild(message_t);
extern void RXmCommandGetChild(message_t);
extern void RXmScrolledWindowSetAreas(message_t);
extern void RXmTrackingLocate(message_t);
extern void RXmMenuPosition(message_t);
extern void RXmScrollBarGetValues(message_t);
extern void RXmScrollBarSetValues(message_t);

/* In translations.c */
extern void RXtParseTranslationTable(message_t);
extern void RXtAugmentTranslations(message_t);
extern void RXtOverrideTranslations(message_t);
extern void RXtUninstallTranslations(message_t);
extern void RXtParseAcceleratorTable(message_t);
extern void RXtInstallAccelerators(message_t);
extern void RXtInstallAllAccelerators(message_t);

/* In text.c */
extern void RXmTextClearSelection(message_t);
extern void RXmTextCopy(message_t);
extern void RXmTextCut(message_t);
extern void RXmTextGetBaseline(message_t);
extern void RXmTextGetEditable(message_t);
extern void RXmTextGetInsertionPosition(message_t);
extern void RXmTextGetLastPosition(message_t);
extern void RXmTextGetMaxLength(message_t);
extern void RXmTextGetSelection(message_t);
extern void RXmTextGetSelectionPosition(message_t);
extern void RXmTextGetString(message_t);
extern void RXmTextGetTopCharacter(message_t);
extern void RXmTextInsert(message_t);
extern void RXmTextPaste(message_t);
extern void RXmTextPosToXY(message_t);
extern void RXmTextRemove(message_t);
extern void RXmTextReplace(message_t);
extern void RXmTextScroll(message_t);
extern void RXmTextSetAddMode(message_t);
extern void RXmTextSetEditable(message_t);
extern void RXmTextSetHighlight(message_t);
extern void RXmTextSetInsertionPosition(message_t);
extern void RXmTextSetMaxLength(message_t);
extern void RXmTextSetSelection(message_t);
extern void RXmTextSetString(message_t);
extern void RXmTextSetTopCharacter(message_t);
extern void RXmTextShowPosition(message_t);
extern void RXmTextXYToPos(message_t);

/* In xmstring.c */
extern void RXmFontListAdd(message_t);
extern void RXmFontListCreate(message_t);
extern void RXmFontListFree(message_t);
extern void RXmStringBaseline(message_t);
extern void RXmStringByteCompare(message_t);
extern void RXmStringCompare(message_t);
extern void RXmStringConcat(message_t);
extern void RXmStringCopy(message_t);
extern void RXmStringCreate(message_t);
extern void RXmStringCreateLtoR(message_t);
extern void RXmStringGetLtoR(message_t);
extern void RXmStringCreateSimple(message_t);
extern void RXmStringEmpty(message_t);
extern void RXmStringExtent(message_t);
extern void RXmStringFree(message_t);
extern void RXmStringHasSubstring(message_t);
extern void RXmStringHeight(message_t);
extern void RXmStringLength(message_t);
extern void RXmStringLineCount(message_t);
extern void RXmStringNConcat(message_t);
extern void RXmStringNCopy(message_t);
extern void RXmStringSeparatorCreate(message_t);
extern void RXmStringWidth(message_t);

/* In list.c */
extern void RSetItems(message_t);
extern void RGetItems(message_t);
extern void RXmListAddItem(message_t);
extern void RXmListAddItemUnselected(message_t);
extern void RXmListDeleteItem(message_t);
extern void RXmListDeletePos(message_t);
extern void RXmListDeselectAllItems(message_t);
extern void RXmListDeselectItem(message_t);
extern void RXmListDeselectPos(message_t);
extern void RXmListSelectItem(message_t);
extern void RXmListSelectPos(message_t);
extern void RXmListSetBottomItem(message_t);
extern void RXmListSetBottomPos(message_t);
extern void RXmListSetHorizPos(message_t);
extern void RXmListSetItem(message_t);
extern void RXmListSetPos(message_t);
extern void RXmListAddItems(message_t);
extern void RXmListDeleteAllItems(message_t);
extern void RXmListDeleteItems(message_t);
extern void RXmListDeleteItemsPos(message_t);
extern void RXmListItemExists(message_t);
extern void RXmListItemPos(message_t);
extern void RXmListReplaceItems(message_t);
extern void RXmListReplaceItemsPos(message_t);
extern void RXmListSetAddMode(message_t);
extern void RXmListGetSelectedPos(message_t);
