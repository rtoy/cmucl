CC = cc
#CC = gcc

#
# Places to look for X and Motif headers
XHDR  = -I/usr/misc/.X11/include
XMHDR = -I/usr/misc/.motif/include

#
# Places to look for X and Motif libraries
XLIB  = -L/usr/misc/.X11/lib
XMLIB = -L/usr/misc/.motif/lib

#
# -G 0 option is for MIPS only
CFLAGS = -O -G 0 $(XHDR) $(XMHDR)
LDFLAGS = -G 0 $(XLIB) $(XMLIB)
LIBS = -lXm -lXt -lX11
OBJS = main.o server.o translations.o packet.o message.o datatrans.o \
       requests.o callbacks.o widgets.o resources.o tables.o motif.o \
       text.o xmstring.o list.o events.o
TARGET = motifd


$(TARGET) : $(OBJS)
	$(CC) -o $(TARGET) $(LDFLAGS) $(OBJS) $(LIBS)
	strip $(TARGET)

tables.o : tables.c StringTable.h ClassTable.h TypeTable.h
	$(CC) $(CFLAGS) -c tables.c

requests.o : requests.c Interface.h
	$(CC) $(CFLAGS) -c requests.c
