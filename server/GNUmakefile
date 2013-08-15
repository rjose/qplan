include $(GNUSTEP_MAKEFILES)/common.make

TOOL_NAME = qplan
qplan_C_FILES = qplan.c tcp.c context.c web.c \
		websockets/base64.c websockets/handshake.c \
		websockets/frames.c websockets/read_message.c websockets/util.c
qplan_LDFLAGS = -llua52 -lpthread -lreadline
qplan_CFLAGS = -I/usr/local/include/lua52
ADDITIONAL_TOOL_LIBS = -lssl -lcrypto -lm

include $(GNUSTEP_MAKEFILES)/tool.make
