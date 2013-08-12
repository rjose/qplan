#ifndef WEB_H
#define WEB_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "qplan_context.h"
#include "tcp_io.h"

/* ----------------------------------------------------------------------------
 * Data structure
 */

typedef struct WebHandlerContext_ {
        QPlanContext *context;
        int connfd;
} WebHandlerContext;

/* ----------------------------------------------------------------------------
 * API
 */

void *web_routine(void *arg);
int web_register_lua_funcs(lua_State *L);

#endif
