#ifndef WEB_H
#define WEB_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "context.h"
#include "tcp.h"

/* ----------------------------------------------------------------------------
 * Data structure
 */

typedef struct WebHandlerContext_ {
        Context *context;
        int connfd;
} WebHandlerContext;

/* ----------------------------------------------------------------------------
 * API
 */

void *web_routine(void *arg);
int web_register_lua_funcs(lua_State *L);

#endif
