#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>

#include <pthread.h>

#include "lua.h"
#include "lauxlib.h"

#include "tcp_io.h"
#include "web.h"
#include "websockets/ws.h"

extern void err_abort(int status, const char *message);


#define SA struct sockaddr
#define LISTENQ 1024
#define MAXLINE 1024

#define CONTENT_LENGTH_KEY_LEN 16
#define REGISTRATION_FUNCNAME "register_connection"
#define DEREGISTRATION_FUNCNAME "deregister_connection"

/*
 * Static declarations
 */
static const char content_length_key[] = "content-length: ";

static void *handle_request_routine(void *);
static int read_request_string(int, char **, size_t *);
static int read_request_body(int, const char *, char **, size_t *);
static int handle_http_request(int, Context *, const char *, size_t,
                                                    const char *, size_t);
static int registration_helper(const char *, int, Context *);
static int register_ws_connection(int, Context *);
static int deregister_ws_connection(int, Context *);

static int push_message(lua_State *);

// TODO: Allow port to be configured
/*
 * This will be run in a thread and listens for web requests. Each request is
 * handled in its own thread via handle_request_routine.
 */
void *web_routine(void *arg)
{
        Context *ctx = (Context *)arg;
        WebHandlerContext *handler_context;

	int listenfd, connfd;
	socklen_t clilen;
	struct sockaddr_in cliaddr, servaddr;
        pthread_t tid;
        int option = 1;
        int status;

	listenfd = socket(AF_INET, SOCK_STREAM, 0);

	/* Reuse port so we don't have to wait before the program can be
	 * restarted because of the TIME_WAIT state. */
        if ( setsockopt(listenfd,
                        SOL_SOCKET,
                        SO_REUSEADDR,
                        &option, sizeof(option)) != 0)
                err(1, "setsockopt failed");

	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port = htons(8888);

	if (bind(listenfd, (SA*) &servaddr, sizeof(servaddr)) < 0)
		err(1, "Problem binding to descriptor:%d", listenfd);

	if (listen(listenfd, LISTENQ) < 0)
		err(1, "Problem listening to descriptor: %d", listenfd);

        /*
         * Listen for connections
         */
        clilen = sizeof(cliaddr);
        while(1) {
		connfd = accept(listenfd, (SA*) &cliaddr, &clilen);

                /*
                 * Set up handler context
                 */
                handler_context = (WebHandlerContext *)malloc(sizeof(WebHandlerContext));
                if (handler_context == NULL)
                        err_abort(-1, "Unable to allocate memory");
                handler_context->context = ctx;
                handler_context->connfd = connfd;

                status = pthread_create(&tid, NULL, handle_request_routine,
                                                       (void *)handler_context);
                if (status != 0)
                        err_abort(status, "Create web thread");

                if (pthread_detach(tid) != 0)
                        err_abort(-1, "Couldn't detach thread");
        }

        return NULL;
}

static int lua_registration_helper(const char *funcname, int connfd,
                                                             lua_State *L)
{
        int error = 0;

        lua_getglobal(L, "WebSocket"); // This is set when requiring qplan.lua
        lua_pushstring(L, funcname);
        lua_gettable(L, -2);
        lua_pushinteger(L, connfd);
        error = lua_pcall(L, 1, 1, 0);

        return error;
}

static int registration_helper(const char *funcname, int connfd,
                                                     Context *context)
{
        int error;
        lua_State *L_main = context->main_lua_state;
        int result = 0;

        lock_main(context);

        error = lua_registration_helper(funcname, connfd, L_main);

        if (error) {
                fprintf(stderr, "%s\n", lua_tostring(L_main, -1));
                lua_pop(L_main, 1);
                result = -1;
                goto error;
        }

        /*
         * Get response from lua
         */
        result = lua_tointeger(L_main, -1);
        lua_pop(L_main, 1);


error:
        unlock_main(context);
        return result;
}

static int register_ws_connection(int connfd, Context *context)
{
        return registration_helper(REGISTRATION_FUNCNAME, connfd, context);
}


static int deregister_ws_connection(int connfd, Context *context)
{
        return registration_helper(DEREGISTRATION_FUNCNAME, connfd, context);
}

static int handle_websocket_request(int connfd, Context *context,
                                                const char* request_string)
{
        const char *res_str;
        int result = 0;
        uint8_t *response_frame = NULL;
        enum WebsocketFrameType frame_type;
        char *message = NULL;
        size_t frame_len;

        /*
         * Complete handshake to establish connection
         */
        res_str = ws_complete_handshake(request_string);
        my_writen(connfd, res_str, strlen(res_str));
        free((char *)res_str);
        register_ws_connection(connfd, context);


        /*
         * Read and handle messages
         */
        while(1) {
                frame_type = ws_read_next_message(connfd, my_buffered_read, &message);
                if (frame_type == WS_FT_TEXT) {
                        // Echo response, then close
                        printf("Got a message: %s\n", message);
                        frame_len = ws_make_text_frame(message, NULL, &response_frame);
                        my_writen(connfd, response_frame, frame_len);
                        free(response_frame);
                }
                else if (frame_type == WS_FT_PING) {
                        frame_len = ws_make_pong_frame(&response_frame);
                        my_writen(connfd, response_frame, frame_len);
                        free(response_frame);
                }
                else if (frame_type == WS_FT_CLOSE) {
                        deregister_ws_connection(connfd, context);
                        break;
                }
        }

        return result;
}



/*
 * Every web request is handled in its own thread. In general, this requires
 * access to the main lua state. The web routing and handling code is written in
 * lua and is also in the main lua state within the "web" module.
 *
 * NOTE: The req_context passed in as "arg" needs to be freed by this function.
 */
static void *handle_request_routine(void *arg)
{
        WebHandlerContext *req_context = (WebHandlerContext *)arg;
        int connfd = req_context->connfd;

        char *request_string;
        size_t req_len;

        char *body = NULL;
        size_t body_len = 0;


        /*
         * Read in requst string and body (if needed)
         */
        if (read_request_string(connfd, &request_string, &req_len) < 0)
                goto error;

        if (read_request_body(connfd, request_string, &body, &body_len) < 0)
                goto error;

        /*
         * Handle request (either http or websocket)
         */
        if (ws_is_handshake(request_string))
                handle_websocket_request(connfd, req_context->context, request_string);
        else
                handle_http_request(connfd, req_context->context,
                                     request_string, req_len, body, body_len);

error:
        close(connfd);
        free(request_string);
        free(body);
        free(req_context);
        return NULL;
}

/*
 * Reads in a request string. This may be a regular HTTP request or the
 * beginning of a websocket handshake.
 *
 * NOTE: Callers of this function are responsible for freeing "request_string"
 */
static int read_request_string(int connfd,
                               char **request_string_p, size_t *req_len_p)
{
	char buf[MAXLINE];
        int str_capacity;
        int cur_len;
        char *request_string;
        size_t req_len = 0;
        int i;

        if ((request_string = malloc(sizeof(char) * MAXLINE)) == NULL)
                err_abort(-1, "Couldn't allocate memory");
        str_capacity = MAXLINE;

        /*
         * Read in request
         */
	while ((cur_len = my_readline(connfd, buf, MAXLINE)) > 0) {
		if (strcmp(buf, "\r\n") == 0)
			break;

                /*
                 * Build request string
                 */
                if ((req_len + cur_len) >= str_capacity) {
                        if ((request_string = realloc(request_string,
                            sizeof(char) * (str_capacity + MAXLINE))) == NULL)
                                err_abort(-1, "Couldn't realloc memory");

                        str_capacity += MAXLINE;
                }
                for (i = 0; i < cur_len; i++)
                        request_string[req_len++] = buf[i];
	}
        request_string[req_len] = '\0';

        /*
         * Store results
         */
        *request_string_p = request_string;
        *req_len_p = req_len;

        return 0;
}


/*
 * This reads in the body of a request based on the "Content-length" header in
 * the "request_string".
 *
 * NOTE: Callers of this function are responsible for freeing "body".
 */
static int read_request_body(int connfd, const char *request_string,
                                           char **body_p, size_t *body_len_p)
{
        char *s;
        char *body = NULL;
        size_t body_len = 0;

        /*
         * Read in body (if any)
         */
        if ((s = strcasestr(request_string, "Content-Length: "))) {
                body_len = strtol(s + CONTENT_LENGTH_KEY_LEN, NULL, 0);

                if ((body = malloc(body_len + 1)) == NULL)
                        err_abort(-1, "Couldn't allocate memory");

                if (my_buffered_read(connfd, body, body_len) < 0)
                        goto error;
        }

        /*
         * Store results
         */
        *body_p = body;
        *body_len_p = body_len;

        return 0;

error:
        free(body);
        return -1;
}


/*
 * This calls out to lua to generate the response for the given request string
 * and body. This also writes the response back out.
 *
 * NOTE: It's up to the caller of this function to close the connection.
 */
static int handle_http_request(int connfd, Context *context,
                                    const char *request_string, size_t req_len,
                                              const char *body, size_t body_len)
{
        int error;
        lua_State *L_main = context->main_lua_state;
        char *res_str = NULL;
        size_t res_len = 0;
        const char *tmp = NULL;
        int result = 0;

        lock_main(context);

        /*
         * Call "handle_request" from lua
         */
        lua_getglobal(L_main, "Router"); // This is set when requiring qplan.lua
        lua_pushstring(L_main, "handle_request");
        lua_gettable(L_main, -2);
        lua_pushlstring(L_main, request_string, req_len);
        lua_pushlstring(L_main, body, body_len);
        error = lua_pcall(L_main, 2, 1, 0);
        if (error) {
                fprintf(stderr, "%s\n", lua_tostring(L_main, -1));
                lua_pop(L_main, 1);
                result = -1;
                goto error;
        }

        /*
         * Get response string from lua
         */
        tmp = lua_tolstring(L_main, -1, &res_len);
        if ((res_str = (char *)malloc(sizeof(char)*res_len)) == NULL)
                err_abort(-1, "Couldn't allocate memory");
        strncpy(res_str, tmp, res_len);
        lua_pop(L_main, 1);

        /*
         * Write response out
         */
        my_writen(connfd, res_str, res_len);

error:
        unlock_main(context);
        free(res_str);
        return result;
}


static int push_message(lua_State *L)
{
        uint8_t connfd;
        const char *message = NULL;
        uint8_t *response_frame = NULL;
        size_t frame_len;

        connfd = luaL_checkunsigned(L, 1);
        message = luaL_checkstring(L, 2);

        /*
         * Write data to connfd. If something goes wrong, deregister this
         * channel.
         */
        frame_len = ws_make_text_frame(message, NULL, &response_frame);
        if (my_writen(connfd, response_frame, frame_len) != 0) {
                if (lua_registration_helper(DEREGISTRATION_FUNCNAME,connfd,L)) {
                        fprintf(stderr, "%s\n", lua_tostring(L, -1));
                        lua_pop(L, 1);
                }
        }
        free(response_frame);

        return 0; /* No results */
}

int web_register_lua_funcs(lua_State *L)
{
        lua_pushcfunction(L, push_message);
        lua_setglobal(L, "push_message");

        return 0;
}
