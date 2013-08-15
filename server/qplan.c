#include <err.h>
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <signal.h>
#include <syslog.h>

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "errors.h"
#include "context.h"
#include "web.h"


static lua_State *init_lua_state();
static void sigint_handler(int signal);
static void clean_up();

static lua_State *L_main;


int main(int argc, char *argv[])
{
	void *thread_result;
	long status;
        pthread_t web_thread_id;
        pthread_mutex_t main_mutex = PTHREAD_MUTEX_INITIALIZER;

        openlog(NULL, LOG_CONS | LOG_PERROR | LOG_PID, LOG_USER);

        signal(SIGPIPE, SIG_IGN);
        signal(SIGINT, &sigint_handler);

        L_main = init_lua_state();

        /*
         * Register C functions to be called from lua
         */
        if (web_register_lua_funcs(L_main) != 0)
                lua_failure(__FILE__, __LINE__);

        /*
         * Set up context and spin up threads
         */
        Context context;
        context.main_lua_state = L_main;
        context.main_mutex = &main_mutex;

        /* Create web server thread */
	status = pthread_create(&web_thread_id, NULL, web_routine, (void *)&context);
	if (status != 0)
                pthread_failure(__FILE__, __LINE__);

	/* Join web server thread */
	status = pthread_join(web_thread_id, &thread_result);
	if (status != 0)
                pthread_failure(__FILE__, __LINE__);

        /*
         * Clean up
         */
        clean_up();
	return 0;
}


static lua_State *init_lua_state()
{
        lua_State *result = luaL_newstate();
        luaL_openlibs(result);

        /* Load qplan functionality */
        lua_getglobal(result, "require");
        lua_pushstring(result, "app.qplan");
        if (lua_pcall(result, 1, 1, 0) != LUA_OK)
                luaL_error(result, "Problem requiring qplan.lua: %s",
                                lua_tostring(result, -1));

        return result;
}


static void sigint_handler(int signal)
{
        clean_up();
        exit(0);
}

static void clean_up()
{
        lua_close(L_main);
        closelog();
	printf("\nWe are most successfully done!\n");
}
