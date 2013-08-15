#include <err.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <syslog.h>
#include <unistd.h>

#include "lauxlib.h"
#include "lua.h"
#include "lualib.h"

#include "context.h"
#include "errors.h"
#include "web.h"

/*============================================================================= 
 * Static declarations
 */
static void clean_up();
static lua_State *init_lua_state();
static void sigint_handler(int signal);

static pthread_mutex_t main_mutex = PTHREAD_MUTEX_INITIALIZER;
static lua_State *L_main;


/*============================================================================= 
 * Main
 */
int
main(int argc, char *argv[])
{
	void *thread_result;
	long status;
        pthread_t web_thread_id;

        /* Enable syslog */
        openlog(NULL, LOG_CONS | LOG_PERROR | LOG_PID, LOG_USER);

        /* Handle program signals */
        signal(SIGPIPE, SIG_IGN);
        signal(SIGINT, &sigint_handler);

        /* Set up lua */
        L_main = init_lua_state();
        if (web_register_lua_funcs(L_main) != 0)
                lua_failure(__FILE__, __LINE__);

        /* Spin up web server thread */
        Context context;
        context.main_lua_state = L_main;
        context.main_mutex = &main_mutex;
	status = pthread_create(&web_thread_id, NULL, web_routine, (void *)&context);
	if (status != 0)
                pthread_failure(__FILE__, __LINE__);


	/* Join web server thread when done. */
        /* NOTE: Most of the time, we'll exit through a SIGINT */
	status = pthread_join(web_thread_id, &thread_result);
	if (status != 0)
                pthread_failure(__FILE__, __LINE__);

        /* Clean up */
        clean_up();
	return 0;
}


/*============================================================================= 
 * Static functions
 */


/*------------------------------------------------------------------------------
 * Cleans up before exiting program
 */
static void
clean_up()
{
        lua_close(L_main);
        closelog();
	printf("\nWe are most successfully done!\n");
}

/*------------------------------------------------------------------------------
 * Requires lua modules needed by the app
 *
 * NOTE: We may want to specify the file to require on the commandline. This
 * will make this app more generic.
 */
static lua_State *
init_lua_state()
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


/*------------------------------------------------------------------------------
 * Handles SIGINT by just cleaning up and exiting.
 */
static void
sigint_handler(int signal)
{
        clean_up();
        exit(0);
}
