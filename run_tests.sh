#!/usr/bin/zsh

lm_results=$(cd lua_modules/tests/; lua tests.lua)
sh_results=$(cd tests; for f in test*.sh; do $f; done)
server_results=$(cd server; gnustep-tests)

echo "===>> shell script tests"
echo $sh_results

echo "===>> lua_modules tests"
echo $lm_results

echo "===>> server tests"
echo $server_results

# TODO: Combine results and analyze
