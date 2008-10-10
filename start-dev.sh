#!/bin/sh
cd `dirname $0`
mkdir priv/www
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s mochirest
