#!/bin/sh
cd `dirname $0`


# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa $PWD/ebin edit $PWD/deps/*/ebin \
    -boot start_sasl \
    -sname ebml \
    -s ebml_app \
    -s ex_reloader

