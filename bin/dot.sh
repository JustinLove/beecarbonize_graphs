#!/bin/sh
dot -Tsvg $1 | sed -e's/<svg/<svg id=\"graph\"/' > $2
