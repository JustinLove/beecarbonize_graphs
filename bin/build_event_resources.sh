#!/bin/sh
dot -Tsvg generated/event_resources.dot | sed -e's/<svg/<svg id="graph"/' > generated/event_resources.dot.svg
cat header.html generated/event_resources.dot.svg footer.html | sed -e's/Tech Tree/Event Resources/' > public/beecarbonize_event_resources.html
