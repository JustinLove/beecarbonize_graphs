#!/bin/sh
dot -Tsvg beecarbonizeevents.dot | sed -e's/<svg/<svg id="graph"/' > beecarbonizeevents.dot.svg
cat header.html beecarbonizeevents.dot.svg footer.html | sed -e's/Tech Tree/Event Chances/' > public/beecarbonize_event_chances.html
