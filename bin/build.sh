#!/bin/sh
dot -Tsvg beecarbonize.dot | sed -e's/<svg/<svg id="graph"/' > beecarbonize.dot.svg
cat header.html beecarbonize.dot.svg footer.html > public/beecarbonize_tech_tree.html

dot -Tsvg beecarbonizeevents.dot | sed -e's/<svg/<svg id="graph"/' > beecarbonizeevents.dot.svg
cat header.html beecarbonizeevents.dot.svg footer.html | sed -e's/Tech Tree/Event Chances/' > public/beecarbonize_event_chances.html

dot -Tsvg generated/resources.dot | sed -e's/<svg/<svg id="graph"/' > generated/resources.dot.svg
cat header.html generated/resources.dot.svg footer.html | sed -e's/Tech Tree/Tech Resources/' > public/beecarbonize_tech_resources.html

cp public/*.html "public/20230810/"
