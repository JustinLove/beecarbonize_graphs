#!/bin/sh
dot -Tsvg generated/resources.dot | sed -e's/<svg/<svg id="graph"/' > generated/resources.dot.svg
cat header.html generated/resources.dot.svg footer.html | sed -e's/Tech Tree/Tech Resources/' > public/beecarbonize_tech_resources.html
