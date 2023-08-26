#!/bin/sh
dot -Tsvg beecarbonize.dot | sed -e's/<svg/<svg id="graph"/' > beecarbonize.dot.svg
cat header.html beecarbonize.dot.svg footer.html > public/beecarbonize_tech_tree.html
