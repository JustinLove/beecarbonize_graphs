#!/bin/sh
bash bin/build_tech_tree.sh
bash bin/build_event_chances.sh
bash bin/build_tech_resources.sh

cp public/*.html "public/20230810/"
