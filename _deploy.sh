#!/bin/sh

set -ev

/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/bin/R -e "bookdown::publish_book(render = 'local',account = 'yiluansong', server = 'bookdown.org')"

