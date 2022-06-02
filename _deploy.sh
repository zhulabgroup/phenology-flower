#!/bin/sh

set -ev

/opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript -e "bookdown::publish_book(render = 'local',account = 'yiluansong', server = 'bookdown.org')"
# ssh root@zhulab.ucsc.edu "rm -rf ~/wordpress/projects/RS4flower/*"

# cp -r figures/ _book/
# scp -r _book/* root@zhulab.ucsc.edu:~/wordpress/projects/RS4flower/
