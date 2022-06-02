#!/bin/sh

set -ev

ssh root@zhulab.ucsc.edu "rm -rf ~/wordpress/projects/RS4flower/*"

cp -r figures/ _book/
scp -r _book/* root@zhulab.ucsc.edu:~/wordpress/projects/RS4flower/
