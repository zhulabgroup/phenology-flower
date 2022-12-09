#!/bin/sh

set -ev

/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/bin/R -e "rmarkdown::clean_site(preview = FALSE)"
/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/bin/R -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
