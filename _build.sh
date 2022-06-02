#!/bin/sh

set -ev

/opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript -e "rmarkdown::clean_site(preview = FALSE)"
/opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
