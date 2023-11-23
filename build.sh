#!/bin/bash

Rscript -e "
  devtools::document()
  pkgdown::build_site()
  pkgdown::preview_site()
"
