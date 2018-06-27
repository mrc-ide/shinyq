#!/usr/bin/env Rscript
dir.create(file.path("packages"), showWarnings = FALSE)

cran_packages <- c(
"Rcpp",
"assertthat",
"crayon",
"curl",
"drat",
"hms",
"ids",
"openssl",
"pkgconfig",
"prettyunits",
"processx",
"progress",
"redux",
"remotes",
"rlang",
"rversions",
"shinyjs",
"withr",
"xml2"
)
install.packages(cran_packages)

github_packages <- c(
"richfitz/storr",
"mrc-ide/context@develop",
"mrc-ide/queuer@develop",
"mrc-ide/rrq@develop"
)
remotes::install_github(github_packages)
