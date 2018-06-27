FROM rocker/shiny:latest
COPY docker/bin /usr/local/bin/

RUN apt-get update && apt-get -y install \
  libhiredis-dev \
  libssl-dev \
  libxml2-dev

## These are the deps pulled separately for faster builds
## lazyeval too
RUN install_packages --repo "https://mrc-ide.github.io/drat" \
  Rcpp \
  assertthat \
  crayon \
  curl \
  drat \
  hms \
  ids \
  openssl \
  pkgconfig \
  prettyunits \
  processx \
  progress \
  redux \
  remotes \
  rlang \
  rversions \
  shinyjs \
  withr \
  xml2

## This will change fairly frequently
RUN install_remote \
  richfitz/storr \
  mrc-ide/context@develop \
  mrc-ide/queuer@develop \
  mrc-ide/rrq@develop

COPY rrq-worker /usr/local/bin

RUN mkdir /srv/shiny-server/shinyq
COPY *.R init /srv/shiny-server/shinyq/

RUN cd /srv/shiny-server/shinyq/ && ./init

# shiny-server is scrubbing environment variables as a security
# measure so we need to specify the redis host some other way.  This
# is far from ideal because it means that the server can't be run
# without some effort to remap this host name.
COPY Renviron /srv/shiny-server/shinyq/.Renviron

RUN chown -R shiny:shiny /srv/shiny-server/shinyq
