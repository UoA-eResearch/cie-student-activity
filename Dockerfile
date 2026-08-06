FROM rocker/shiny:4.1.2

# System libraries required by packages in renv.lock.
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    default-jdk \
    libbz2-dev \
    liblzma-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libicu-dev \
    libcairo2-dev \
    libxt-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

# Copy lock metadata first so dependency restore can be cached.
COPY renv.lock ./renv.lock

# Keep renv libraries outside the app directory so later COPY doesn't overwrite them.
ENV RENV_PATHS_LIBRARY_ROOT=/usr/local/lib/R/renv

# Install renv and restore pinned dependencies from renv.lock.
RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
    && R -q -e "renv::consent(TRUE); renv::restore(project = '/srv/shiny-server', lockfile = '/srv/shiny-server/renv.lock', prompt = FALSE)"

# Copy the rest of the application source.
COPY . ./

# Use explicit route mappings for Shiny apps.
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Persist app data by default and allow host bind mounts.
VOLUME ["/srv/shiny-server/data", "/srv/shiny-server/backup_data"]

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]