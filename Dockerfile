# syntax=docker/dockerfile:1.7
FROM rocker/shiny:4.3.3

# System libraries required by packages in renv.lock.
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt/lists,sharing=locked \
    apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    gfortran \
    default-jdk \
    fonts-liberation \
    libasound2 \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libbz2-dev \
    libcups2 \
    libdbus-1-3 \
    libdrm2 \
    libgbm1 \
    libglpk40 \
    libgtk-3-0 \
    liblzma-dev \
    libcurl4-openssl-dev \
    libnspr4 \
    libnss3 \
    libssl-dev \
    libuv1-dev \
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
    unzip \
    xdg-utils \
    zlib1g-dev \
    && CHROME_VERSION=$(curl -fsSL https://googlechromelabs.github.io/chrome-for-testing/LATEST_RELEASE_STABLE) \
    && curl -fsSL "https://storage.googleapis.com/chrome-for-testing-public/${CHROME_VERSION}/linux64/chrome-linux64.zip" -o /tmp/chrome-linux64.zip \
    && unzip -q /tmp/chrome-linux64.zip -d /opt \
    && mv /opt/chrome-linux64 /opt/chrome \
    && ln -sf /opt/chrome/chrome /usr/local/bin/chrome \
    && rm -f /tmp/chrome-linux64.zip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

# Copy lock metadata first so dependency restore can be cached.
COPY renv.lock ./renv.lock

# Keep renv libraries outside the app directory so later COPY doesn't overwrite them.
ENV RENV_PATHS_LIBRARY_ROOT=/usr/local/lib/R/renv
ENV RENV_PATHS_CACHE=/root/.cache/R/renv

# Prefer precompiled Linux binaries where available (jammy) to avoid compiling from source.
# Falls back to source if a binary is unavailable for a package/version.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/jammy/latest
ENV CHROMOTE_CHROME=/usr/local/bin/chrome

# Install renv and restore pinned dependencies from renv.lock.
RUN --mount=type=cache,target=/root/.cache/R/renv,sharing=locked \
    R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
    && R -q -e "renv::consent(TRUE); renv::restore(project = '/srv/shiny-server', lockfile = '/srv/shiny-server/renv.lock', prompt = FALSE)"

# Copy the rest of the application source.
COPY . ./

# Persist app data by default and allow host bind mounts.
VOLUME ["/srv/shiny-server/data", "/srv/shiny-server/backup_data"]

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]