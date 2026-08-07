#!/usr/bin/env bash
set -euo pipefail

COMPOSE_CMD=${COMPOSE_CMD:-"docker compose"}
BASE_URL=${BASE_URL:-"http://127.0.0.1:3838"}
REBUILD=${REBUILD:-"0"}

echo "[1/2] Ensuring shiny container is up"
if [[ "$REBUILD" == "1" ]]; then
	$COMPOSE_CMD up -d --build
else
	$COMPOSE_CMD up -d
fi

echo "Waiting for dashboard route to become reachable"
$COMPOSE_CMD exec -T shiny bash -lc "cd /srv/shiny-server && BASE_URL='$BASE_URL' ./tests/test_apps_http.sh >/dev/null"

echo "[2/2] Running testthat and shinytest2 suite inside container"
$COMPOSE_CMD exec -T shiny bash -lc "cd /srv/shiny-server && export CHROMOTE_CHROME=\${CHROMOTE_CHROME:-/usr/local/bin/chrome} && BASE_URL='$BASE_URL' Rscript tests/testthat.R"

echo "PASS: all tests completed inside container"