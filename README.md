# CIE Dashboards + Uploads

This repository hosts two Shiny applications behind one Shiny Server:

- `cie-dashboards`: analytics dashboard for CIE participant and programme data.
- `cie-uploads`: upload and validation UI for source files used by the dashboard.

Both apps are served from the same container on port `3838`.

## App URLs

- Dashboard: `http://localhost:3838/cie-dashboards/`
- Uploads: `http://localhost:3838/cie-uploads/`

## Project Layout

- `cie-dashboards/ui.R` and `cie-dashboards/server.R`: dashboard app entrypoints.
- `cie-uploads/app.R`: uploads app entrypoint.
- `data/`: primary data directory consumed by apps.
- `backup_data/`: backup destination used by uploads flows.
- `renv.lock` and `renv/activate.R`: reproducible R dependency environment.
- `shiny-server.conf`: route mapping for both app paths.
- `Dockerfile`: container build, system dependencies, and `renv::restore()`.
- `docker-compose.yml`: compose service definition with bind-mounted data paths.

## Runtime Data Requirements

The dashboard reads files from `../data` relative to `cie-dashboards`, including:

- `all.csv`
- `all_studio.csv`
- `all_training.csv`
- `tags/tags_selection.csv`

If these files are missing in the mounted data path, `cie-dashboards` returns HTTP 500.

## Run with Docker

Build image:

```bash
docker build -t shiny .
```

Run container with required bind mounts:

```bash
DATA_DIR=$(readlink -f data)
BACKUP_DIR=$(readlink -f backup_data)

docker run -d --name shiny -p 3838:3838 \
	--group-add 100 \
	--group-add 998 \
	-v "$(readlink -f shiny-server.conf)":/etc/shiny-server/shiny-server.conf:ro \
	-v "$DATA_DIR":/srv/shiny-server/data \
	-v "$BACKUP_DIR":/srv/shiny-server/backup_data \
	shiny
```

The extra groups above allow the `shiny` runtime user to write into existing year folders (for example `2026`) and backup folders with host group ownership.

Stop/remove:

```bash
docker rm -f shiny
```

## Run with Docker Compose

Set a compose command for your environment:

```bash
COMPOSE_CMD=docker-compose
# or
COMPOSE_CMD="docker compose"
```

Start:

```bash
DATA_DIR=$(readlink -f data) \
BACKUP_DIR=$(readlink -f backup_data) \
DATA_GID=100 \
BACKUP_GID=998 \
$COMPOSE_CMD up -d --build
```

Stop:

```bash
$COMPOSE_CMD down
```

If `DATA_DIR` and `BACKUP_DIR` are not set, compose falls back to `./data` and `./backup_data`.
The compose service also bind-mounts `./shiny-server.conf` into `/etc/shiny-server/shiny-server.conf`.

## Health Checks

```bash
curl -i http://localhost:3838/cie-dashboards/
curl -i http://localhost:3838/cie-uploads/
```

Expected: HTTP 200 for both routes.

## Tests

Run all tests inside the `shiny` container (recommended):

```bash
chmod +x tests/run_in_container.sh tests/test_apps_http.sh
REBUILD=1 ./tests/run_in_container.sh
# REBUILD=1 rebuilds the image so the latest tests/app code is included. The suite
# needs testthat and shinytest2, which the Dockerfile installs, so a stale image
# fails with "there is no package called 'testthat'".
# Without REBUILD, the existing image is reused (faster when only rerunning tests).
./tests/run_in_container.sh
```

This runs:

- `tests/testthat.R`, which runs a `testthat` suite covering:
- deployed route reachability inside the container
- mounted data/backup directory writability
- upload sheet-choice regression checks
- `shinytest2` browser-driven dashboard tab coverage
- `shinytest2` browser-driven upload preview, save, and reload coverage using sandboxed data directories

Run the upload regression test (covers SSO selection with no file attached):

```bash
Rscript tests/test_upload_sheets.R
```

Run HTTP smoke tests for both apps:

```bash
./tests/test_apps_http.sh
```

## Troubleshooting

- `404 Not Found` for app routes:
	- Ensure `shiny-server.conf` maps `/cie-dashboards` and `/cie-uploads` before `location /`.
- `500 Internal Server Error` on `cie-dashboards`:
	- Verify mounted `data` contains `all.csv` and related CSV/tag files.
- Dependency/runtime mismatch:
	- Rebuild image so `renv::restore()` runs from `renv.lock`.
- Container logs:
	- `docker logs shiny --tail 200`
	- Per-app logs inside container: `/var/log/shiny-server/`