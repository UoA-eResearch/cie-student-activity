#!/usr/bin/env bash
set -euo pipefail

BASE_URL="${BASE_URL:-http://127.0.0.1:3838}"

check_reachable() {
  local name="$1"
  local url="$2"

  BASE_URL_CHECK="$url" Rscript - <<'EOF'
target <- Sys.getenv("BASE_URL_CHECK")
ok <- FALSE
for (i in seq_len(15)) {
  ok <- tryCatch({
    con <- url(target, open = "rb")
    on.exit(close(con), add = TRUE)
    length(readBin(con, what = "raw", n = 2048)) > 0
  }, error = function(e) FALSE)
  if (ok) break
  Sys.sleep(1)
}
if (!ok) {
  quit(status = 1)
}
EOF

  echo "PASS: $name"
}

check_reachable "dashboard app" "$BASE_URL/cie-dashboards/"
check_reachable "uploads app" "$BASE_URL/cie-uploads/"

echo "PASS: all app HTTP checks"
