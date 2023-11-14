#!/usr/bin/env bash
set -e

if ! command -v python3 > /dev/null; then
  echo "Error: Requires Python 3 to be installed." >&2
  exit 1
fi

root="$(dirname "$0")/.."
if [[ ! -d "$root/.venv" ]]; then
  python3 -m venv "$root/.venv"
fi

"$root/.venv/bin/python3" -m pip install \
  --require-virtualenv \
  --no-input \
  --quiet \
  --requirement "$root/doc/requirements.txt"

"$root/.venv/bin/mkdocs" "$@"
