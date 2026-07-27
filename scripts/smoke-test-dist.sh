#!/usr/bin/env bash
set -euo pipefail

error() {
  echo "error: $*" >&2
  exit 1
}

if [[ "$#" -ne 1 ]]; then
  error "usage: $0 ARCHIVE"
fi

archive="$1"
[[ -f "$archive" ]] || error "archive not found: $archive"

temporary_directory="$(mktemp -d "${TMPDIR:-/tmp}/dojang-smoke.XXXXXX")"
temporary_directory="$(cd "$temporary_directory" && pwd -P)"
trap 'rm -rf "$temporary_directory"' EXIT HUP INT TERM
extracted="$temporary_directory/extracted"
repository="$temporary_directory/repository"
bootstrapped_repository="$temporary_directory/bootstrapped-repository"
home="$temporary_directory/home"
state_root="$temporary_directory/state"
bootstrap_home="$temporary_directory/bootstrap-home"
bootstrap_state_root="$temporary_directory/bootstrap-state"
mkdir -p \
  "$extracted" \
  "$home" \
  "$state_root" \
  "$bootstrap_home" \
  "$bootstrap_state_root"

tar -xJf "$archive" -C "$extracted"
executable="$extracted/dojang"
[[ -f "$executable" ]] ||
  error "archive does not contain the dojang executable."
[[ -x "$executable" ]] ||
  error "archive contains a non-executable dojang file."

version_output="$("$executable" version)"
[[ -n "$version_output" ]] || error "dojang version produced no output."

case "$(uname -s)-$(uname -m)" in
  Linux-x86_64 | Linux-amd64) preset="--linux-amd64" ;;
  Linux-aarch64 | Linux-arm64) preset="--linux-aarch64" ;;
  Darwin-x86_64 | Darwin-amd64) preset="--intel-mac" ;;
  Darwin-arm64 | Darwin-aarch64) preset="--apple-silicon-mac" ;;
  *) error "unsupported smoke-test platform: $(uname -s)-$(uname -m)" ;;
esac

HOME="$home" \
  XDG_CONFIG_HOME="$home/.config" \
  XDG_DATA_HOME="$state_root" \
  "$executable" -r "$repository" init "$preset" --no-interactive
[[ -f "$repository/dojang.toml" ]] ||
  error "dojang init did not create a manifest."
HOME="$home" \
  XDG_CONFIG_HOME="$home/.config" \
  XDG_DATA_HOME="$state_root" \
  "$executable" -r "$repository" status
HOME="$bootstrap_home" \
  XDG_CONFIG_HOME="$bootstrap_home/.config" \
  XDG_DATA_HOME="$bootstrap_state_root" \
  "$executable" \
  -r "$bootstrapped_repository" \
  init \
  --from "$repository" \
  --no-interactive \
  --yes
[[ -f "$bootstrapped_repository/dojang.toml" ]] ||
  error "dojang init --from did not copy the manifest."
