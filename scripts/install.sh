#!/usr/bin/env sh
set -eu

error() {
  echo "error: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" > /dev/null 2>&1 ||
    error "$1 is required to install Dojang."
}

require_command curl
require_command install
require_command tar

version="${DOJANG_INSTALL_VERSION:-}"
if [ -z "$version" ]; then
  latest_url="$(
    curl -fsSL -o /dev/null -w '%{url_effective}' \
      https://github.com/dahlia/dojang/releases/latest
  )"
  version="${latest_url##*/}"
fi

case "$version" in
  "" | *[!A-Za-z0-9._-]*)
    error "invalid Dojang version: $version"
    ;;
esac

operating_system="${DOJANG_INSTALL_OS:-}"
if [ -z "$operating_system" ]; then
  case "$(uname -s)" in
    Linux) operating_system="linux" ;;
    Darwin) operating_system="macos" ;;
    *) error "unsupported operating system: $(uname -s)" ;;
  esac
fi

architecture="${DOJANG_INSTALL_ARCH:-}"
if [ -z "$architecture" ]; then
  case "$(uname -m)" in
    x86_64 | amd64) architecture="x86_64" ;;
    aarch64 | arm64) architecture="aarch64" ;;
    *) error "unsupported architecture: $(uname -m)" ;;
  esac
fi

case "$operating_system-$architecture" in
  linux-x86_64 | linux-aarch64 | macos-x86_64 | macos-aarch64) ;;
  *) error "unsupported platform: $operating_system-$architecture" ;;
esac

base_url="${DOJANG_INSTALL_BASE_URL:-https://github.com/dahlia/dojang/releases/download}"
base_url="${base_url%/}"
asset="dojang-$version-$operating_system-$architecture.tar.xz"
temporary_directory="$(mktemp -d "${TMPDIR:-/tmp}/dojang-install.XXXXXX")"
trap 'rm -rf "$temporary_directory"' EXIT HUP INT TERM
archive="$temporary_directory/$asset"
checksums="$temporary_directory/SHA256SUMS"

curl -fsSL "$base_url/$version/$asset" -o "$archive"
curl -fsSL "$base_url/$version/SHA256SUMS" -o "$checksums"

expected_checksum="$(
  awk -v filename="$asset" '
    {
      candidate = $2
      sub(/^\*/, "", candidate)
    }
    candidate == filename {
      if (found) {
        exit 2
      }
      print $1
      found = 1
    }
    END {
      if (!found) {
        exit 1
      }
    }
  ' "$checksums"
)" || error "SHA256SUMS does not contain exactly one checksum for $asset."
expected_checksum="$(
  printf '%s' "$expected_checksum" | tr 'A-F' 'a-f'
)"

case "$expected_checksum" in
  *[!A-Fa-f0-9]* | "")
    error "SHA256SUMS contains an invalid checksum for $asset."
    ;;
esac
[ "${#expected_checksum}" -eq 64 ] ||
  error "SHA256SUMS contains an invalid checksum for $asset."

if command -v sha256sum > /dev/null 2>&1; then
  actual_checksum="$(sha256sum "$archive" | awk '{print $1}')"
elif command -v shasum > /dev/null 2>&1; then
  actual_checksum="$(shasum -a 256 "$archive" | awk '{print $1}')"
else
  error "sha256sum or shasum is required to verify Dojang."
fi
actual_checksum="$(printf '%s' "$actual_checksum" | tr 'A-F' 'a-f')"

[ "$actual_checksum" = "$expected_checksum" ] ||
  error "checksum verification failed for $asset."

extracted="$temporary_directory/extracted"
mkdir "$extracted"
tar -xJf "$archive" -C "$extracted"
[ -f "$extracted/dojang" ] ||
  error "$asset does not contain the dojang executable."

install_directory="${DOJANG_INSTALL_DIR:-}"
if [ -z "$install_directory" ]; then
  [ -n "${HOME:-}" ] ||
    error "HOME or DOJANG_INSTALL_DIR is required."
  install_directory="$HOME/.local/bin"
fi
install -d "$install_directory"
install -m 0755 "$extracted/dojang" "$install_directory/dojang"

echo "Dojang $version installed at $install_directory/dojang."
