#!/usr/bin/env bash
set -euo pipefail

if ! command -v dasel > /dev/null; then
  echo "error: dasel not found." >&2
  exit 1
fi

if [[ "$1" = "" ]]; then
  echo "error: missing argument." >&2
  echo "usage: $0 DIR" >&2
  exit 1
fi

bin_dir="$1"

if [[ ! -f "$bin_dir/dojang" ]]; then
  echo "error: dojang not found." >&2
  exit 1
fi

chmod +x "$bin_dir/dojang"
mkdir -p "$bin_dir/completions/"
"$bin_dir/dojang" --bash-completion-script dojang \
  > "$bin_dir/completions/dojang.bash"
"$bin_dir/dojang" --zsh-completion-script dojang \
  > "$bin_dir/completions/dojang.zsh"
"$bin_dir/dojang" --fish-completion-script dojang \
  > "$bin_dir/completions/dojang.fish"

version="$("$bin_dir/dojang" version | awk '{print $2}')"
os="$("$bin_dir/dojang" env | dasel -p toml -w yaml ".os")"
arch="$("$bin_dir/dojang" env | dasel -p toml -w yaml ".arch")"

workdir="$(pwd)"

pushd "$bin_dir"
tar cvfJ "$workdir/dojang-$version-$os-$arch.tar.xz" ./*
popd
