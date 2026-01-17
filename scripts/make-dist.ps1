#!/usr/bin/env pwsh
param ([string]$binDir)

if (-not (Get-Command dasel -ErrorAction Ignore)) {
  Write-Error "dasel not found"
  exit 1
}

if (-not (Get-Command "$binDir/dojang" -ErrorAction Ignore)) {
  Write-Error "dojang not found"
  exit 1
}

$version = (& "$binDir/dojang" version).Split()[1]
& "$binDir\dojang" env | Out-File .env.toml -Encoding ascii
$os = dasel -f .env.toml -p toml -w yaml ".os"
$arch = dasel -f .env.toml -p toml -w yaml ".arch"
Remove-Item .env.toml

$workDir = Get-Location

Compress-Archive `
  -Path $binDir\* `
  -DestinationPath dojang-$version-$os-$arch.zip `
  -CompressionLevel Optimal
