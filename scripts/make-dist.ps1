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

# Pipe env output through Write-Output to ensure proper string conversion
$envContent = & "$binDir\dojang" env | Out-String
$os = $envContent | Write-Output | dasel -i toml -o yaml "os"
$arch = $envContent | Write-Output | dasel -i toml -o yaml "arch"

$workDir = Get-Location

Compress-Archive `
  -Path $binDir\* `
  -DestinationPath dojang-$version-$os-$arch.zip `
  -CompressionLevel Optimal
