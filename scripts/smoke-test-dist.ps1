#!/usr/bin/env pwsh
param (
  [Parameter(Mandatory = $true)]
  [string]$Archive
)

$ErrorActionPreference = "Stop"

function New-SmokeTestFailure([string]$Message) {
  throw "error: $Message"
}

if (-not (Test-Path -LiteralPath $Archive -PathType Leaf)) {
  New-SmokeTestFailure "archive not found: $Archive"
}

$temporaryDirectory = Join-Path `
  ([IO.Path]::GetTempPath()) `
  "dojang-smoke-$([Guid]::NewGuid())"
$extracted = Join-Path $temporaryDirectory "extracted"
$repository = Join-Path $temporaryDirectory "repository"
$bootstrappedRepository = Join-Path $temporaryDirectory "bootstrapped-repository"
$homeDirectory = Join-Path $temporaryDirectory "home"
$stateRoot = Join-Path $temporaryDirectory "state"
$bootstrapHome = Join-Path $temporaryDirectory "bootstrap-home"
$bootstrapStateRoot = Join-Path $temporaryDirectory "bootstrap-state"
$previousHome = $env:HOME
$previousUserProfile = $env:USERPROFILE
$previousAppData = $env:APPDATA
$previousLocalAppData = $env:LOCALAPPDATA
$previousXdgDataHome = $env:XDG_DATA_HOME

New-Item -ItemType Directory -Path $temporaryDirectory | Out-Null
try {
  Expand-Archive -LiteralPath $Archive -DestinationPath $extracted
  $executable = Join-Path $extracted "dojang.exe"
  if (-not (Test-Path -LiteralPath $executable -PathType Leaf)) {
    New-SmokeTestFailure "archive does not contain the dojang.exe executable."
  }

  $versionOutput = & $executable version | Out-String
  if ($LASTEXITCODE -ne 0) {
    New-SmokeTestFailure "dojang version failed with exit code $LASTEXITCODE."
  }
  if ([string]::IsNullOrWhiteSpace($versionOutput)) {
    New-SmokeTestFailure "dojang version produced no output."
  }

  New-Item -ItemType Directory -Path $homeDirectory | Out-Null
  New-Item -ItemType Directory -Path $stateRoot | Out-Null
  New-Item -ItemType Directory -Path $bootstrapHome | Out-Null
  New-Item -ItemType Directory -Path $bootstrapStateRoot | Out-Null
  $env:HOME = $homeDirectory
  $env:USERPROFILE = $homeDirectory
  $env:APPDATA = $homeDirectory
  $env:LOCALAPPDATA = $stateRoot
  $env:XDG_DATA_HOME = $stateRoot

  & $executable `
    -r $repository `
    init `
    --windows-amd64 `
    --no-interactive
  if ($LASTEXITCODE -ne 0) {
    New-SmokeTestFailure "dojang init failed with exit code $LASTEXITCODE."
  }
  if (-not (
      Test-Path `
        -LiteralPath (Join-Path $repository "dojang.toml") `
        -PathType Leaf
    )) {
    New-SmokeTestFailure "dojang init did not create a manifest."
  }

  & $executable -r $repository status
  if ($LASTEXITCODE -ne 0) {
    New-SmokeTestFailure "dojang status failed with exit code $LASTEXITCODE."
  }

  $env:HOME = $bootstrapHome
  $env:USERPROFILE = $bootstrapHome
  $env:APPDATA = $bootstrapHome
  $env:LOCALAPPDATA = $bootstrapStateRoot
  $env:XDG_DATA_HOME = $bootstrapStateRoot
  & $executable `
    -r $bootstrappedRepository `
    init `
    --from $repository `
    --no-interactive `
    --yes
  if ($LASTEXITCODE -ne 0) {
    New-SmokeTestFailure `
      "dojang init --from failed with exit code $LASTEXITCODE."
  }
  if (-not (
      Test-Path `
        -LiteralPath (Join-Path $bootstrappedRepository "dojang.toml") `
        -PathType Leaf
    )) {
    New-SmokeTestFailure "dojang init --from did not copy the manifest."
  }
}
finally {
  $env:HOME = $previousHome
  $env:USERPROFILE = $previousUserProfile
  $env:APPDATA = $previousAppData
  $env:LOCALAPPDATA = $previousLocalAppData
  $env:XDG_DATA_HOME = $previousXdgDataHome
  Remove-Item `
    -LiteralPath $temporaryDirectory `
    -Recurse `
    -Force `
    -ErrorAction SilentlyContinue
}
