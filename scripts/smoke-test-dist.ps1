#!/usr/bin/env pwsh
param (
  [Parameter(Mandatory = $true)]
  [string]$Archive
)

$ErrorActionPreference = "Stop"

function Stop-SmokeTest([string]$Message) {
  throw "error: $Message"
}

if (-not (Test-Path -LiteralPath $Archive -PathType Leaf)) {
  Stop-SmokeTest "archive not found: $Archive"
}

$temporaryDirectory = Join-Path `
  ([IO.Path]::GetTempPath()) `
  "dojang-smoke-$([Guid]::NewGuid())"
$extracted = Join-Path $temporaryDirectory "extracted"
$repository = Join-Path $temporaryDirectory "repository"
$homeDirectory = Join-Path $temporaryDirectory "home"
$stateRoot = Join-Path $temporaryDirectory "state"
$previousHome = $env:HOME
$previousLocalAppData = $env:LOCALAPPDATA
$previousXdgDataHome = $env:XDG_DATA_HOME

New-Item -ItemType Directory -Path $temporaryDirectory | Out-Null
try {
  Expand-Archive -LiteralPath $Archive -DestinationPath $extracted
  $executable = Join-Path $extracted "dojang.exe"
  if (-not (Test-Path -LiteralPath $executable -PathType Leaf)) {
    Stop-SmokeTest "archive does not contain the dojang.exe executable."
  }

  $versionOutput = & $executable version | Out-String
  if ($LASTEXITCODE -ne 0) {
    Stop-SmokeTest "dojang version failed with exit code $LASTEXITCODE."
  }
  if ([string]::IsNullOrWhiteSpace($versionOutput)) {
    Stop-SmokeTest "dojang version produced no output."
  }

  New-Item -ItemType Directory -Path $homeDirectory | Out-Null
  New-Item -ItemType Directory -Path $stateRoot | Out-Null
  $env:HOME = $homeDirectory
  $env:LOCALAPPDATA = $stateRoot
  $env:XDG_DATA_HOME = $stateRoot

  & $executable `
    -r $repository `
    init `
    --windows-amd64 `
    --no-interactive
  if ($LASTEXITCODE -ne 0) {
    Stop-SmokeTest "dojang init failed with exit code $LASTEXITCODE."
  }
  if (-not (
      Test-Path `
        -LiteralPath (Join-Path $repository "dojang.toml") `
        -PathType Leaf
    )) {
    Stop-SmokeTest "dojang init did not create a manifest."
  }

  & $executable -r $repository status
  if ($LASTEXITCODE -ne 0) {
    Stop-SmokeTest "dojang status failed with exit code $LASTEXITCODE."
  }
}
finally {
  $env:HOME = $previousHome
  $env:LOCALAPPDATA = $previousLocalAppData
  $env:XDG_DATA_HOME = $previousXdgDataHome
  Remove-Item `
    -LiteralPath $temporaryDirectory `
    -Recurse `
    -Force `
    -ErrorAction SilentlyContinue
}
