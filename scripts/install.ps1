#!/usr/bin/env pwsh
$ErrorActionPreference = "Stop"

function Stop-Install([string]$Message) {
  throw "error: $Message"
}

$version = $env:DOJANG_INSTALL_VERSION
if ([string]::IsNullOrWhiteSpace($version)) {
  $release = Invoke-RestMethod `
    -Headers @{ "User-Agent" = "dojang-installer" } `
    -Uri "https://api.github.com/repos/dahlia/dojang/releases/latest"
  $version = $release.tag_name
}
if ($version -notmatch "^[A-Za-z0-9][A-Za-z0-9._-]*$") {
  Stop-Install "invalid Dojang version: $version"
}

$architecture = $env:DOJANG_INSTALL_ARCH
if ([string]::IsNullOrWhiteSpace($architecture)) {
  $runtimeArchitecture = [Runtime.InteropServices.RuntimeInformation]::
    OSArchitecture.ToString()
  $architecture = switch ($runtimeArchitecture) {
    "X64" { "x86_64" }
    default {
      Stop-Install "unsupported Windows architecture: $runtimeArchitecture"
    }
  }
}
if ($architecture -ne "x86_64") {
  Stop-Install "unsupported Windows architecture: $architecture"
}

$baseUrl = $env:DOJANG_INSTALL_BASE_URL
if ([string]::IsNullOrWhiteSpace($baseUrl)) {
  $baseUrl = "https://github.com/dahlia/dojang/releases/download"
}
$baseUrl = $baseUrl.TrimEnd("/")
$asset = "dojang-$version-windows-$architecture.zip"
$temporaryDirectory = Join-Path `
  ([IO.Path]::GetTempPath()) `
  "dojang-install-$([Guid]::NewGuid())"
$archive = Join-Path $temporaryDirectory $asset
$checksums = Join-Path $temporaryDirectory "SHA256SUMS"
$extracted = Join-Path $temporaryDirectory "extracted"

New-Item -ItemType Directory -Path $temporaryDirectory | Out-Null
try {
  Invoke-WebRequest `
    -Uri "$baseUrl/$version/$asset" `
    -OutFile $archive `
    -UseBasicParsing
  Invoke-WebRequest `
    -Uri "$baseUrl/$version/SHA256SUMS" `
    -OutFile $checksums `
    -UseBasicParsing

  $matches = @(
    foreach ($line in Get-Content -LiteralPath $checksums) {
      if ($line -match "^([A-Fa-f0-9]{64})\s+\*?(.+)$") {
        if ($Matches[2] -ceq $asset) {
          $Matches[1].ToLowerInvariant()
        }
      }
    }
  )
  if ($matches.Count -ne 1) {
    Stop-Install `
      "SHA256SUMS does not contain exactly one checksum for $asset."
  }

  $actualChecksum = (
    Get-FileHash -LiteralPath $archive -Algorithm SHA256
  ).Hash.ToLowerInvariant()
  if ($actualChecksum -cne $matches[0]) {
    Stop-Install "checksum verification failed for $asset."
  }

  Expand-Archive -LiteralPath $archive -DestinationPath $extracted
  $sourceExecutable = Join-Path $extracted "dojang.exe"
  if (-not (Test-Path -LiteralPath $sourceExecutable -PathType Leaf)) {
    Stop-Install "$asset does not contain the dojang.exe executable."
  }

  $installDirectory = $env:DOJANG_INSTALL_DIR
  if ([string]::IsNullOrWhiteSpace($installDirectory)) {
    $installDirectory = Join-Path $HOME ".local/bin"
  }
  New-Item -ItemType Directory -Force -Path $installDirectory | Out-Null
  $installedExecutable = Join-Path $installDirectory "dojang.exe"
  Copy-Item -LiteralPath $sourceExecutable -Destination $installedExecutable
  Write-Output "Dojang $version installed at $installedExecutable."
}
finally {
  Remove-Item -LiteralPath $temporaryDirectory -Recurse -Force `
    -ErrorAction SilentlyContinue
}
