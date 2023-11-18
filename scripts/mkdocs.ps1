if (!(Get-Command python3 -ErrorAction Ignore)) {
  Write-Error "Requires Python 3 to be installed."
  exit 1
}

$root = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)

if (!(Test-Path -PathType Container $root\.venv)) {
  python3 -m venv $root\.venv
}

& $root\.venv\Scripts\python.exe -m pip install `
  --require-virtualenv `
  --no-input `
  --quiet `
  --requirement $root\doc\requirements.txt

& $root\.venv\Scripts\mkdocs.exe @Args
