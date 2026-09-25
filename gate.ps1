# Pre-commit gate. docs/11-plan-review.md section 6.
#   ./gate.ps1             validate + EditMode + PlayMode
#   ./gate.ps1 -EditOnly   validate + EditMode (SHOW lane, iterating)
param([switch]$EditOnly)

$ErrorActionPreference = 'Continue'

function Explain($code) {
    switch ($code) {
        8 { "A test failed (exit 8). Fix it - this is not a flake to retry." }
        6 { "No verdict (exit 6): compile error or licence problem. Not a pass." }
        default { "unity test exited $code." }
    }
}

$proj  = Join-Path $PSScriptRoot 'trench-warfare-3d'
$unity = Join-Path $env:LOCALAPPDATA 'unity\bin\unity.exe'
if (-not (Test-Path $unity)) { Write-Host "unity.exe not found at $unity" -ForegroundColor Red; exit 1 }

Push-Location $proj
try {
    Write-Host "`n== validate.py ==" -ForegroundColor Cyan
    python validate.py
    if ($LASTEXITCODE -ne 0) { Write-Host "validate.py failed ($LASTEXITCODE)" -ForegroundColor Red; exit $LASTEXITCODE }

    Write-Host "`n== EditMode ==" -ForegroundColor Cyan
    & $unity test . --mode EditMode --timeout 600
    $edit = $LASTEXITCODE
    if ($edit -ne 0) { Write-Host (Explain $edit) -ForegroundColor Red; exit $edit }

    if ($EditOnly) { Write-Host "`nEditMode green. PlayMode skipped (-EditOnly)." -ForegroundColor Yellow; exit 0 }

    Write-Host "`n== PlayMode ==" -ForegroundColor Cyan
    & $unity test . --mode PlayMode --timeout 600 -- -nographics
    $play = $LASTEXITCODE
    if ($play -ne 0) { Write-Host (Explain $play) -ForegroundColor Red; exit $play }

    Write-Host "`nGate green." -ForegroundColor Green
    exit 0
}
finally { Pop-Location }
