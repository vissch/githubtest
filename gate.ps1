# Pre-commit gate. docs/reference/workflow.md, "Gate".
#   ./gate.ps1             validate + EditMode + PlayMode
#   ./gate.ps1 -EditOnly   validate + EditMode (SHOW lane, iterating)
#
# Exit codes: 0 green; 8 a test failed; 6 no verdict (compile error, licence); 3 the project is held by an editor
# or another batch run (close it, or run the tests inside it: workflow.md); anything else is validate.py's code.
#
# Failures are printed with their message, so you do not need to open test-results.xml.
#
# EXTERNAL NOISE. A batch test run opens the com.unity.pipeline port like any editor. Another session's
# `unity command` or the MCP server polling it can make Unity log an error inside the run, and an unexpected error
# log fails whichever test happens to be running. Two signatures are known (both seen 2026-09-25, a different test
# each run): "Sharing violation on path ...\.unity-pipeline-port" and "Failed to handle /api/exec request". When
# EVERY failure carries one of them, the failed tests are rerun once and that verdict stands. A real failure is
# never retried.
param([switch]$EditOnly)

$ErrorActionPreference = 'Continue'
$noise = 'unity-pipeline-port|Failed to handle /api/exec request'

function Explain($code) {
    switch ($code) {
        8 { "A test failed (exit 8). Fix it - this is not a flake to retry." }
        6 { "No verdict (exit 6): compile error or licence problem. Not a pass." }
        default { "unity test exited $code." }
    }
}

# Print every failed test and the first lines of its message. Returns $true when all of them are external noise.
function Show-Failures($xmlPath) {
    if (-not (Test-Path $xmlPath)) { Write-Host "(no $xmlPath to read)"; return $false }
    [xml]$x = Get-Content $xmlPath -Raw
    $failed = @($x.SelectNodes('//test-case[@result="Failed"]'))
    $allNoise = $failed.Count -gt 0
    foreach ($c in $failed) {
        $msg = "$($c.failure.message.InnerText)".Trim()
        Write-Host "  FAILED $($c.fullname)" -ForegroundColor Red
        $first = ($msg -split "`n" | Select-Object -First 3) -join ' | '
        Write-Host "         $first"
        if ($msg -notmatch $noise) { $allNoise = $false }
    }
    return $allNoise
}

function Run-Tests($mode, [string[]]$extra) {
    & $unity test . --mode $mode --timeout 600 @extra | Out-Host
    $code = $LASTEXITCODE
    if ($code -ne 8) { return $code }
    $noiseOnly = Show-Failures 'test-results.xml'
    if (-not $noiseOnly) { return $code }
    Write-Host "`nEvery failure is external pipeline noise (see the header of gate.ps1). Rerunning the failed tests once." -ForegroundColor Yellow
    & $unity test . --mode $mode --timeout 600 --rerun-failed @extra | Out-Host
    $code = $LASTEXITCODE
    if ($code -eq 8) { Show-Failures 'test-results.xml' | Out-Null }
    return $code
}

$proj  = Join-Path $PSScriptRoot 'trench-warfare-3d'
$unity = Join-Path $env:LOCALAPPDATA 'unity\bin\unity.exe'
if (-not (Test-Path $unity)) { Write-Host "unity.exe not found at $unity" -ForegroundColor Red; exit 1 }

Push-Location $proj
try {
    python Tools/editor_lock.py guard
    if ($LASTEXITCODE -ne 0) {
        Write-Host "The project is held (an open editor or another batch run). Close it, or run the tests inside it: docs/reference/workflow.md, Gate." -ForegroundColor Red
        exit 3
    }

    Write-Host "`n== validate.py ==" -ForegroundColor Cyan
    python validate.py
    if ($LASTEXITCODE -ne 0) { Write-Host "validate.py failed ($LASTEXITCODE)" -ForegroundColor Red; exit $LASTEXITCODE }

    Write-Host "`n== EditMode ==" -ForegroundColor Cyan
    $edit = Run-Tests 'EditMode' @()
    if ($edit -ne 0) { Write-Host (Explain $edit) -ForegroundColor Red; exit $edit }

    if ($EditOnly) { Write-Host "`nEditMode green. PlayMode skipped (-EditOnly)." -ForegroundColor Yellow; exit 0 }

    Write-Host "`n== PlayMode ==" -ForegroundColor Cyan
    $play = Run-Tests 'PlayMode' @('--', '-nographics')
    if ($play -ne 0) { Write-Host (Explain $play) -ForegroundColor Red; exit $play }

    Write-Host "`nGate green." -ForegroundColor Green
    exit 0
}
finally { Pop-Location }
