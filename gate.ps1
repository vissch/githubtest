# Pre-commit gate. docs/reference/workflow.md, "Gate".
#   ./gate.ps1             validate + EditMode + PlayMode
#   ./gate.ps1 -EditOnly   validate + EditMode (SHOW lane, iterating)
#
# Exit codes: 0 green; 8 a test failed; 6 no verdict (compile error, licence); 3 the project is held by an editor
# or another batch run (close it, or run the tests inside it: workflow.md); 5 validate.py failed (its lines are
# printed); 1 unity.exe is missing; any other code is unity's own.
#
# Failures are printed with their message, so you do not need to open test-results.xml.
#
# A green FULL run records the tree it tested (the working tree as it would be committed, untracked files included)
# in tw-gate-green in this checkout's git dir. Tools/land.py lands code only when HEAD is exactly that tree. The tree
# is taken before the tests and again after; if anything changed in between, nothing is recorded.
#
# EXTERNAL NOISE. A batch test run opens the com.unity.pipeline port like any editor. Another session's
# `unity command` or the MCP server polling it can make Unity log an error inside the run, and an unexpected error
# log fails whichever test happens to be running. Two signatures are known (both seen 2026-09-25, a different test
# each run): "Sharing violation on path ...\.unity-pipeline-port" and "Failed to handle /api/exec request". When
# EVERY failure carries one of them and none carries an assertion (Expected / But was / Assert), the failed tests
# are rerun once and that verdict stands. A real failure is never retried.
#
# The xml is the verdict, not unity's exit code: failures in it, or a run in which nothing passed, fail the suite
# even when unity exits 0.
param([switch]$EditOnly)

$ErrorActionPreference = 'Continue'
$noise = 'unity-pipeline-port|Failed to handle /api/exec request'
$assertion = 'Expected|But was|Assert'

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
        if ($msg -notmatch $noise -or $msg -match $assertion) { $allNoise = $false }
    }
    return $allNoise
}

# Print what actually ran and keep a copy per mode (the next run overwrites test-results.xml). A "pass" that ran no
# tests is no verdict: the EditMode run prints nothing to the console, so without this line green and empty look alike.
function Report-Run($mode, $code) {
    if (-not (Test-Path 'test-results.xml')) { Write-Host "$mode : no test-results.xml written" -ForegroundColor Red; return 6 }
    Copy-Item 'test-results.xml' "test-results-$mode.xml" -Force
    [xml]$x = Get-Content 'test-results.xml' -Raw
    $r = $x.'test-run'
    Write-Host "$mode : $($r.total) run, $($r.passed) passed, $($r.failed) failed, $($r.skipped) skipped (test-results-$mode.xml)"
    if ($code -eq 0 -and [int]$r.total -eq 0) { Write-Host "$mode ran no tests: not a pass." -ForegroundColor Red; return 6 }
    # the xml is the verdict, not unity's exit code (memory: unity-batch-gate-verdicts)
    if ($code -eq 0 -and ([int]$r.failed -gt 0 -or $r.result -ne 'Passed')) { Write-Host "$mode : the xml says $($r.result), $($r.failed) failed, though unity exited 0: not a pass." -ForegroundColor Red; return 8 }
    if ($code -eq 0 -and [int]$r.passed -eq 0) { Write-Host "$mode : nothing passed ($($r.skipped) skipped): not a pass." -ForegroundColor Red; return 6 }
    return $code
}

function Run-Tests($mode, [string[]]$extra) {
    Remove-Item 'test-results.xml' -ErrorAction SilentlyContinue
    Remove-Item "test-results-$mode.xml" -ErrorAction SilentlyContinue   # a run that writes nothing must not leave the last green copy
    & $unity test . --mode $mode --timeout 600 @extra | Out-Host
    $code = $LASTEXITCODE
    $first = Report-Run $mode $code          # the full run's results stay in test-results-<mode>.xml whatever follows
    if ($code -ne 8) { return $first }
    $noiseOnly = Show-Failures 'test-results.xml'
    if (-not $noiseOnly) { return $code }
    Write-Host "`nEvery failure is external pipeline noise (see the header of gate.ps1). Rerunning the failed tests once." -ForegroundColor Yellow
    Remove-Item 'test-results.xml' -ErrorAction SilentlyContinue
    & $unity test . --mode $mode --timeout 600 --rerun-failed @extra | Out-Host
    $code = $LASTEXITCODE
    if ($code -eq 8) { Show-Failures 'test-results.xml' | Out-Null }
    return (Report-Run "$mode-rerun" $code)
}

# The tree a commit of the working tree would record, through a scratch index; $null when git cannot say.
function Working-Tree {
    $index = Join-Path $env:TEMP "tw-gate-index-$PID"
    try {
        $env:GIT_INDEX_FILE = $index
        git -C $PSScriptRoot read-tree HEAD; if ($LASTEXITCODE -ne 0) { return $null }
        git -C $PSScriptRoot add -A; if ($LASTEXITCODE -ne 0) { return $null }
        $t = git -C $PSScriptRoot write-tree; if ($LASTEXITCODE -ne 0 -or $t -notmatch '^[0-9a-f]{40}$') { return $null }
        return $t
    }
    finally { Remove-Item Env:GIT_INDEX_FILE -ErrorAction SilentlyContinue; Remove-Item $index -ErrorAction SilentlyContinue }
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

    $treeBefore = if ($EditOnly) { $null } else { Working-Tree }

    Write-Host "`n== validate.py ==" -ForegroundColor Cyan
    python validate.py
    if ($LASTEXITCODE -ne 0) { Write-Host "validate.py failed (exit 5): fix the lines above" -ForegroundColor Red; exit 5 }

    Write-Host "`n== EditMode ==" -ForegroundColor Cyan
    $edit = Run-Tests 'EditMode' @()
    if ($edit -ne 0) { Write-Host (Explain $edit) -ForegroundColor Red; exit $edit }

    if ($EditOnly) { Write-Host "`nEditMode green. PlayMode skipped (-EditOnly)." -ForegroundColor Yellow; exit 0 }

    Write-Host "`n== PlayMode ==" -ForegroundColor Cyan
    $play = Run-Tests 'PlayMode' @('--', '-nographics')
    if ($play -ne 0) { Write-Host (Explain $play) -ForegroundColor Red; exit $play }

    # record the tree only if it is the one the tests started on: an edit during the run was never tested
    $tree = Working-Tree
    $marker = git -C $PSScriptRoot rev-parse --path-format=absolute --git-path tw-gate-green
    if ($null -ne $tree -and $tree -eq $treeBefore) {
        Set-Content -Path $marker -Value "$tree $(Get-Date -Format s)" -Encoding ascii
        Write-Host "`nGate green. Tested tree $($tree.Substring(0, 10)): commit exactly this and Tools/land.py will land it." -ForegroundColor Green
    } else {
        Write-Host "`nGate green, but the files changed during the run (or git could not read them), so no tree is recorded for Tools/land.py. Gate again." -ForegroundColor Yellow
    }
    exit 0
}
finally { Pop-Location }
