# Pre-commit gate. docs/reference/workflow.md, "Gate".
#   ./gate.ps1                    validate + every EditMode test + PlayMode. The only run Tools/land.py accepts.
#   ./gate.ps1 -EditOnly          validate + the EditMode test modules this lane's changes can reach (before a commit)
#   ./gate.ps1 -EditOnly -All     validate + every EditMode test
#   ./gate.ps1 -Module Sim,Match  validate + exactly these EditMode modules (while iterating)
#   ./gate.ps1 -EditOnly -Plan    print what would run and the unity command, and run nothing
#
# Exit codes: 0 green; 8 a test failed; 6 no verdict (compile error, licence, an unknown module, or a scoped run whose
# results do not hold what was asked for); 3 the project is held by an editor or another batch run (close it, or run the
# tests inside it: workflow.md); 5 validate.py failed (its lines are printed); 1 unity.exe is missing; any other code is
# unity's own.
#
# Failures are printed with their message, so you do not need to open test-results.xml.
#
# A green FULL run records the tree it tested (the working tree as it would be committed, untracked files included)
# in tw-gate-green in this checkout's git dir. Tools/land.py lands code only when HEAD is exactly that tree. The tree
# is taken before the tests and again after; if anything changed in between, nothing is recorded.
#
# SCOPE. The EditMode tests are one assembly per module (Assets/_Project/Tests/<Module>), and the sim and match tests
# are nearly all of the run time. -EditOnly asks Tools/gate_scope.py which modules the lane's changes can reach (the
# working tree against the merge-base with origin's integration branch) and skips a slow module only when none can;
# the rule is in that file and deny-by-default. A scoped run never writes tw-gate-green, keeps its results apart
# (test-results-EditMode-scoped.xml) so the last full run's counts stay readable, and is no verdict unless the results
# hold every assembly that was asked for. The full run is never scoped.
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
param([switch]$EditOnly, [switch]$All, [string[]]$Module, [switch]$Plan)

$ErrorActionPreference = 'Continue'
$noise = 'unity-pipeline-port|Failed to handle /api/exec request'
$assertion = 'Expected|But was|Assert'

# How a scoped run tells Unity which tests to run. 'assemblyNames': the editor's own -assemblyNames, passed after `--`.
# 'filter': the unity CLI's --filter with every class name of the chosen modules. Whichever is set, Check-Suites
# below reads the results and refuses a run that did not hold what was asked for.
$SelectBy = 'assemblyNames'

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

# Print what actually ran and keep a copy per label (the next run overwrites test-results.xml). A "pass" that ran no
# tests is no verdict: the EditMode run prints nothing to the console, so without this line green and empty look alike.
function Report-Run($label, $code) {
    if (-not (Test-Path 'test-results.xml')) { Write-Host "$label : no test-results.xml written" -ForegroundColor Red; return 6 }
    Copy-Item 'test-results.xml' "test-results-$label.xml" -Force
    [xml]$x = Get-Content 'test-results.xml' -Raw
    $r = $x.'test-run'
    Write-Host "$label : $($r.total) run, $($r.passed) passed, $($r.failed) failed, $($r.skipped) skipped (test-results-$label.xml)"
    if ($code -eq 0 -and [int]$r.total -eq 0) { Write-Host "$label ran no tests: not a pass." -ForegroundColor Red; return 6 }
    # the xml is the verdict, not unity's exit code (memory: unity-batch-gate-verdicts)
    if ($code -eq 0 -and ([int]$r.failed -gt 0 -or $r.result -ne 'Passed')) { Write-Host "$label : the xml says $($r.result), $($r.failed) failed, though unity exited 0: not a pass." -ForegroundColor Red; return 8 }
    if ($code -eq 0 -and [int]$r.passed -eq 0) { Write-Host "$label : nothing passed ($($r.skipped) skipped): not a pass." -ForegroundColor Red; return 6 }
    return $code
}

# The unity CLI's per-run timeout, in seconds. 900 since 2026-09-29: EditMode ran 576 s of the old 600 on a quiet machine,
# and a run that hits the limit reports no verdict at all, which three lanes' gates did that day. 1800 since 2026-10-01:
# the laptop ran the whole EditMode suite in 1548 s on integration 600c1a79 (819 s the day before; one test alone at its
# usual speed), so four gates in a row ended with no verdict at 900.
$TestTimeout = 1800

# $label names the kept results file (test-results-<label>.xml). $first goes before the `--` (unity CLI options),
# $editor after it (the editor's own arguments). The rerun of noise failures keeps $editor only: --rerun-failed is
# itself the selection, inside what the first run selected.
function Run-Tests($mode, $label, [string[]]$first, [string[]]$editor) {
    $tail = if ($editor.Count) { @('--') + $editor } else { @() }
    Remove-Item 'test-results.xml' -ErrorAction SilentlyContinue
    Remove-Item "test-results-$label.xml" -ErrorAction SilentlyContinue   # a run that writes nothing must not leave the last green copy
    & $unity test . --mode $mode --timeout $TestTimeout @first @tail | Out-Host
    $code = $LASTEXITCODE
    $verdict = Report-Run $label $code       # the run's results stay in test-results-<label>.xml whatever follows
    if ($code -ne 8) { return $verdict }
    $noiseOnly = Show-Failures 'test-results.xml'
    if (-not $noiseOnly) { return $code }
    Write-Host "`nEvery failure is external pipeline noise (see the header of gate.ps1). Rerunning the failed tests once." -ForegroundColor Yellow
    Remove-Item 'test-results.xml' -ErrorAction SilentlyContinue
    & $unity test . --mode $mode --timeout $TestTimeout --rerun-failed @tail | Out-Host
    $code = $LASTEXITCODE
    if ($code -eq 8) { Show-Failures 'test-results.xml' | Out-Null }
    return (Report-Run "$label-rerun" $code)
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

# Ask Tools/gate_scope.py what to run. Its `key: value` lines come back as a table, with its exit code under 'code'.
function Get-Scope([string[]]$scopeArgs) {
    $lines = @(python Tools/gate_scope.py @scopeArgs)
    $s = @{ code = $LASTEXITCODE; lines = $lines }
    foreach ($l in $lines) { if ($l -match '^(\w+): ?(.*)$') { $s[$matches[1]] = $matches[2] } }
    return $s
}

# A scoped run is a verdict only when the results hold every assembly that was asked for and has tests. Fewer: the
# selection did not do what the gate thinks (exit 6). More: Unity ignored the selection and ran a wider set, which
# tested at least as much, so the verdict stands and the line says so.
function Check-Suites($xmlPath, [string[]]$expected) {
    [xml]$x = Get-Content $xmlPath -Raw
    $ran = @($x.SelectNodes('//test-suite[@type="Assembly"]') | ForEach-Object { $_.name -replace '\.dll$', '' })
    $missing = @($expected | Where-Object { $ran -notcontains $_ })
    if ($missing.Count) {
        Write-Host "The results hold no tests from $($missing -join ', ') (they hold: $($ran -join ', ')). The selection did not run what was asked: not a verdict. Run gate.ps1 -EditOnly -All." -ForegroundColor Red
        return 6
    }
    $more = @($ran | Where-Object { $expected -notcontains $_ })
    if ($more.Count) { Write-Host "Unity also ran $($more -join ', '): the selection was not applied, so this run was wider than asked." -ForegroundColor Yellow }
    return 0
}

$proj  = Join-Path $PSScriptRoot 'trench-warfare-3d'
# TW_GATE_UNITY: Tools/selftest.py points the gate at a stand-in that writes canned results, to test the gate's own rules.
$unity = if ($env:TW_GATE_UNITY) { $env:TW_GATE_UNITY } else { Join-Path $env:LOCALAPPDATA 'unity\bin\unity.exe' }
if ($env:TW_GATE_UNITY) { Write-Host "TW_GATE_UNITY is set: this run uses a stand-in for Unity ($unity). It tests the gate, not the game." -ForegroundColor Yellow }
$Module = @($Module | ForEach-Object { "$_" -split ',' } | ForEach-Object { $_.Trim() } | Where-Object { $_ })   # -File passes "Sim,Match" as one string
if ($Module.Count) { $EditOnly = $true }
if (-not $Plan -and -not (Test-Path $unity)) { Write-Host "unity.exe not found at $unity" -ForegroundColor Red; exit 1 }

Push-Location $proj
try {
    if (-not $Plan) {
        python Tools/editor_lock.py guard
        if ($LASTEXITCODE -ne 0) {
            Write-Host "The project is held (an open editor or another batch run). Close it, or run the tests inside it: docs/reference/workflow.md, Gate." -ForegroundColor Red
            exit 3
        }
    }

    $fullRun = -not $EditOnly                # the only run that may record a tree for Tools/land.py
    $treeBefore = Working-Tree

    # What the EditMode pass runs. Anything that goes wrong while working it out runs everything.
    $scoped = $false; $first = @(); $editor = @(); $expected = @(); $note = 'every module (the full gate is never scoped)'
    if ($EditOnly -and -not $All) {
        $scopeArgs = if ($Module.Count) { @('--modules', ($Module -join ',')) } elseif ($treeBefore) { @('--tree', $treeBefore) } else { @('--all') }
        $s = Get-Scope $scopeArgs
        if ($s.code -eq 2) { $s.lines | Out-Host; Write-Host "No verdict (exit 6): nothing ran." -ForegroundColor Red; exit 6 }
        if ($s.code -ne 0 -or -not $s.assemblies) {
            $note = 'every module (Tools/gate_scope.py did not answer)'
        } else {
            $note = $s.note
            if ($s.scoped -eq 'yes') {
                $scoped = $true
                $expected = @($s.expect -split ';' | Where-Object { $_ })
                if ($SelectBy -eq 'filter') { $first = @('--filter', $s.classes) } else { $editor = @('-assemblyNames', $s.assemblies) }
            }
        }
    } elseif ($EditOnly) { $note = 'every module (-All)' }
    $label = if ($scoped) { 'EditMode-scoped' } else { 'EditMode' }
    Write-Host "scope    $note" -ForegroundColor Cyan

    if ($Plan) {
        $tail = if ($editor.Count) { @('--') + $editor } else { @() }
        Write-Host "validate python validate.py"
        Write-Host "EditMode unity test . --mode EditMode --timeout $TestTimeout $(@($first + $tail) -join ' ')"
        Write-Host "results  test-results-$label.xml$(if ($scoped) { '; must hold ' + ($expected -join ', ') })"
        if ($fullRun) { Write-Host "PlayMode unity test . --mode PlayMode --timeout $TestTimeout -- -nographics"; Write-Host "marker   tw-gate-green is written if all of it is green and the tree did not change" }
        else { Write-Host "PlayMode skipped"; Write-Host "marker   not written: only the full gate (no arguments) records a tree for Tools/land.py" }
        exit 0
    }

    Write-Host "`n== validate.py ==" -ForegroundColor Cyan
    python validate.py
    if ($LASTEXITCODE -ne 0) { Write-Host "validate.py failed (exit 5): fix the lines above" -ForegroundColor Red; exit 5 }

    Write-Host "`n== EditMode ==" -ForegroundColor Cyan
    $edit = Run-Tests 'EditMode' $label $first $editor
    if ($edit -eq 0 -and $scoped) { $edit = Check-Suites "test-results-$label.xml" $expected }
    if ($edit -ne 0) { Write-Host (Explain $edit) -ForegroundColor Red; exit $edit }

    if (-not $fullRun) {
        $what = if ($scoped) { "EditMode green for this scope ($note)" } else { "EditMode green" }
        Write-Host "`n$what. PlayMode skipped. Landing needs the full gate (no arguments)." -ForegroundColor Yellow
        exit 0
    }

    Write-Host "`n== PlayMode ==" -ForegroundColor Cyan
    $play = Run-Tests 'PlayMode' 'PlayMode' @() @('-nographics')
    if ($play -ne 0) { Write-Host (Explain $play) -ForegroundColor Red; exit $play }

    # record the tree only if it is the one the tests started on: an edit during the run was never tested
    $tree = Working-Tree
    $marker = git -C $PSScriptRoot rev-parse --path-format=absolute --git-path tw-gate-green
    if ($fullRun -and -not $scoped -and $null -ne $tree -and $tree -eq $treeBefore) {
        Set-Content -Path $marker -Value "$tree $(Get-Date -Format s)" -Encoding ascii
        Write-Host "`nGate green. Tested tree $($tree.Substring(0, 10)): commit exactly this and Tools/land.py will land it." -ForegroundColor Green
    } else {
        Write-Host "`nGate green, but the files changed during the run (or git could not read them), so no tree is recorded for Tools/land.py. Gate again." -ForegroundColor Yellow
    }
    exit 0
}
finally { Pop-Location }
