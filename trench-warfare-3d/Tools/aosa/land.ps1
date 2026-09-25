# AOSA lander's gate + build, background only (owner rule 2026-09-25): every Unity process is -batchmode, at
# below-normal priority (children inherit the class), and shows no window. Writes a summary to -Log.
#   powershell -NoProfile -ExecutionPolicy Bypass -File Tools/aosa/land.ps1 [-EditOnly] [-NoPlay] [-NoBuild] [-Log <path>]
# Exit 0 = everything asked for passed. Exit 6 from a test run = no verdict (compile error), which is NOT a pass.
param([switch]$EditOnly, [switch]$NoBuild, [string]$Log = "$env:TEMP\aosa-land.log")

$ErrorActionPreference = 'Continue'
$proj = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)          # trench-warfare-3d
$cli = Join-Path $env:LOCALAPPDATA 'unity\bin\unity.exe'
$editor = 'C:\Program Files\Unity\Hub\Editor\6000.0.50f1\Editor\Unity.exe'
(Get-Process -Id $PID).PriorityClass = 'BelowNormal'                  # inherited by everything started below
function Say($m) { "$(Get-Date -Format HH:mm:ss) $m" | Out-File $Log -Append -Encoding utf8 }
"" | Out-File $Log -Encoding utf8
Set-Location $proj
$churn = @('Assets/UniversalRenderPipelineGlobalSettings.asset', 'Assets/_Project/Settings/TW-URP.asset', 'ProjectSettings/GraphicsSettings.asset')
function Unchurn { git checkout -- $churn 2>$null; if (-not (git ls-files Assets/Resources)) { Remove-Item -Recurse -Force Assets/Resources, Assets/Resources.meta -ErrorAction SilentlyContinue } }  # + the perf-test package's run files

# Owner decision A08 (2026-09-25): this worktree's editors never start the com.unity.pipeline server, so other sessions'
# MCP calls cannot land in them. The switch is an untracked, never-committed asset; no asset, no editor.
if (-not (Test-Path 'Assets/_AosaLocal/PipelineServerOff.asset')) { Say "FAILED: Assets/_AosaLocal/PipelineServerOff.asset missing (A08); no editor started"; exit 9 }

python validate.py *> "$env:TEMP\aosa-validate.txt"; $v = $LASTEXITCODE; Say "validate exit $v"
if ($v -ne 0) { Say "FAILED validate"; exit $v }

# One test run of a mode. The only failures ever excused are the environment's, never the game's (LESSONS, cycles 2
# and 3): the CLI's own /api/exec request timing out while a long test holds the main thread, or -nographics refusing a
# view. They arrive as "Unhandled log message", which Unity checks after the test body, so the test's own assertions
# passed (a failed assertion would be the message instead). Such a run is repeated once; twice in a row it is accepted
# with a note naming the tests. Any other failure fails the land.
function Run-Tests($mode, $xmlPath, $extra) {
    foreach ($try in 1, 2) {
        & $cli test . --mode $mode --timeout 900 --output $xmlPath @extra *> "$xmlPath.txt"; $rc = $LASTEXITCODE
        $sum = Select-String -Path $xmlPath -Pattern '<test-run[^>]*' -ErrorAction SilentlyContinue | Select-Object -First 1
        Say "$mode exit $rc $(if ($sum) { ($sum.Matches[0].Value -replace '.*?(total="\d+").*?(passed="\d+").*?(failed="\d+").*', '$1 $2 $3') })"
        if ($rc -eq 0) { return 0 }
        $xml = Get-Content $xmlPath -Raw -ErrorAction SilentlyContinue
        $fails = @([regex]::Matches("$xml", '<test-case [^>]*result="Failed".*?</test-case>', 'Singleline'))
        $envFails = @($fails | Where-Object { $_.Value -match 'Unhandled log message: .\[Error\] (Failed to handle /api/exec request|No graphic device is available)' })
        if ($fails.Count -eq 0 -or $envFails.Count -ne $fails.Count) { return $rc }
        if ($try -eq 1) { Say "${mode}: all $($fails.Count) failure(s) are environment signatures; rerunning once"; continue }
        $names = ($envFails | ForEach-Object { if ($_.Value -match 'fullname="([^"]+)"') { $Matches[1] } }) -join ', '
        Say "$mode ENV-ONLY twice, assertions passed, accepted with a note: $names"
        return 0
    }
}

Remove-Item "$env:TEMP\aosa-edit.xml", "$env:TEMP\aosa-play.xml" -ErrorAction SilentlyContinue   # never read a stale report
# -logFile: without it a batch editor writes %LOCALAPPDATA%\Unity\Editor\Editor.log, the owner's own editor's log (cycle 3)
$e = Run-Tests 'EditMode' "$env:TEMP\aosa-edit.xml" @('--', '-logFile', "$env:TEMP\aosa-editmode-editor.log")
if (Select-String -Path "$env:TEMP\aosa-editmode-editor.log" -Pattern 'Pipeline Server started' -Quiet -ErrorAction SilentlyContinue) { Say "WARNING: the pipeline server started despite A08's switch" }
if ($e -ne 0) { Say "FAILED EditMode"; Unchurn; exit $e }

if (-not $EditOnly) {
    $p = Run-Tests 'PlayMode' "$env:TEMP\aosa-play.xml" @('--', '-nographics', '-logFile', "$env:TEMP\aosa-playmode-editor.log")
    if ($p -ne 0) { Say "FAILED PlayMode"; Unchurn; exit $p }
}

if (-not $NoBuild) {
    foreach ($dev in @($false, $true)) {
        $ba = @('-batchmode', '-quit', '-projectPath', "`"$proj`"", '-executeMethod', 'TW.Editor.BuildWindows.CommandLine', '-logFile', "`"$env:TEMP\aosa-build-$(if ($dev) {'dev'} else {'rel'}).log`"")
        if ($dev) { $ba += '-twdev' }
        $b = Start-Process -FilePath $editor -ArgumentList $ba -Wait -PassThru -NoNewWindow   # -batchmode: no window
        $st = Get-Content (Join-Path $proj "Builds/$(if ($dev) {'WinBenchDev'} else {'WinBench'})/build-status.txt") -ErrorAction SilentlyContinue
        Say "build $(if ($dev) {'dev'} else {'release'}) exit $($b.ExitCode): $st"
        if ($b.ExitCode -ne 0) { Unchurn; Say "FAILED build"; exit 1 }
    }
}
Unchurn          # URP rewrites its prefilter fields on every build: churn, never a change
Say "LANDED OK"
exit 0
