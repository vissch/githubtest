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

python validate.py *> "$env:TEMP\aosa-validate.txt"; $v = $LASTEXITCODE; Say "validate exit $v"
if ($v -ne 0) { Say "FAILED validate"; exit $v }

& $cli test . --mode EditMode --timeout 900 --output "$env:TEMP\aosa-edit.xml" *> "$env:TEMP\aosa-edit.txt"; $e = $LASTEXITCODE
$sum = Select-String -Path "$env:TEMP\aosa-edit.xml" -Pattern '<test-run[^>]*' -ErrorAction SilentlyContinue | Select-Object -First 1
Say "EditMode exit $e $(if ($sum) { ($sum.Matches[0].Value -replace '.*?(total="\d+").*?(passed="\d+").*?(failed="\d+").*', '$1 $2 $3') })"
if ($e -ne 0) { Say "FAILED EditMode"; Unchurn; exit $e }

if (-not $EditOnly) {
    & $cli test . --mode PlayMode --timeout 900 --output "$env:TEMP\aosa-play.xml" -- -nographics *> "$env:TEMP\aosa-play.txt"; $p = $LASTEXITCODE
    $sum = Select-String -Path "$env:TEMP\aosa-play.xml" -Pattern '<test-run[^>]*' -ErrorAction SilentlyContinue | Select-Object -First 1
    Say "PlayMode exit $p $(if ($sum) { ($sum.Matches[0].Value -replace '.*?(total="\d+").*?(passed="\d+").*?(failed="\d+").*', '$1 $2 $3') })"
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
