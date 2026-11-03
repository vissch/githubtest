# Starts the supervisor (Tools/supervisor/main.py --watch 60) detached, when none runs. The pattern of the desktop's
# %TEMP%\start-autoloop.ps1, which "TW Autoloop" runs at logon and every 10 minutes.
#
# Safe to run as often as you like: main.py --watch keeps one copy a home by its own lock (lock.json + beat.json in
# the supervisor home, %LOCALAPPDATA%\TrenchWarfare\supervisor); this script also looks for a live one first.
# It runs from a detached checkout of its own, githubtest-supervisor, so no lane work moves the code under it:
#   git -C C:\Users\PC\Documents\GitHub\githubtest-autoloop worktree add --detach ..\githubtest-supervisor <sha>
# (deploy a new sha there with: git -C ...\githubtest-supervisor switch --detach <sha>, then let the old one leave:
#  write supervisor.stop, wait a pass, delete lock.json only when its pid is gone, delete supervisor.stop, run this).
#
# Stop it: create supervisor.stop in the supervisor home (it stands by), or disable the task "TW Supervisor".
# This script never ends a process, never forces anything, and touches no checkout but reading its path.
param(
    [string]$Checkout = 'C:\Users\PC\Documents\GitHub\githubtest-supervisor',
    [string]$Python = 'python',
    [int]$Every = 60
)
$ErrorActionPreference = 'Stop'
$home_ = Join-Path $env:LOCALAPPDATA 'TrenchWarfare\supervisor'
New-Item -ItemType Directory -Force $home_ | Out-Null
$log = Join-Path $home_ 'starts.log'
function Say($t) { Add-Content -Path $log -Value ('{0} start-supervisor: {1}' -f (Get-Date -Format 's'), $t) -Encoding utf8 }

$main = Join-Path $Checkout 'trench-warfare-3d\Tools\supervisor\main.py'
if (-not (Test-Path $main)) { Say "no $main : not started (make the checkout first, see the head of this script)"; exit 1 }

$alive = @(Get-CimInstance Win32_Process | Where-Object {
    $_.Name -like 'python*' -and $_.CommandLine -match 'supervisor[\\/]+main\.py' -and $_.CommandLine -match '--watch' })
if ($alive.Count -gt 0) { exit 0 }

$beat = Join-Path $home_ 'beat.json'
if (Test-Path $beat) {
    try { $age = ((Get-Date).ToUniversalTime() - [DateTimeOffset]::FromUnixTimeSeconds([int64](Get-Content $beat -Raw | ConvertFrom-Json).at).UtcDateTime).TotalSeconds } catch { $age = 1e9 }
    if ($age -lt 120) { exit 0 }      # beat within two passes: one runs that the process list did not show
}

# its own output goes to supervisor.out.log in the home (the modules' lines go to supervisor.log by core.log)
$out = Join-Path $home_ 'supervisor.out.log'
$cmd = "cmd /c `"`"$Python`" -u `"$main`" --watch $Every >> `"$out`" 2>&1`""
$r = Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{ CommandLine = $cmd; CurrentDirectory = (Join-Path $Checkout 'trench-warfare-3d') }
Say ('create said {0}, pid {1} (cmd; python is its child), checkout {2}' -f $r.ReturnValue, $r.ProcessId, $Checkout)
