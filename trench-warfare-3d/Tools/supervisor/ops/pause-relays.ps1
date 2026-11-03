# Pause the relays: the loop stands by and every relay stops before its next leg. Moved into the repo from the desktop's
# %TEMP%\pause-relays.ps1 (the owner, 2026-10-19 ~22:10: "no more relay work until 00:01"), which the stop tasks
# (stop-0900.ps1, stop-0940.ps1, "TW Stop ...") call.
#
#   pause-relays.ps1 -Why "owner: pause until 00:01"         resume with resume-relays.ps1
#
# What it does: writes autoloop.stop in the loop's home (the autoloop and every supervisor loop_* module stand by on
# it, so nothing is fed or started), then asks each relay, through relay.py's own `stop`, to stop before its next leg
# (a leg in flight finishes; nothing is ended or forced). It never ends a process, never touches a Unity or a checkout
# (empire-og-client or any other), and never changes the day's figure: that is budget.py's.
param(
    [Parameter(Mandatory = $true)][string]$Why,
    [string]$By = 'pause-relays.ps1',
    [string]$GH = 'C:/Users/PC/Documents/GitHub',
    [string]$Python = 'python',
    [int[]]$Relays = @(1, 2, 3, 4, 5, 6)
)
$dir = Join-Path $env:LOCALAPPDATA 'TrenchWarfare\autoloop'
New-Item -ItemType Directory -Force $dir | Out-Null
Set-Content -Path (Join-Path $dir 'autoloop.stop') -Value ('{0} {1}: {2}' -f (Get-Date -Format 's'), $By, $Why) -Encoding utf8
$R = "$GH/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py"
$keep = @{ TW_BOARD = $env:TW_BOARD; TW_RELAY_HOME = $env:TW_RELAY_HOME }
try {
    foreach ($n in $Relays) {
        if ($n -eq 1) { $env:TW_BOARD = "$GH/tw3d-board"; $env:TW_RELAY_HOME = "$env:LOCALAPPDATA/TrenchWarfare/relay" }
        else { $env:TW_BOARD = "$GH/tw3d-board-$n"; $env:TW_RELAY_HOME = "$env:LOCALAPPDATA/TrenchWarfare/relay$n" }
        if (-not (Test-Path $env:TW_BOARD)) { "== relay $n : no board, skipped"; continue }
        "== relay $n"
        & $Python $R stop --why $Why --by $By 2>&1 | Select-Object -Last 1
    }
} finally {
    $env:TW_BOARD = $keep.TW_BOARD; $env:TW_RELAY_HOME = $keep.TW_RELAY_HOME
}
Add-Content (Join-Path $dir 'autoloop.log') ('{0} pause ({1}): {2}; loop on stand-by, relays stop before their next leg' -f (Get-Date -Format 'MM-dd HH:mm:ss'), $By, $Why) -Encoding utf8
