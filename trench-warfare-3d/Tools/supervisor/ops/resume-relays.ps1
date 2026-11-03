# Resume after pause-relays.ps1: take autoloop.stop away, so the loop's next round (the autoloop, or the supervisor's
# loop_feed once it is switched on) feeds and starts the relays again under the day's budget. The desktop's
# %TEMP%\daypct-reset.ps1 did this and also killed the loop and reset TW_DAY_PCT; this one does neither: the loop
# reads the stop file every round, so no restart is needed, and the day's figure is budget.py's.
#
#   resume-relays.ps1 -Why "owner: go again"
#
# It starts no relay itself, ends no process and forces nothing.
param(
    [string]$Why = 'resume',
    [string]$By = 'resume-relays.ps1'
)
$dir = Join-Path $env:LOCALAPPDATA 'TrenchWarfare\autoloop'
$stop = Join-Path $dir 'autoloop.stop'
if (-not (Test-Path $stop)) { 'not paused: no autoloop.stop'; exit 0 }
$was = (Get-Content $stop -Raw -ErrorAction SilentlyContinue)
Remove-Item $stop
Add-Content (Join-Path $dir 'autoloop.log') ('{0} resume ({1}): {2}; stand-by lifted (it said: {3})' -f (Get-Date -Format 'MM-dd HH:mm:ss'), $By, $Why, "$was".Trim()) -Encoding utf8
'stand-by lifted: the next round feeds and starts the relays'
