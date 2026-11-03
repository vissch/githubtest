# Registers the scheduled task "TW Supervisor" the way "TW Autoloop" is registered on the desktop: start-supervisor.ps1
# every 10 minutes (it starts one only when none runs), plus "TW Supervisor logon" at logon, so the supervisor comes
# back after a reboot.
#
# WITHOUT -Really IT CHANGES NOTHING: it prints the two schtasks commands it would run. Run it with -Really only on
# the owner's word, after the switch-over order in docs/reference/pipelines.md (the supervisor row) has reached the
# step that needs it. It never deletes, changes or runs another task, and never starts the supervisor itself.
#
# Undo: schtasks /delete /tn "TW Supervisor" /f ; schtasks /delete /tn "TW Supervisor logon" /f
param(
    [string]$Script = 'C:\Users\PC\Documents\GitHub\githubtest-supervisor\trench-warfare-3d\Tools\supervisor\ops\start-supervisor.ps1',
    [switch]$Really
)
$run = "powershell -NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File `"$Script`""
$start = (Get-Date).AddMinutes(2).ToString('HH:mm')
$every = @('/create', '/tn', 'TW Supervisor', '/tr', $run, '/sc', 'minute', '/mo', '10', '/st', $start)
$logon = @('/create', '/tn', 'TW Supervisor logon', '/tr', $run, '/sc', 'onlogon')

foreach ($name in 'TW Supervisor', 'TW Supervisor logon') {
    $null = schtasks /query /tn $name 2>$null
    if ($LASTEXITCODE -eq 0) { "'$name' exists already: left as it is (delete it first to register it again)"; exit 1 }
}
if (-not (Test-Path $Script)) { "no $Script : make the githubtest-supervisor checkout first (start-supervisor.ps1 says how)"; exit 1 }
if (-not $Really) {
    'would run:'
    'schtasks ' + (($every | ForEach-Object { if ($_ -match ' ') { "`"$_`"" } else { $_ } }) -join ' ')
    'schtasks ' + (($logon | ForEach-Object { if ($_ -match ' ') { "`"$_`"" } else { $_ } }) -join ' ')
    'nothing registered: run again with -Really on the owner''s word'
    exit 0
}
schtasks @every
schtasks @logon
