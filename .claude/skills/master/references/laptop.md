# The master on the laptop

For a session on the laptop (host `MSI`). The skill itself is written for the desktop, where runs start.

The laptop has no run copy and no work checkout, so here you can look and report, not start or stop a run.
Its `Documents/GitHub/tw3d-board` is behind origin and holds another session's uncommitted work: never read
numbers from its files, never commit or push it. Read a snapshot of origin instead:

```bash
B=C:/Users/thomas.visscher_magi/Documents/GitHub/tw3d-board
SNAP="$TEMP/tw-board-snap"; rm -rf "$SNAP"; mkdir -p "$SNAP/board" "$SNAP/home"
git -C $B fetch -q origin && git -C $B archive origin/main | tar -x -C "$SNAP/board"
export TW_STATION=laptop TW_BOARD="$SNAP/board" TW_RELAY_HOME="$SNAP/home"
R="python C:/Users/thomas.visscher_magi/Documents/GitHub/githubtest-relay-dev/trench-warfare-3d/Tools/relay/relay.py"
```
`$R day`, `$R budget`, `$R status`, `$R refusals` and `$R agents` are fine on the snapshot. `$R add` and `$R prio`
commit and push the board, and `$R run`, `$R stop` and `$R hold` act on this machine: on the laptop say what you
would do and that it has to be done on the desktop (`ssh emtd-desktop`, only on the owner's word).

**The cap and the pace on this screen are worked out by the laptop's copy of the scripts.** Runs happen on the
desktop, and the desktop holds the 11% and the pace only once its frozen copy has them. Look before you say so
(reading over ssh needs no word from the owner):

```bash
ssh emtd-desktop "python C:/Users/PC/Documents/GitHub/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py day"
```
A `Pace:` line there means the desktop holds both. No such line: tell the owner the desktop still stops at its old
$50 day (about 2.7%) and has no pace, whatever this screen says ("Older scripts" below).

**The laptop's own agents** count toward the day ("The day's budget"). When `$R agents` shows any, hand them over
as a file, the way a unit goes (`<name>` is the file's name; the desktop's relay must be the 2026-10-07 one or newer):

```bash
$R agents book --out "$TEMP/agents-laptop.json" --who <your session name>
scp "$TEMP/agents-laptop.json" emtd-desktop:C:/Users/PC/AppData/Local/Temp/
ssh emtd-desktop "python C:/Users/PC/Documents/GitHub/githubtest-relay-dev/trench-warfare-3d/Tools/relay/relay.py agents book --file C:/Users/PC/AppData/Local/Temp/agents-laptop.json"
```
Read `booked laptop ...` or `A run is going here: it adds this ...` in what it prints.

**The owner's answers on the Decide page ("Decisions").** On the laptop:

```bash
B="python C:/Users/thomas.visscher_magi/Documents/GitHub/githubtest-frog-house/trench-warfare-3d/Tools/assetboard/briefs.py"
```
`$B waiting`, `$B unit` and `$B take` are done here: the laptop is where he clicks, so it sees his notes first. The
unit is queued on the desktop's board, and it goes there as a file (quoted words do not survive ssh):

```bash
scp FILE emtd-desktop:C:/Users/PC/AppData/Local/Temp/
ssh emtd-desktop "git -C C:/Users/PC/Documents/GitHub/githubtest-relay-dev pull -q --ff-only; python C:/Users/PC/Documents/GitHub/githubtest-relay-dev/trench-warfare-3d/Tools/relay/relay.py add --unit C:/Users/PC/AppData/Local/Temp/<the file's name> --role <role>"
```
Read `Board: pushed` in what it prints. His answer on a brief is the word this needs (the owner, 2026-10-06 evening):
for `queue` the unit the option names, for `write` the unit you write from his answer. Nothing else is queued from here.

