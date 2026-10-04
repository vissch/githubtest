---
name: relay
description: Run Trench Warfare 3D work as a relay - a chain of short headless Claude sessions (legs) that plan, then execute, so no session fills its context. Start a run, see what it is doing, stop it, queue lane work for it, or dry-run to see what it would pick. Use for "/relay start", "/relay status", "/relay stop", "/relay add", "/relay dry", "run the relay for N hours", "why did the relay stop". NOT for taking one pipeline job by hand (pipeline skill) and NOT for landing (tw-master).
---

# Relay

The runner is a script. It picks the work, starts one leg at a time, checks the result and decides to go on. You only
start it, read its lines and tell the owner in short plain words (he skims: answer first, a few bullets).

```bash
R="python C:/Users/PC/Documents/GitHub/githubtest-relay/trench-warfare-3d/Tools/relay/relay.py"
WORK="C:/Users/PC/Documents/GitHub/githubtest-relay-work"
```
The work checkout is the relay's own. Never point `--work` at a checkout a session or an editor is using.

| The owner says | Run | Then |
|---|---|---|
| `/relay dry` | `$R run --work $WORK --dry-run` | say the one unit it would take |
| `/relay start` | `$R run --work $WORK --hours <H> [--max-legs N] [--sources pipeline,lane]` in the background | say it started and what it took; report the `STOP:` line when it ends |
| `/relay status` | `$R status` | one or two lines |
| `/relay stop` | `$R stop` (ends before the next leg) or `$R stop --now` (ends the leg too) | confirm with `$R status` |
| `/relay add` | `$R add <id> --lane lane/show/<x> --goal "<the owner's words>" --done-when <program> <arg> ...` | say it is queued |
| watch a leg | `$R view "<leg folder>" --follow` (status prints the folder) | |

## Rules
- Ask for the hours if the owner gave none. `--hours` is clamped to 0.25-12 (default 3). `--max-legs` caps the legs;
  two legs is one unit (plan, execute), so leave it off for a long run.
- A leg with an edit gate takes 30 minutes or more. Do not poll: the runner ends with one `STOP:` line.
- `done_when` for `/relay add` is a command that exits 0 when the work is there, as words, not one string. No shell.
- The runner stops on anything it cannot trust. Read the `STOP:` line and the last file under
  `tw3d-board/relay/<station>/stops/`; say the reason as it is, do not guess.
- "the work checkout cannot be used": it names why (missing, uncommitted files, a Unity editor open, git activity in
  the last 10 minutes: `--no-quiet` skips that last check when you know nobody is in it).
- Do not edit `Tools/relay/` or `Tools/pipeline/` while a run is going: the runner sees its own code change and stops.
- Legs never land. Landing stays the owner's word (tw-master).
- Leg folders and logs: `%LOCALAPPDATA%\TrenchWarfare\relay\runs\<run>\`. Tests: `python Tools/relay/test_relay.py`.
