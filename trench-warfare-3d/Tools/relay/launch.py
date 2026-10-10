#!/usr/bin/env python3
"""Starts one leg and owns it: the runner is the parent of the headless Claude session, so it holds the timeout and
kills the whole tree, also when the runner itself is interrupted. A Windows Terminal tab only shows the leg's output.

  make_leg(...)            folders, compiled prompt, card and hooks for one leg
  run_leg(d, lim, secs)    start, log every output line, stop on timeout or a compaction trip, record how it ended;
                           a working leg that ends with no RESULT line gets one more turn in the same session
  said(report)             done | blocked | failed, from the report's RESULT line
  open_view(d)             a Windows Terminal tab that follows the leg's output (run --view)
  ran_clean(leg)           why the leg cannot be trusted (it did not run under the hooks, in auto mode, to the end)
TW_RELAY_CLAUDE (a JSON list) replaces the claude executable: the tests use a stub. Stdlib only. ASCII only.
"""
import hashlib, json, os, re, shutil, subprocess, sys, threading, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, proc_start          # noqa: E402
from run_detached import kill_tree            # noqa: E402
import config                                 # noqa: E402
import legdir                                 # noqa: E402
import prompt                                 # noqa: E402
import relay_hook                             # noqa: E402
import usage                                  # noqa: E402

POLL_S = 2
BAD_END = ("TIMEOUT", "COMPACT", "STOPPED")
VERDICT = re.compile(r"^\W*RESULT\W+(done|blocked|failed)\b", re.I | re.M)
RESUME, RESUME_MIN_S = "resume.txt", 120
NUDGE = ("Your turn ended, and with it the leg, with no report: no line that starts with RESULT. Nobody wakes a leg "
         "and nothing of yours runs in the background. If you were waiting for something, look at it now with one "
         "foreground call (leg gate wait, leg play, or the job's log). Then finish what your leg card asks, or say "
         "why you cannot, and end with the report in its shape. This is the one extra turn a leg gets.\n")


def said(report):
    """done | blocked | failed, from the first line of a leg's report that starts with RESULT (markdown around it is
    fine); else None. Words before that line do not hide it: rv-12-hud-f9's last leg (2026-10-08) wrote "Leg
    complete." above "RESULT: done", and a finished, pushed unit was recorded as failed."""
    m = VERDICT.search(report or "")
    return m.group(1).lower() if m else None


def no_window():
    """True when this process cannot open a window (Windows session 0: a service, a remote or elevated shell). A
    windowed Unity editor hangs there; batch mode works. TW_RELAY_NO_WINDOW=1|0 says it outright (the tests)."""
    said = os.environ.get("TW_RELAY_NO_WINDOW")
    if said in ("0", "1"):
        return said == "1"
    if os.name != "nt":
        return False
    import ctypes
    sid = ctypes.c_ulong(1)
    ok = ctypes.windll.kernel32.ProcessIdToSessionId(os.getpid(), ctypes.byref(sid))
    return bool(ok) and sid.value == 0


def claude_cmd():
    stub = os.environ.get("TW_RELAY_CLAUDE")
    if stub:
        return json.loads(stub)
    exe = shutil.which("claude")
    if not exe:
        raise SystemExit("relay: claude is not on PATH")
    return [exe]


def prepare(d, system, card, prompt_text):
    d = Path(d)
    fwd = lambda p: str(p).replace("\\", "/")
    hooks = (HERE / "relay-hooks.json").read_text(encoding="utf-8")
    hooks = hooks.replace("{PY}", fwd(sys.executable)).replace("{HOOK}", fwd(HERE / "relay_hook.py")).replace("{LEG}", fwd(d))
    for name, text in ((legdir.SYSTEM, system), (legdir.CARD, card), (legdir.PROMPT, prompt_text), (legdir.HOOKS, hooks)):
        (d / name).write_text(text, encoding="utf-8", newline="\n")


def make_leg(run, nn, unit, phase, worktree, lane, board, body, lim=None, plan=None, model=None):
    """One leg, ready to start: folders, compiled prompt, card, hooks. The same path for every source and phase."""
    lim = lim or config.limits()
    ph = dict(config.phases()[phase])
    ph.update(config.route(unit["role"], phase, unit=unit.get("id")))   # routes.json: these legs run otherwise
    if model:
        ph["model"] = model
    d = legdir.new_leg(run, nn, unit, phase, ph, lim, worktree, lane, board)
    prepare(d, prompt.system_text(unit["role"], phase, config.style(), lim["prompt_max_bytes"]),
            prompt.card_text(legdir.read(d), body, plan), "Read your leg card and do the leg.\n")
    return d


def argv(d, leg, lim):
    d = Path(d)
    a = claude_cmd() + ["-p", "--output-format", "stream-json", "--verbose", "--name",
                        "relay-%s-%02d" % (leg["run"], leg["leg"]), "--model", leg["model"], "--effort", leg["effort"],
                        "--permission-mode", "auto", "--permission-prompts", "none",
                        "--disallowedTools", "AskUserQuestion", "--strict-mcp-config",
                        "--append-system-prompt-file", str(d / legdir.SYSTEM), "--settings", str(d / legdir.HOOKS),
                        "--autocompact", "%dk" % (lim["autocompact_tokens"] // 1000), "--add-dir", leg["desk"]]
    if leg.get("board"):
        a += ["--add-dir", leg["board"]]
    if lim.get("leg_budget_usd"):
        a += ["--max-budget-usd", str(lim["leg_budget_usd"])]
    return a


# Nobody wakes a leg: the end of its turn is the end of the leg. Claude Code moves a command that outlasts its time
# limit to the background and says "you will be notified", and a leg that believes it ends its turn to wait
# (rv-12-hud-f9, 2026-10-08). With background tasks off such a command stops with an error the leg must act on, and
# `run_in_background` is gone. One call may last ten minutes, so `leg gate wait` and `leg play` (540 s) fit in one.
NO_WAKING = {"CLAUDE_CODE_DISABLE_BACKGROUND_TASKS": "1", "BASH_DEFAULT_TIMEOUT_MS": "600000",
             "BASH_MAX_TIMEOUT_MS": "600000"}


def leg_env(d, leg):
    """The leg is not told where its guard folder is. TW_RELAY makes land.py and pipeline.py refuse inside the leg;
    the leg's detached jobs (gate, bench) go under its desk, so it can read their logs and the runner can stop them."""
    env = dict(os.environ, TW_RELAY="1", TW_WORKER_PID=str(os.getpid()), TW_RUNS=str(Path(leg["desk"]) / "jobs"),
               PYTHONUTF8="1", PYTHONDONTWRITEBYTECODE="1", **NO_WAKING)
    env.pop("TW_RELAY_LEG", None)
    if leg.get("board"):
        env["TW_BOARD"] = leg["board"]
    return env


def open_view(d):
    """A Windows Terminal tab that follows the leg's output (relay.py view --follow). It is only a viewer: closing
    it does nothing to the leg, and a tab that cannot open is a note, never an error. Nothing that varies passes
    through wt or cmd quoting: the command is a file in the leg's folder and the title is plain letters.
    TW_RELAY_WT (a JSON list) replaces wt.exe: the tests use a stub."""
    d = Path(d)
    leg = legdir.read(d)
    cmd = d / "view.cmd"
    cmd.write_text('@echo off\r\n"%s" "%s" view "%s" --follow\r\n' % (sys.executable, HERE / "relay.py", d),
                   encoding="utf-8", newline="")
    title = re.sub(r"[^A-Za-z0-9 -]", "", "leg %02d %s %s" % (leg["leg"], leg["phase"], leg["unit"]))[:40]
    wt = json.loads(os.environ["TW_RELAY_WT"]) if os.environ.get("TW_RELAY_WT") else ["wt.exe"]
    try:
        subprocess.Popen(wt + ["-w", "tw-relay", "new-tab", "--title", title, "cmd.exe", "/d", "/c", str(cmd)],
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return True
    except OSError as e:
        print("note: no viewer tab for leg %02d (%s)" % (leg["leg"], e), flush=True)
        return False


def notify(text):
    """A Windows notification that the run stopped, for an owner who is not watching the terminal. Never an error:
    a notification that cannot show is skipped. The text travels in a variable, not on a command line.
    TW_RELAY_NOTIFY (a JSON list) replaces the command: the tests use a stub."""
    ps = ("Add-Type -AssemblyName System.Windows.Forms; Add-Type -AssemblyName System.Drawing; "
          "$n = New-Object System.Windows.Forms.NotifyIcon; $n.Icon = [System.Drawing.SystemIcons]::Information; "
          "$n.Visible = $true; $n.ShowBalloonTip(10000, 'TW3D relay', $env:TW_RELAY_TOAST, "
          "[System.Windows.Forms.ToolTipIcon]::Info); Start-Sleep -Seconds 8; $n.Dispose()")
    cmd = json.loads(os.environ["TW_RELAY_NOTIFY"]) if os.environ.get("TW_RELAY_NOTIFY") else \
        ["powershell", "-NoProfile", "-WindowStyle", "Hidden", "-Command", ps]
    try:
        subprocess.Popen(cmd, env=dict(os.environ, TW_RELAY_TOAST=str(text)[:200]), stdin=subprocess.DEVNULL,
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return True
    except OSError:
        return False


def stop_jobs(d):
    """Stop every detached job the leg started: the tree kill does not reach a process whose parent has exited."""
    jobs = legdir.desk(d) / "jobs"
    for run in (sorted(jobs.iterdir()) if jobs.is_dir() else []):
        subprocess.run([sys.executable, str(HERE.parent / "pipeline" / "run_detached.py"), "stop", run.name],
                       env=dict(os.environ, TW_RUNS=str(jobs)), capture_output=True)


def unity_rows():
    """Every Unity process on this machine as (pid, when it began, its command line). [] where it cannot be read."""
    if os.name != "nt":
        return []
    ask = ("Get-CimInstance Win32_Process -Filter \"Name='Unity.exe'\" | ForEach-Object { '{0}|{1}|{2}' -f "
           "$_.ProcessId, ([DateTimeOffset]$_.CreationDate).ToUnixTimeSeconds(), $_.CommandLine }")
    try:
        out = subprocess.run(["powershell", "-NoProfile", "-Command", ask], capture_output=True, text=True,
                             stdin=subprocess.DEVNULL, encoding="utf-8", errors="replace", timeout=60).stdout
    except (OSError, subprocess.TimeoutExpired):
        return []
    rows = []
    for line in out.splitlines():
        parts = line.split("|", 2)
        if len(parts) == 3 and parts[0].isdigit() and parts[1].isdigit():
            rows.append((int(parts[0]), int(parts[1]), parts[2]))
    return rows


def leftover_unity(rows, worktree, since):
    """The pids among `rows` of a batch-mode Unity that has this leg's checkout open and began after the leg did.
    The tree kill does not reach a process whose parent has exited, and a Unity is no job of run_detached: leg 35
    of run 20261010-103442-62944 was cut at its limit at 18:24 and its film capture (started 17:58, its shell long
    gone) held the work checkout until midnight; six starts ended with no leg. An editor WINDOW is never one of
    these, nor a Unity on another checkout, nor one that was there before the leg."""
    want = {os.path.normcase(os.path.normpath(str(worktree))),
            os.path.normcase(os.path.normpath(os.path.join(str(worktree), "trench-warfare-3d")))}
    out = []
    for pid, began, cmd in rows:
        m = re.search(r'-projectpath\s+"?([^"]+?)"?(?:\s+-|\s*$)', cmd, re.I)
        if (m and "-batchmode" in cmd.lower() and began >= since - 5
                and os.path.normcase(os.path.normpath(m.group(1).strip())) in want):
            out.append(pid)
    return out


def sweep_unity(leg, since):
    """Stop what leftover_unity finds, once the leg is over. Returns the pids."""
    left = leftover_unity(unity_rows(), leg["worktree"], since)
    for pid in left:
        kill_tree(pid)
    return left


SEALED = ("mode", "lane", "desk", "board", "output", "amber_tokens", "red_tokens", "worktree", "phase",
          "deny_tools")


def guard_hash(d):
    """A seal over what the leg is held to: its prompt, card, hooks and the fixed fields of leg.json."""
    h = hashlib.sha256()
    for name in (legdir.SYSTEM, legdir.CARD, legdir.HOOKS, legdir.PROMPT):
        h.update((Path(d) / name).read_bytes())
    leg = legdir.read(d)
    h.update(json.dumps({k: leg.get(k) for k in SEALED}, sort_keys=True).encode())
    return h.hexdigest()


def lines(path):
    try:
        with open(path, "rb") as f:
            return sum(1 for _ in f)
    except OSError:
        return 0


def tool_uses(d):
    """How many tool calls the session made, by its own output."""
    return sum(1 for e in records(d, "assistant") for c in (e.get("message") or {}).get("content") or []
               if isinstance(c, dict) and c.get("type") == "tool_use")


def unguarded(d):
    """Names of the tool calls the session made that the guard never saw: a call in the output with no line in
    calls.jsonl. A call Claude Code itself refused before running it (its result is a <tool_use_error>, as for
    `sleep 60; ..`) never reaches a hook and never ran, so it is not one. None when the calls carry no ids
    (then the two are compared by count)."""
    used, harness = {}, set()
    for e in records(d, "assistant"):
        for c in (e.get("message") or {}).get("content") or []:
            if isinstance(c, dict) and c.get("type") == "tool_use":
                used[c.get("id")] = c.get("name")
    for e in records(d, "user"):
        content = (e.get("message") or {}).get("content")
        for c in content if isinstance(content, list) else []:
            if isinstance(c, dict) and c.get("type") == "tool_result" and c.get("is_error"):
                body = c.get("content")
                text = body if isinstance(body, str) else " ".join(
                    str(x.get("text", "")) for x in body or [] if isinstance(x, dict))
                if text.lstrip().startswith("<tool_use_error>"):
                    harness.add(c.get("tool_use_id"))
    seen = set()
    try:
        with open(Path(d) / "calls.jsonl", encoding="utf-8") as f:
            seen = {json.loads(line).get("id") for line in f if line.strip()}
    except (OSError, ValueError):
        pass
    if None in used or None in seen:
        return None
    return [name for i, name in used.items() if i not in seen and i not in harness]


def _pump(stream, path):
    with open(path, "ab") as f:
        for line in iter(stream.readline, b""):
            f.write(line)
            f.flush()


def records(d, kind, subtype=None):
    """The session's own records of one type from out.jsonl (init at the start, result at the end)."""
    out = []
    try:
        with open(Path(d) / legdir.OUT, "rb") as f:
            for line in f:
                if ('"type":"%s"' % kind).encode() in line.replace(b" ", b""):
                    try:
                        e = json.loads(line)
                    except ValueError:
                        continue
                    if e.get("type") == kind and (subtype is None or e.get("subtype") == subtype):
                        out.append(e)
    except OSError:
        pass
    return out


def week_now(lim):
    """The newest reading of the plan's weekly limit on this machine (usage.py), or None. It never fails a leg."""
    try:
        return usage.read(lim=lim)
    except Exception:                              # noqa: BLE001
        return None


def owes_report(d, leg, child, left_s):
    """The session to give one more turn, or None. A working leg whose session ended cleanly with no RESULT line has
    most often stopped to wait for something (rv-12-hud-f9 and rv-04-flow-goals, "Waiting for the gate"): the turn's
    end is the leg's end, so its work would be lost and its unit recorded as failed. Not at red (it was told to
    close), not without time left, not a leg that only reads (its paper is its result)."""
    if leg.get("mode") != "work" or child.returncode != 0 or left_s < RESUME_MIN_S or (d / legdir.COMPACT).exists():
        return None
    res = (records(d, "result") or [{}])[-1]
    if res.get("subtype") != "success" or res.get("is_error") or said(res.get("result")):
        return None
    if (relay_hook.read_json(d / legdir.METER, {}) or {}).get("level") == "red":
        return None
    return (relay_hook.read_json(d / legdir.SESSION, {}) or {}).get("session_id") or None


def run_leg(d, lim, timeout_s, stop_file=None):
    """stop_file: the owner's stop request; one that says "now" ends the leg at once (state STOPPED)."""
    d = Path(d)
    leg = legdir.read(d)
    os.makedirs(leg["worktree"], exist_ok=True)
    seal = guard_hash(d)
    week = week_now(lim)                           # where the week stood before the leg, if anything read it
    start, state, child, t, again = time.time(), "STOPPED", None, None, None
    try:
        while True:                                # once, and once more for a leg that owes its report
            if again:
                (d / RESUME).write_text(NUDGE, encoding="utf-8", newline="\n")
            with open(d / (RESUME if again else legdir.PROMPT), "rb") as stdin:
                child = subprocess.Popen(argv(d, leg, lim) + (["--resume", again] if again else []),
                                         cwd=leg["worktree"], stdin=stdin, stdout=subprocess.PIPE,
                                         stderr=subprocess.STDOUT, env=leg_env(d, leg),
                                         creationflags=0x00000200 if os.name == "nt" else 0)   # its own process group
            state = "STOPPED"
            if again:
                legdir.update(d, child_pid=child.pid, child_start=proc_start(child.pid), resumed=1)
            else:
                legdir.update(d, state="RUNNING", child_pid=child.pid, child_start=proc_start(child.pid),
                              started_at=now())
            t = threading.Thread(target=_pump, args=(child.stdout, d / legdir.OUT), daemon=True)
            t.start()
            while child.poll() is None:
                if (d / legdir.COMPACT).exists():
                    state = "COMPACT"
                    break
                if time.time() - start > timeout_s:
                    state = "TIMEOUT"
                    break
                if stop_file and (relay_hook.read_json(stop_file) or {}).get("now"):
                    break                                  # state stays STOPPED
                time.sleep(POLL_S)
            else:
                state = "COMPACT" if (d / legdir.COMPACT).exists() else "DONE"
            if state != "DONE" or again:
                break
            t.join(timeout=5)                      # the result record is the last line the session writes
            again = owes_report(d, leg, child, timeout_s - (time.time() - start))
            if not again:
                break
    finally:                                       # also on Ctrl+C or an error in the runner: never leave it running
        rec = {"state": state, "finished_at": now(), "seconds": round(time.time() - start)}
        after = week_now(lim)
        rec.update(week_start=usage.brief(week), week_end=usage.brief(after), week_used=usage.delta(week, after))
        try:                                       # an error in here must not replace the one that brought us here
            if child.poll() is None:
                kill_tree(child.pid)
                try:
                    child.wait(timeout=30)
                except subprocess.TimeoutExpired:
                    pass
            stop_jobs(d)                           # a leg that is over has no job left to wait for
            rec["swept_unity"] = sweep_unity(leg, start)       # nor a Unity of its own on its checkout
            t.join(timeout=5)
            results = records(d, "result")         # one per turn: a resumed session names each turn's own cost
            res = (results or [{}])[-1]
            init = (records(d, "system", "init") or [{}])[0]
            sess = relay_hook.read_json(d / legdir.SESSION, {}) or {}
            meter = relay_hook.read_json(d / legdir.METER, {}) or {}
            final, _, readable = relay_hook.context_tokens(sess.get("transcript_path") or "")
            use = res.get("usage") or {}
            costs = [r["total_cost_usd"] for r in results if isinstance(r.get("total_cost_usd"), (int, float))]
            turns = [r["num_turns"] for r in results if isinstance(r.get("num_turns"), int)]
            rec.update(exit_code=child.returncode, cost_usd=sum(costs) if costs else res.get("total_cost_usd"),
                       turns=sum(turns) if turns else res.get("num_turns"), resumed=1 if again else 0,
                       subtype=res.get("subtype"), is_error=bool(res.get("is_error")), report=res.get("result") or "",
                       said=said(res.get("result")), session_id=sess.get("session_id") or "",
                       has_result=bool(res), denials=len(res.get("permission_denials") or []),
                       final_tokens=final, transcript_read=readable, metered_tokens=meter.get("tokens"),
                       level=meter.get("level", "green"), hooked=bool(sess), tool_uses=tool_uses(d),
                       guard_calls=lines(d / "calls.jsonl"), guard_refusals=lines(d / "denials.jsonl"),
                       unguarded=unguarded(d),
                       ran_model=init.get("model"),
                       tokens_in=use.get("input_tokens"), tokens_out=use.get("output_tokens"),
                       cache_read=use.get("cache_read_input_tokens"),
                       cache_write=use.get("cache_creation_input_tokens"),
                       ran_mode=init.get("permissionMode"))
            rec["guard_intact"] = guard_hash(d) == seal
            leg = legdir.update(d, **rec)
        except Exception as e:                     # noqa: BLE001
            leg = dict(leg, **rec, record_error=repr(e))
    return leg


def ran_clean(leg):
    """Why this leg's outcome cannot be trusted, or None. A leg that did not run under the hooks, in auto mode, to a
    clean result record, proves nothing: the run stops and no verdict is written for its unit."""
    if leg["state"] in BAD_END:
        return "ended %s" % leg["state"]
    if leg.get("record_error"):
        return "its record could not be written (%s)" % leg["record_error"]
    if leg.get("exit_code") != 0 or not leg.get("has_result"):
        return "claude exited %s with %s result record" % (leg.get("exit_code"), "a" if leg.get("has_result") else "no")
    if leg.get("subtype") != "success" or leg.get("is_error"):
        return "its session ended as %s, not success" % leg.get("subtype")
    if leg.get("ran_mode") != "auto":
        return "ran in %s mode, not auto: it cannot work unattended" % leg.get("ran_mode")
    if not leg.get("hooked"):
        return "the hooks did not run (no session record), so nothing guarded it"
    if not leg.get("guard_intact"):
        return "its guard files were changed while it ran"
    if leg.get("unguarded"):                                # by id: a call that ran with no guard on it
        return "the guard did not see %d of its %d tool calls (the first: %s)" % (
            len(leg["unguarded"]), leg.get("tool_uses", 0), leg["unguarded"][0])
    if leg.get("unguarded") is None and leg.get("tool_uses", 0) > leg.get("guard_calls", 0):
        return "the guard saw %d of its %d tool calls" % (leg.get("guard_calls", 0), leg.get("tool_uses", 0))
    if not leg.get("transcript_read"):
        return "its transcript cannot be read, so its context was never measured"
    if leg.get("final_tokens", 0) >= leg["red_tokens"] and leg.get("level") != "red":
        return "it ended at %d tokens but the meter never said red" % leg["final_tokens"]
    return None
