#!/usr/bin/env python3
"""Captures the killed frog (DeathGags.FrogFlop: belly up, legs twitching) and measures the proof number
(contrast_median at death + 1 s) from the pixels CaptureRig actually drew, rather than typing it.

Run from anywhere: `python docs/evidence/frog-death-flop/probe.py`. Needs this checkout's editor open and in Play
in GreyboxCorridor (`Tools/tw up`, `Tools/tw run open_scene -- --path "Assets/_Project/Scenes/GreyboxCorridor.unity"`,
`Tools/tw run editor_play`) — see the pipeline skill's driving-and-evidence.md.

Driven paused and stepped (not wall-clock sleeps): the live battle's own deaths evict a corpse from VATRenderer's
fallen list in well under a second, so a wall-clock wait between separate `tw` calls routinely missed the window.
Pausing first and stepping an exact frame count makes the death, and the capture a second later, land the same way
every run. CaptureRig.Shot's own JSON sidecar already carries `contrast_median` (CaptureRig.cs's figure/ground
reading over the live men in frame — a lone corpse has none to read, so the scene stands a few riflemen beside the
frog), so this script only drives the capture and copies that number out.
"""
import json
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
PROJECT = HERE.parent.parent.parent / "trench-warfare-3d"
FOCUS_X, FOCUS_Z = 46.5, 160.0   # between the frog (45, 160) and the row beside him


def tw(*args, timeout=60):
    r = subprocess.run(["bash", "Tools/tw", *args], cwd=str(PROJECT), capture_output=True, text=True, timeout=timeout)
    return (r.stdout or "") + (r.stderr or "")


def eval_cs(code, timeout=30):
    return tw("eval", code, str(timeout), timeout=timeout + 15)


def main():
    out = tw("build", timeout=180)
    if '"compilationFailed": true' in out:
        print("COMPILE FAILED - not capturing:", out, file=sys.stderr)
        return 1

    tw("pause", "true")
    print(eval_cs("var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); "
                  "if (h==null||h.Local==null) return \"no host\"; "
                  "h.ScriptedPeer=false; h.PeerAttacks=false; "
                  "h.WriteWorlds(m=>{var b=m.World.GetSystem<TW.Sim.Match.AmbientBombardmentSystem>(); if(b!=null) b.ShellsPerMinute=0f;}); "
                  "TW.Editor.DeathLab.Absurd(1f); "
                  f"return TW.Editor.DeathLab.Scene(\"frog\", 45f, {FOCUS_Z}f);"))

    for _ in range(5):
        tw("step", "4")
    death = eval_cs("var h=UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>(); "
                     "return UnityEngine.Time.time + \" \" + h.Local.World.Hp[5];")
    print("after the shell:", death)

    # death + ~1 s of game time, stepped (not slept), with the editor paused the whole way
    for _ in range(8):
        tw("step", "6")

    out_dir = PROJECT / "Captures" / "frog-death-flop"
    out_dir.mkdir(parents=True, exist_ok=True)
    wdir = str(out_dir).replace("\\", "/")
    name = "frog_death"
    print(eval_cs(f'return TW.Editor.CaptureRig.Shot("{wdir}/{name}.png", {FOCUS_X}f, {FOCUS_Z}f, 14.91f, 40f, 20f, 1600, 900, 0.4f);'))
    tw("step", "6")
    for _ in range(30):
        pending = eval_cs("return TW.Editor.CaptureRig.Pending();", timeout=15)
        if '"result": "0"' in pending:
            break
        time.sleep(1)
    else:
        print("capture never drained", file=sys.stderr)
        return 1

    png = out_dir / f"{name}.png"
    sidecar = out_dir / f"{name}.json"
    if not png.exists() or not sidecar.exists():
        print("missing capture:", png, sidecar, file=sys.stderr)
        return 1

    data = json.loads(sidecar.read_text())
    contrast_median = data["contrast_median"]

    evidence_dir = HERE
    for src in (png, sidecar):
        (evidence_dir / src.name).write_bytes(src.read_bytes())

    probe = {"contrast_median": contrast_median, "nominal_seconds": 1.0, "still": png.name}
    (evidence_dir / "probe.json").write_text(json.dumps(probe, indent=2))
    print("probe.json:", probe)
    return 0


if __name__ == "__main__":
    sys.exit(main())
