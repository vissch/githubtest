"""Offline test run (otr): run the EditMode tests that need no engine, outside Unity, on the dlls occ.py built.

Why: the gate needs an editor and ~4 GB free; on a loaded machine nothing runs for days. Most presentation logic
(scatter fields, the campaign graph, HUD arithmetic, aim shapes, meshes built in plain C#) is managed code: this
loads the test dll occ.py compiled into Unity's own Mono and runs every [Test] / [TestCase] / [Values] /
[ValueSource] with its [OneTimeSetUp] / [SetUp] / [TearDown]. A test that reaches native engine code (NativeArray,
jobs, JsonUtility, Resources, Mesh, Debug.Log...) cannot run here and is reported ENGINE, not failed: every sim test
is ENGINE. So otr is a first filter, never a gate verdict: green here says nothing about ENGINE tests.

Usage (after occ.py built the assemblies you changed and TW.Tests.EditMode):
  python Tools/otr.py                     # every EditMode test class
  python Tools/otr.py ScatterRules Campaign   # only classes whose name contains one of these
  python Tools/otr.py -v                  # also list PASS and ENGINE lines
Verdicts: PASS, FAIL (an assertion), ERROR (any other exception from managed code: look), TIMEOUT (60 s), IGNORE,
ENGINE (native code reached), RUNNER (needs NUnit's own runner: TestContext), SKIP-UNITYTEST / SKIP-EXPLICIT, and
KNOWN: a FAIL listed in Tools/otr/known.txt with its reason (code that CATCHES a native failure, e.g. a store falling back to
defaults when JsonUtility is missing, fails here for the engine's absence; Unity's Mono raises no first-chance event
to tell). Add a line there only after reading the assertion. Exit 1 when anything else FAILs, ERRORs or times out.
"""
import os, pathlib, subprocess, sys, glob

HERE = pathlib.Path(__file__).resolve()
sys.path.insert(0, str(HERE.parent / "aosa"))   # occ.py lives there (from lane/show/aosa, not tracked on every lane)
try:
    import occ  # noqa: E402  (OUT, UNITY, MAIN_LIB, PKG_CACHE, nunit)
except ImportError:
    print("otr: needs Tools/aosa/occ.py (lane/show/aosa) beside it, and its compiled dlls"); sys.exit(2)

RUNNER_SRC = HERE.parent / "otr" / "Runner.cs"
MONO = occ.UNITY / "Data/MonoBleedingEdge/bin/mono.exe"
API = occ.UNITY / "Data/MonoBleedingEdge/lib/mono/4.7.1-api"
NOISE = ("cant resolve internal call", "Your mono runtime and class libraries", "The out of sync library",
         "When you update one from git", "the other too.", "Do not report this as a bug", "you probably have a broken",
         "If you see other errors", "and you need to fix your mono")


def build_runner():
    exe = occ.OUT / "otr" / "Runner.exe"
    if exe.exists() and exe.stat().st_mtime >= RUNNER_SRC.stat().st_mtime:
        return exe
    exe.parent.mkdir(parents=True, exist_ok=True)
    cmd = [str(occ.UNITY / "Data/NetCoreRuntime/dotnet.exe"), str(occ.UNITY / "Data/DotNetSdkRoslyn/csc.dll"),
           "-noconfig", "-nostdlib", "-nologo", "-langversion:9", f"-out:{exe}",
           f"-r:{API / 'mscorlib.dll'}", f"-r:{API / 'System.dll'}", f"-r:{API / 'System.Core.dll'}", str(RUNNER_SRC)]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        print(r.stdout + r.stderr); sys.exit(2)
    return exe


def main(argv):
    verbose = "-v" in argv
    filters = [a for a in argv if a != "-v"]
    dll = occ.OUT / "TW.Tests.EditMode.dll"
    if not dll.exists():
        print(f"otr: {dll} not built: run occ.py with TW.Tests.EditMode first"); return 2
    exe = build_runner()
    nunit_dirs = [str(pathlib.Path(p).parent) for p in occ.nunit()]
    env = dict(os.environ)
    env["TW_RUN_DIRS"] = ";".join([str(occ.OUT), str(occ.MAIN_LIB), str(occ.UNITY / "Data/Managed/UnityEngine"),
                                   str(occ.UNITY / "Data/Managed"), *nunit_dirs,
                                   str(occ.UNITY / "Data/MonoBleedingEdge/lib/mono/4.5/Facades"),
                                   str(occ.UNITY / "Data/NetStandard/compat/2.1.0/shims/netfx")])
    known = {}
    kf = HERE.parent / "otr" / "known.txt"
    if kf.exists():
        for line in kf.read_text(encoding="utf-8").splitlines():
            if line.strip() and not line.startswith("#"):
                name, _, why = line.partition(" ")
                known[name] = why.strip()
    runs = filters or [""]
    bad = False
    tally = {}
    for f in runs:
        r = subprocess.run([str(MONO), str(exe), str(dll), f], cwd=str(occ.PROJ), env=env, capture_output=True, text=True,
                           encoding="utf-8", errors="replace")
        for line in (r.stdout + r.stderr).splitlines():
            if not line.strip() or line.startswith(NOISE):
                continue
            if line.startswith("---- "):
                for kv in line[5:].split():
                    k, v = kv.split("="); tally[k] = tally.get(k, 0) + int(v)
                continue
            verdict = line.split("  ", 1)[0]
            name = line.split("  ")[1] if "  " in line else ""
            if verdict == "FAIL" and name in known:
                verdict = "KNOWN"; line = f"KNOWN  {name}  ({known[name]})"
                tally["KNOWN"] = tally.get("KNOWN", 0) + 1
            if verdict in ("FAIL", "ERROR", "TIMEOUT"):
                bad = True
            if verbose or verdict not in ("PASS", "ENGINE"):
                print(line)
    if tally.get("KNOWN"):
        tally["FAIL"] = tally.get("FAIL", 0) - tally["KNOWN"]
        if tally["FAIL"] <= 0: tally.pop("FAIL")
    print("otr: " + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
