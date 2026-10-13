"""Offline test run (otr): run the EditMode test modules outside Unity, on the dlls occ.py built, with a stand-in engine.

Why: the gate needs an editor and ~4 GB free; on a loaded machine nothing runs for days. otr loads the test dll into
Unity's own Mono (mono-bdwgc) and registers managed stand-ins for the native engine calls the code uses
(Tools/otr/*.cs), so most of the suite runs as it would in the editor:
  Engine.cs    NativeArray/NativeList memory, safety handles (a use after Dispose still throws), IJob/IJobParallelFor
               run synchronously in schedule order, Burst SharedStatic, Debug.Log (an unexpected error fails the test
               as under Unity's runner), Time, Application (persistentDataPath is a throwaway folder), Mathf.PerlinNoise
               (a reimplementation: compare Perlin-dependent numbers with a tolerance)
  Json.cs      JsonUtility by Unity's field rules
  Objects.cs   Unity object lifetimes and Mesh (channels, sub-meshes, bounds, normals, CombineMeshes, built-in cube/
               quad/plane exact, cylinder/sphere/capsule by size only); Destroy in edit mode logs Unity's error
  MathCalls.cs Quaternion / Matrix4x4 / Vector3 native maths
  Render.cs    Resources.Load: TextAssets, ScriptableObject .assets (Yaml.cs), FBX meshes (Fbx.cs: Blender's axes and
               bakeAxisConversion 1 only, anything else is ENGINE; the axis map is proven by HouseKitTests putting the
               sliced kit props back together in Unity and here alike), images as placeholder textures; Shader.Find;
               materials and textures created, every other rendering call an inert stub
What still reports ENGINE: UI Toolkit layout, GameObjects/components, physics, audio, prefabs, UXML, profiler
recorders. What otr cannot see: Burst codegen, the job system's threads and race detection, GPU work, pixels. A
green otr is a strong first filter; the gate stays the verdict.

Usage (after occ.py built every assembly, the test modules included):
  python Tools/otr.py                        # every test class of every module under Assets/_Project/Tests
  python Tools/otr.py ScatterRules MineTests # only classes whose name contains one of these
  python Tools/otr.py --module Sim,Match     # only these modules (the names Tools/gate_scope.py lists)
  python Tools/otr.py -v                     # also list PASS and ENGINE lines
Env: TW_OTR_ENGINE=0 (no stand-ins), TW_OTR_LOG=1 (echo logs), TW_OTR_TRACE=1 (jobs), TW_OTR_STACK=1 (full stacks),
TW_OTR_BUILD=<dir> (build the runner elsewhere, for a second run beside one in flight),
TW_OTR_PROC_TIMEOUT=<s> (how long one runner process may take, 1800 by default; over it the class is CRASH).
Verdicts: PASS, FAIL (an assertion), ERROR (any other exception: look), LOGERR (logged an error it did not expect),
TIMEOUT (300 s), IGNORE, ENGINE (reached a native call with no stand-in), RUNNER (needs NUnit's own runner), CRASH (the
process died in that class; the rest ran in a new one), KNOWN (a FAIL listed in Tools/otr/known.txt with its reason:
read the assertion before adding one), SKIP-UNITYTEST / SKIP-EXPLICIT. Exit 1 on FAIL, ERROR, LOGERR, TIMEOUT or CRASH.
"""
import os, pathlib, shutil, subprocess, sys, glob, tempfile, time

HERE = pathlib.Path(__file__).resolve()
# the checkout under test is the one otr is run in (Unity's cwd rule: the project folder), not the one otr.py lives in:
# occ.OUT is keyed by occ.py's own folder, so importing this file's neighbour tested the SHOW tree's dlls when otr was
# run from the sim checkout (2026-09-27). Use the working project's occ.py when it has one.
_here_proj = HERE.parents[1]
_cwd_occ = pathlib.Path.cwd() / "Tools" / "aosa"
sys.path.insert(0, str(_cwd_occ if (_cwd_occ / "occ.py").exists() else HERE.parent / "aosa"))   # occ.py (from lane/show/aosa, not tracked on every lane)
try:
    import occ  # noqa: E402  (OUT, UNITY, MAIN_LIB, PKG_CACHE, nunit)
except ImportError:
    print("otr: needs Tools/aosa/occ.py (lane/show/aosa) beside it, and its compiled dlls"); sys.exit(2)
sys.path.insert(0, str(HERE.parent))
import gate_scope  # noqa: E402  (the test modules: one definition, shared with the gate)

RUNNER_SRC = HERE.parent / "otr" / "Runner.cs"
ENGINE_SRC = HERE.parent / "otr" / "Engine.cs"
EXTRA_SRC = [HERE.parent / "otr" / "Json.cs", HERE.parent / "otr" / "Objects.cs", HERE.parent / "otr" / "MathCalls.cs", HERE.parent / "otr" / "Render.cs", HERE.parent / "otr" / "Yaml.cs", HERE.parent / "otr" / "Fbx.cs"]
MONO = occ.UNITY / "Data/MonoBleedingEdge/bin/mono-bdwgc.exe"   # the Boehm runtime Unity uses: Engine.cs registers its calls in mono-2.0-bdwgc.dll
API = occ.UNITY / "Data/MonoBleedingEdge/lib/mono/4.7.1-api"
NOISE = ("cant resolve internal call", "Your mono runtime and class libraries", "The out of sync library",
         "When you update one from git", "the other too.", "Do not report this as a bug", "you probably have a broken",
         "If you see other errors", "and you need to fix your mono")


def build_runner():
    exe = pathlib.Path(os.environ.get("TW_OTR_BUILD", str(occ.OUT / "otr"))) / "Runner.exe"   # TW_OTR_BUILD: a second run beside one in flight
    if exe.exists() and exe.stat().st_mtime >= max(s.stat().st_mtime for s in [RUNNER_SRC, ENGINE_SRC, *EXTRA_SRC]):
        return exe
    exe.parent.mkdir(parents=True, exist_ok=True)
    cmd = [str(occ.UNITY / "Data/NetCoreRuntime/dotnet.exe"), str(occ.UNITY / "Data/DotNetSdkRoslyn/csc.dll"),
           "-noconfig", "-nostdlib", "-nologo", "-unsafe", "-langversion:9", f"-out:{exe}",
           f"-r:{API / 'mscorlib.dll'}", f"-r:{API / 'System.dll'}", f"-r:{API / 'System.Core.dll'}", str(RUNNER_SRC), str(ENGINE_SRC), *map(str, EXTRA_SRC)]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        print(r.stdout + r.stderr); sys.exit(2)
    return exe


VERDICTS = ("PASS", "FAIL", "ERROR", "TIMEOUT", "IGNORE", "ENGINE", "RUNNER", "LOGERR", "SKIP-UNITYTEST", "SKIP-EXPLICIT")


def scan_run(text, returncode, known=None, verbose=False):
    """Judge one runner process on its own output: (tally, bad, current, done).

    current is the last class the runner announced (""  when it died before its first), done whether it printed
    #DONE. bad is True for FAIL/ERROR/TIMEOUT/LOGERR, for a #CRASH line, for a LOADER / ENGINE-INSTALL-FAILED line
    (Runner.cs writes those: the module's types or the stand-in engine never loaded), and for a non-zero exit code
    after #DONE. A run that printed neither #DONE nor a class is the caller's to record.
    """
    known = known or {}
    tally, bad, current, done = {}, False, "", False
    for line in text.splitlines():
        if not line.strip() or line.startswith(NOISE):
            continue
        if line.startswith("#CLASS "):
            current = line[7:].strip(); continue
        if line.startswith("#DONE"):
            done = True; continue
        if line.startswith("#CRASH"):
            print(line); bad = True; continue
        if line.startswith("---- "):
            continue   # counted line by line below, so a crashed process loses nothing
        if line.startswith(("Unhandled Exception", "[ERROR] FATAL", "  at ", "   --- End", "System.")):
            continue
        verdict = line.split("  ", 1)[0]
        if verdict not in VERDICTS:
            if line.startswith(("LOADER ", "ENGINE-INSTALL-FAILED ")):
                print(line); bad = True; continue
            if verbose: print(line)
            continue
        name = line.split("  ")[1] if "  " in line else ""
        if verdict == "FAIL" and name in known:
            verdict = "KNOWN"; line = f"KNOWN  {name}  ({known[name]})"
        tally[verdict] = tally.get(verdict, 0) + 1
        if verdict in ("FAIL", "ERROR", "TIMEOUT", "LOGERR"):
            bad = True
        if verbose or verdict not in ("PASS", "ENGINE"):
            print(line)
    if done and returncode != 0:
        print(f"otr: the runner exited {returncode} after #DONE")
        bad = True
    return tally, bad, current, done


def rsp_sources(rsp):
    """The .cs files occ.py wrote into OUT/<name>.rsp (quoted, one per line): resolved lowercase key -> the path."""
    out = {}
    for line in rsp.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if line.startswith('"') and line.endswith('.cs"'):
            f = pathlib.Path(line[1:-1])
            out[str(f.resolve()).lower()] = f
    return out


def stale(table, dirs, out, tested):
    """Why no dll of `table` may be trusted, or None when every one is in step with this tree.

    Each TW dll is checked against its OWN sources (a newest-source-vs-newest-dll check over the whole tree passed a
    fresh test dll over stale Sim dlls when occ.py was run with only some assemblies: critic r6, 2026-09-27), against
    the dlls it references (C# copies consts and enum values into the caller, so a dll built before its dependency
    changed carries the old values: critic r7), against the file list occ.py compiled it from (OUT/<name>.rsp: a
    deleted .cs is gone from disk but still in the dll, and a mtime check can never see it), and against its own
    asmdef (references and defines change the dll without touching a .cs).
    """
    for name, (path, d) in table.items():
        built = out / (name + ".dll")
        if not name.startswith("TW.") or not built.exists():
            continue
        srcs = occ.sources(path, dirs)
        if not srcs:
            continue
        newest_src = max(srcs, key=lambda p: p.stat().st_mtime)
        if newest_src.stat().st_mtime > built.stat().st_mtime + 1:
            return f"otr: {name}.dll is older than {rel(newest_src, tested)}: run occ.py with every assembly and fix its errors first; not a verdict"
        if path.stat().st_mtime > built.stat().st_mtime + 1:
            return f"otr: {name}.dll is older than {rel(path, tested)}: its references or defines may have changed; run occ.py with every assembly; not a verdict"
        rsp = out / (name + ".rsp")
        if not rsp.exists():
            return f"otr: cannot tell what {name}.dll was built from (no {rsp.name} beside it): run occ.py with every assembly; not a verdict"
        was = rsp_sources(rsp)
        now = {str(s.resolve()).lower(): s for s in srcs}
        gone = sorted(set(was) - set(now))
        if gone:
            return f"otr: {name}.dll was built with {rel(was[gone[0]], tested)}, which is gone: run occ.py with every assembly; not a verdict"
        added = sorted(set(now) - set(was))
        if added:
            return f"otr: {rel(now[added[0]], tested)} is not in what {name}.dll was built from: run occ.py with every assembly; not a verdict"
        for ref in d.get("references", []):
            dep = out / (ref + ".dll")
            if ref.startswith("TW.") and dep.exists() and dep.stat().st_mtime > built.stat().st_mtime + 1:
                return f"otr: {name}.dll was built before {ref}.dll: run occ.py with every assembly; not a verdict"
    return None


def rel(path, root):
    try:
        return str(path.relative_to(root))
    except ValueError:
        return str(path)


PROC_TIMEOUT_S = int(os.environ.get("TW_OTR_PROC_TIMEOUT", "1800"))   # a wedged runner process: no run may hang forever


def sweep_temp(tmp_root, max_age_s=86400):
    """Delete the tw-otr-* scratch folders older than a day under `tmp_root`; return how many went.

    The runner deletes its own folder when the process ends (Engine.cs), but a crashed or killed runner never gets
    there, so otr used to leave one tw-otr-<pid> tree per crashed class behind for good.
    """
    gone = 0
    now = time.time()
    for d in sorted(pathlib.Path(tmp_root).glob("tw-otr-*")):
        try:
            if not d.is_dir() or now - d.stat().st_mtime <= max_age_s:
                continue
            shutil.rmtree(d, ignore_errors=True)
            gone += 0 if d.exists() else 1
        except OSError:
            continue
    return gone


def run_module(exe, dll, f, env, known, verbose, runner=subprocess.run):
    """Run one test dll (restarting after a class that took the process down): (tally, bad).

    A runner process that hangs (a native call waiting forever, a thread Abort that did not take) is given
    PROC_TIMEOUT_S; its partial output is still judged, the class it was in is CRASH, and the run carries on after
    that class instead of hanging the whole of otr.
    """
    tally, bad, after = {}, False, ""
    while True:
        timed_out = False
        try:
            r = runner([str(MONO), str(exe), str(dll), f, after], cwd=str(occ.PROJ), env=env, capture_output=True,
                       text=True, encoding="utf-8", errors="replace", timeout=PROC_TIMEOUT_S)
            text, code = (r.stdout or "") + (r.stderr or ""), r.returncode
        except subprocess.TimeoutExpired as e:
            timed_out = True
            def _text(x):
                return x if isinstance(x, str) else (x or b"").decode("utf-8", "replace")
            text, code = _text(e.stdout) + _text(e.stderr), 0
        run_tally, run_bad, current, done = scan_run(text, code, known, verbose)
        for k, v in run_tally.items():
            tally[k] = tally.get(k, 0) + v
        bad |= run_bad
        if done and not timed_out:
            break
        why = f"the runner process hung (over {PROC_TIMEOUT_S} s)" if timed_out else "the process died in this class"
        if not current:
            # nothing of this module ran, and nobody else records it
            print(f"CRASH  {dll.stem} {f}  ({why}: nothing from this module ran)")
            tally["CRASH"] = tally.get("CRASH", 0) + 1
            return tally, True
        print(f"CRASH  {current}  ({why}; the classes after it run in a new process)")
        tally["CRASH"] = tally.get("CRASH", 0) + 1
        bad = True
        after = current
    return tally, bad


def main(argv):
    verbose = "-v" in argv
    sweep_temp(pathlib.Path(tempfile.gettempdir()))
    argv = [a for a in argv if a != "-v"]
    wanted = None
    if "--module" in argv:
        i = argv.index("--module")
        if i + 1 >= len(argv):
            print("otr: --module needs a name (or several, comma-separated)"); return 2
        wanted = [m.strip().lower() for m in argv[i + 1].split(",") if m.strip()]
        argv = argv[:i] + argv[i + 2:]
    filters = argv
    tested = pathlib.Path(occ.PROJ).resolve()
    # the modules the gate's EditMode pass runs from Assets/_Project/Tests (Stills is [Explicit]; the playground's
    # tests are all asset imports, which have no stand-in)
    mods = {m: asm for m, (asm, folder) in gate_scope.modules(tested).items()
            if folder.startswith("trench-warfare-3d/Assets/_Project/Tests/") and m not in gate_scope.EXPLICIT_ONLY}
    if wanted is not None:
        bad = [w for w in wanted if w not in {m.lower() for m in mods}]
        if bad:
            print(f"otr: no test module named {', '.join(bad)}. The modules: {', '.join(sorted(mods))}"); return 2
        mods = {m: asm for m, asm in mods.items() if m.lower() in wanted}
    dlls = [occ.OUT / (asm + ".dll") for asm in mods.values()]
    print(f"otr: testing {tested} (dlls in {occ.OUT})")
    if tested != pathlib.Path.cwd().resolve():
        print(f"otr: WARNING: run from {pathlib.Path.cwd()}, which is not the checkout under test ({tested}); cd into it")
    table = occ.asmdefs()
    dirs = [path.parent for path, _ in table.values()]
    why = stale(table, dirs, occ.OUT, tested)
    if why:
        print(why)
        return 2
    for dll in dlls:
        if not dll.exists():
            print(f"otr: {dll} not built: run occ.py with {dll.stem} first"); return 2
    exe = build_runner()
    nunit_dirs = [str(pathlib.Path(p).parent) for p in occ.nunit()]
    # precompiled package plugins (Unity.Burst.Unsafe, Collections' ILSupport, ...): what Unity loads beside ScriptAssemblies
    nunit_dirs += [str(pathlib.Path(p)) for p in glob.glob(str(occ.PKG_CACHE / "com.unity.burst@*"))]
    nunit_dirs += [str(pathlib.Path(p).parent) for p in glob.glob(str(occ.PKG_CACHE / "*/**/*.dll"), recursive=True)
                   if "Editor" not in p and "CodeGen" not in p and "Tests" not in p]
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
    runs = [(dll, f) for dll in dlls for f in (filters or [""])]
    bad = False
    tally = {}
    for dll, f in runs:
        run_tally, run_bad = run_module(exe, dll, f, env, known, verbose)
        for k, v in run_tally.items():
            tally[k] = tally.get(k, 0) + v
        bad |= run_bad
    print("otr: " + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if sum(tally.values()) == 0:
        print("otr: no test ran (the filter matched nothing, or the runner died before its first class): not a pass")
        return 1
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
