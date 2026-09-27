"""Offline test run (otr): run the EditMode suite outside Unity, on the dlls occ.py built, with a stand-in engine.

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

Usage (after occ.py built every assembly, TW.Tests.EditMode included):
  python Tools/otr.py                        # every EditMode test class
  python Tools/otr.py ScatterRules MineTests # only classes whose name contains one of these
  python Tools/otr.py -v                     # also list PASS and ENGINE lines
Env: TW_OTR_ENGINE=0 (no stand-ins), TW_OTR_LOG=1 (echo logs), TW_OTR_TRACE=1 (jobs), TW_OTR_STACK=1 (full stacks),
TW_OTR_BUILD=<dir> (build the runner elsewhere, for a second run beside one in flight).
Verdicts: PASS, FAIL (an assertion), ERROR (any other exception: look), LOGERR (logged an error it did not expect),
TIMEOUT (300 s), IGNORE, ENGINE (reached a native call with no stand-in), RUNNER (needs NUnit's own runner), CRASH (the
process died in that class; the rest ran in a new one), KNOWN (a FAIL listed in Tools/otr/known.txt with its reason:
read the assertion before adding one), SKIP-UNITYTEST / SKIP-EXPLICIT. Exit 1 on FAIL, ERROR, LOGERR, TIMEOUT or CRASH.
"""
import os, pathlib, subprocess, sys, glob

HERE = pathlib.Path(__file__).resolve()
sys.path.insert(0, str(HERE.parent / "aosa"))   # occ.py lives there (from lane/show/aosa, not tracked on every lane)
try:
    import occ  # noqa: E402  (OUT, UNITY, MAIN_LIB, PKG_CACHE, nunit)
except ImportError:
    print("otr: needs Tools/aosa/occ.py (lane/show/aosa) beside it, and its compiled dlls"); sys.exit(2)

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


def main(argv):
    verbose = "-v" in argv
    filters = [a for a in argv if a != "-v"]
    dll = occ.OUT / "TW.Tests.EditMode.dll"
    if not dll.exists():
        print(f"otr: {dll} not built: run occ.py with TW.Tests.EditMode first"); return 2
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
    runs = filters or [""]
    bad = False
    tally = {}
    for f in runs:
        after = ""
        while True:
            r = subprocess.run([str(MONO), str(exe), str(dll), f, after], cwd=str(occ.PROJ), env=env, capture_output=True,
                               text=True, encoding="utf-8", errors="replace")
            current, done = "", False
            for line in (r.stdout + r.stderr).splitlines():
                if not line.strip() or line.startswith(NOISE):
                    continue
                if line.startswith("#CLASS "):
                    current = line[7:].strip(); continue
                if line.startswith("#DONE"):
                    done = True; continue
                if line.startswith("#CRASH"):
                    print(line); continue
                if line.startswith("---- "):
                    continue   # counted line by line below, so a crashed process loses nothing
                if line.startswith(("Unhandled Exception", "[ERROR] FATAL", "  at ", "   --- End", "System.")):
                    continue
                verdict = line.split("  ", 1)[0]
                if verdict not in ("PASS", "FAIL", "ERROR", "TIMEOUT", "IGNORE", "ENGINE", "RUNNER", "LOGERR", "SKIP-UNITYTEST", "SKIP-EXPLICIT"):
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
            if done or not current:
                break
            # the process died inside a class: record it and carry on after it
            print(f"CRASH  {current}  (the process died in this class; the classes after it run in a new process)")
            tally["CRASH"] = tally.get("CRASH", 0) + 1
            bad = True
            after = current
    print("otr: " + "  ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    if sum(tally.values()) == 0:
        print("otr: no test ran (the filter matched nothing, or the runner died before its first class): not a pass")
        return 1
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
