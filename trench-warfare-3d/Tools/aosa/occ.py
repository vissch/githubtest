"""Offline compile check (occ): build whole TW assemblies from THIS tree's sources with Roslyn, no Unity editor.

Why: the editor is usually held by another session, and a .cs save into Assets/ recompiles under them. This compiles
each named assembly from every .cs file under its asmdef folder (not under a nested asmdef), referencing exactly what
the asmdef declares. A reference to another TW assembly resolves to the one this run just built if it was named on
the command line, otherwise to the built dll in the main clone's Library/ScriptAssemblies. So name every assembly
you changed, in dependency order, and the downstream ones see your new symbols rather than yesterday's dll.

Usage:
  python Tools/aosa/occ.py TW.Presentation.Core TW.Perf TW.Tests.EditMode
  python Tools/aosa/occ.py --changed          # every assembly with a .cs changed vs origin, plus its dependents
Env:
  TW_LIB   folder of built dlls (default: the main clone's trench-warfare-3d/Library/ScriptAssemblies)
  TW_UNITY Unity editor folder (default: C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor)
Exit 0 = every assembly compiled. It cannot check shaders, USS, scenes, or anything Unity does at import.
"""
import json, os, pathlib, subprocess, sys, tempfile, glob

HERE = pathlib.Path(__file__).resolve()
PROJ = HERE.parents[2]                      # trench-warfare-3d/
ASSETS = PROJ / "Assets" / "_Project"
UNITY = pathlib.Path(os.environ.get("TW_UNITY", r"C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor"))
MAIN_LIB = pathlib.Path(os.environ.get(
    "TW_LIB", r"C:/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d/Library/ScriptAssemblies"))
PKG_CACHE = MAIN_LIB.parent / "PackageCache"
OUT = pathlib.Path(tempfile.gettempdir()) / "tw-occ"

DEFINES = ["UNITY_EDITOR", "UNITY_EDITOR_WIN", "UNITY_STANDALONE", "UNITY_STANDALONE_WIN", "ENABLE_PROFILER",
           "UNITY_6000_0", "UNITY_6000_0_OR_NEWER", "UNITY_2023_1_OR_NEWER", "UNITY_2022_3_OR_NEWER",
           "UNITY_2021_3_OR_NEWER", "UNITY_2020_3_OR_NEWER", "UNITY_INCLUDE_TESTS", "UNITY_64", "ENABLE_MONO",
           "ENABLE_BURST_AOT", "ENABLE_INPUT_SYSTEM", "CSHARP_7_3_OR_NEWER"]


def asmdefs():
    out = {}
    for p in ASSETS.rglob("*.asmdef"):
        d = json.loads(p.read_text(encoding="utf-8-sig"))
        out[d["name"]] = (p, d)
    return out


def sources(asm_path, all_dirs):
    root = asm_path.parent
    nested = [d for d in all_dirs if d != root and root in d.parents]
    files = []
    for f in root.rglob("*.cs"):
        if any(n in f.parents for n in nested):
            continue
        files.append(f)
    return sorted(files)


def engine_refs():
    refs = [UNITY / "Data/NetStandard/ref/2.1.0/netstandard.dll"]
    refs += [pathlib.Path(p) for p in glob.glob(str(UNITY / "Data/NetStandard/compat/2.1.0/shims/netstandard/*.dll"))]
    refs += [pathlib.Path(p) for p in glob.glob(str(UNITY / "Data/NetStandard/Extensions/2.0.0/*.dll"))]
    refs += [pathlib.Path(p) for p in glob.glob(str(UNITY / "Data/Managed/UnityEngine/UnityEngine.*.dll"))]
    refs += [pathlib.Path(p) for p in glob.glob(str(UNITY / "Data/Managed/UnityEngine/UnityEditor.*.dll"))]
    return refs


def nunit():
    hits = glob.glob(str(PKG_CACHE / "com.unity.ext.nunit@*/net40/unity-custom/nunit.framework.dll"))
    return [pathlib.Path(hits[0])] if hits else []


def resolve(name, built):
    if name.startswith("GUID:"):
        return None
    if name in built:
        return built[name]
    for cand in (MAIN_LIB / f"{name}.dll", UNITY / f"Data/Managed/UnityEngine/{name}.dll", UNITY / f"Data/Managed/{name}.dll"):
        if cand.exists():
            return cand
    return None


def compile_one(name, table, built, dirs):
    path, d = table[name]
    files = sources(path, dirs)
    if not files:
        print(f"occ: {name}: no sources"); return True
    OUT.mkdir(parents=True, exist_ok=True)
    out = OUT / f"{name}.dll"
    args = ["-nologo", "-target:library", "-nostdlib", "-langversion:9", "-deterministic",
            "-nowarn:CS1701,CS1702,CS0169,CS0414,CS0649,CS8632,CS0618,CS0162,CS0219,CS0067",
            "-define:" + ";".join(DEFINES + [v["define"] for v in d.get("versionDefines", []) if v.get("define")]),
            f'-out:"{out}"']
    if d.get("allowUnsafeCode"):
        args.append("-unsafe")
    refs = engine_refs()
    missing = []
    for r in d.get("references", []):
        p = resolve(r, built)
        if p is None:
            if not r.startswith("GUID:"):
                missing.append(r)
            continue
        refs.append(p)
    # Unity's default package assemblies every asmdef sees when overrideReferences is false
    for extra in ("UnityEngine.UI", "UnityEditor.UI", "Unity.TextMeshPro"):
        p = resolve(extra, built)
        if p: refs.append(p)
    if "Tests" in name or "UNITY_INCLUDE_TESTS" in d.get("defineConstraints", []):
        refs += nunit()
        for r in ("UnityEngine.TestRunner", "UnityEditor.TestRunner"):
            p = resolve(r, built)
            if p: refs.append(p)
        refs += [pathlib.Path(p) for p in glob.glob(str(UNITY / "Data/NetStandard/compat/2.1.0/shims/netfx/*.dll"))]
    for p in d.get("precompiledReferences", []):
        hits = glob.glob(str(PROJ / "**" / p), recursive=True)
        if hits: refs.append(pathlib.Path(hits[0]))
    if missing:
        print(f"occ: {name}: references not found (compiling without them): {', '.join(missing)}")
    seen, rargs = set(), []
    for r in refs:
        k = str(r).lower()
        if k in seen or not pathlib.Path(r).exists():
            continue
        seen.add(k); rargs.append(f'-r:"{r}"')
    rsp = OUT / f"{name}.rsp"
    rsp.write_text("\n".join(args + rargs + [f'"{f}"' for f in files]), encoding="utf-8")
    cmd = [str(UNITY / "Data/NetCoreRuntime/dotnet.exe"), str(UNITY / "Data/DotNetSdkRoslyn/csc.dll"), "-noconfig", f"@{rsp}"]
    r = subprocess.run(cmd, capture_output=True, text=True)
    errs = [l for l in (r.stdout + r.stderr).splitlines() if "error " in l]
    if r.returncode == 0:
        built[name] = out
        print(f"occ OK  {name} ({len(files)} files)")
        return True
    print(f"occ FAIL {name} ({len(errs)} errors)")
    for l in errs[:40]:
        print("   " + l.replace(str(PROJ) + os.sep, ""))
    return False


def order(names, table):
    """Topological order restricted to the named assemblies."""
    done, out = set(), []
    def visit(n):
        if n in done or n not in table: return
        done.add(n)
        for r in table[n][1].get("references", []):
            if r in names: visit(r)
        out.append(n)
    for n in names: visit(n)
    return out


def changed(table):
    root = PROJ.parent
    r = subprocess.run(["git", "-C", str(root), "diff", "--name-only", "origin/claude/trench-warfare-2d-3d-plan-idt7lf"],
                       capture_output=True, text=True)
    u = subprocess.run(["git", "-C", str(root), "ls-files", "--others", "--exclude-standard"], capture_output=True, text=True)
    files = [root / l for l in (r.stdout + u.stdout).splitlines() if l.endswith(".cs")]
    hit = set()
    for n, (p, _) in table.items():
        if any(p.parent in f.parents for f in files):
            hit.add(n)
    # dependents, transitively
    grew = True
    while grew:
        grew = False
        for n, (_, d) in table.items():
            if n not in hit and any(r in hit for r in d.get("references", [])):
                hit.add(n); grew = True
    return sorted(hit)


def main(argv):
    table = asmdefs()
    dirs = [p.parent for p, _ in table.values()]
    names = changed(table) if argv == ["--changed"] else argv
    if not names:
        print(__doc__); return 2
    bad = [n for n in names if n not in table]
    if bad:
        print("occ: unknown assemblies: " + ", ".join(bad)); return 2
    built, ok = {}, True
    for n in order(names, table):
        ok = compile_one(n, table, built, dirs) and ok
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
