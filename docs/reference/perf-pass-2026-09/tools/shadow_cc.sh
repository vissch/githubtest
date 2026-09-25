#!/bin/bash
# Offline compile of a SHADOW copy of the project's C# (the live tree + my staged files + my edit scripts applied),
# assembly by assembly in dependency order, each against the dlls just built here before Library/ScriptAssemblies.
# Nothing in the repo is written. Usage: shadow_cc.sh [asm ...]   (default: every assembly the perf pass touches)
set -u
SP="$(cd "$(dirname "$0")" && pwd)"
R="/c/Users/thomas.visscher_magi/Documents/GitHub/githubtest/trench-warfare-3d"
U="/c/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor"
SH="$SP/shadow/trench-warfare-3d"
OUTD="$SP/cc"
rm -rf "$SP/shadow" "$OUTD"; mkdir -p "$SH/Assets" "$OUTD"
# 1. copy the live C# and asmdefs
(cd "$R" && find Assets/_Project \( -name "*.cs" -o -name "*.asmdef" \) -print0 | tar --null -cf - -T -) | (cd "$SH" && tar -xf -)
# 2. staged new files over it
for st in ${STAGES:-stage}; do [ -d "$SP/$st/trench-warfare-3d" ] && (cd "$SP/$st/trench-warfare-3d" && tar -cf - .) | (cd "$SH" && tar -xf -); done
# 3. the edit scripts, against the shadow
for s in ${EDITS-phase0_edits.py convert_tests.py}; do python "$SP/$s" "$SH" >/dev/null || { echo "edit script failed: $s"; exit 3; }; done
[ -n "${ANIM:-}" ] && python "$SP/anim_why.py" "$SH/Assets/_Project/Presentation/Core/AnimationController.cs" >/dev/null
ORDER=${*:-"TW.Sim.Core TW.Presentation.Core TW.Presentation.Units TW.Presentation.Camera TW.Presentation.Terrain TW.UI TW.Perf TW.Editor TW.Tests.EditMode TW.Tests.PlayMode"}
NUNIT="$R/Library/PackageCache/com.unity.ext.nunit@031a54704bff/net40/unity-custom/nunit.framework.dll"
fail=0
for NAME in $ORDER; do
  ASM=$(grep -rl "\"name\": *\"$NAME\"" "$SH/Assets/_Project" --include=*.asmdef | head -1)
  DIR=$(dirname "$ASM")
  # files in DIR not under a nested asmdef
  mapfile -t NESTED < <(find "$DIR" -mindepth 2 -name "*.asmdef" -exec dirname {} \;)
  FILES=()
  while IFS= read -r f; do
    skip=0; for n in "${NESTED[@]}"; do case "$f" in "$n"/*) skip=1;; esac; done
    [ $skip -eq 0 ] && FILES+=("$f")
  done < <(find "$DIR" -name "*.cs")
  ARGS=(-nologo -target:library -nostdlib -noconfig -langversion:9 -nowarn:CS1701,CS1702,CS0169,CS0414,CS0649,CS8632
        -define:UNITY_EDITOR -define:UNITY_6000_0 -define:UNITY_INCLUDE_TESTS -define:ENABLE_PROFILER -define:UNITY_2021_3_OR_NEWER "-out:$OUTD/$NAME.dll")
  grep -q '"allowUnsafeCode": *true' "$ASM" && ARGS+=(-unsafe)
  ARGS+=("-r:$U/Data/NetStandard/ref/2.1.0/netstandard.dll")
  for f in "$U"/Data/NetStandard/compat/2.1.0/shims/netstandard/*.dll "$U"/Data/NetStandard/Extensions/2.0.0/*.dll; do [ -f "$f" ] && ARGS+=("-r:$f"); done
  for f in "$U"/Data/Managed/UnityEngine/UnityEngine.*Module.dll "$U"/Data/Managed/UnityEngine/UnityEditor.CoreModule.dll; do ARGS+=("-r:$f"); done
  case "$NAME" in *Editor*|*Tests*) for f in "$U"/Data/Managed/UnityEngine/UnityEditor.*Module.dll; do case "$f" in *UnityEditor.CoreModule.dll) ;; *) ARGS+=("-r:$f");; esac; done;; esac
  REFS=$(python -c "import json,sys;print(' '.join(json.load(open(sys.argv[1],encoding='utf-8-sig'))['references']))" "$ASM")
  for r in $REFS; do
    if [ -f "$OUTD/$r.dll" ]; then ARGS+=("-r:$OUTD/$r.dll");
    elif [ -f "$R/Library/ScriptAssemblies/$r.dll" ]; then ARGS+=("-r:$R/Library/ScriptAssemblies/$r.dll");
    else echo "  ! reference not found for $NAME: $r"; fi
  done
  # transitive TW deps a dll needs at compile time (types in signatures): add every TW dll already built or present
  for d in "$OUTD"/TW.*.dll; do [ -f "$d" ] && case " ${ARGS[*]} " in *"$d"*) ;; *) ;; esac; done
  case "$NAME" in *Tests*) ARGS+=("-r:$NUNIT"); for f in "$U"/Data/NetStandard/compat/2.1.0/shims/netfx/*.dll; do [ -f "$f" ] && ARGS+=("-r:$f"); done;; esac
  RSP="$OUTD/$NAME.rsp"; : > "$RSP"
  for a in "${ARGS[@]}" "${FILES[@]}"; do
    case "$a" in
      -r:*) printf '"-r:%s"
' "$(cygpath -w "${a#-r:}")" ;;
      -out:*) printf '"-out:%s"
' "$(cygpath -w "${a#-out:}")" ;;
      -*) printf '%s
' "$a" ;;
      *) printf '"%s"
' "$(cygpath -w "$a")" ;;
    esac >> "$RSP"
  done
  out=$("$U/Data/NetCoreRuntime/dotnet.exe" "$U/Data/DotNetSdkRoslyn/csc.dll" "@$(cygpath -w "$RSP")" 2>&1)
  rc=$?
  errs=$(echo "$out" | grep -c "error ")
  if [ $rc -eq 0 ]; then echo "OK   $NAME (${#FILES[@]} files)"; else echo "FAIL $NAME (${#FILES[@]} files, $errs errors)"; echo "$out" | grep -v "warning" | head -15; fail=1; fi
done
exit $fail
