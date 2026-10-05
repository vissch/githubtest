#!/usr/bin/env python3
"""What a Blender split was run with, written down by the split itself.

    import splitlog                      at the top of a split script (Tools/ is put on sys.path first)
    splitlog.write(__file__, argv)       as its last line: the record of this run

    python Tools/splitlog.py                          from trench-warfare-3d/: every record against the tree
    python Tools/splitlog.py --adopt F --why "..."    a model edited after its split, on purpose: record the new bytes
    python Tools/splitlog.py --witness split.json DIR a run made elsewhere wrote the very models DIR holds: record it for DIR

WHY. pipelines.md says it of nearly every set: "Parameters the last run used were not recorded ... A fresh run with
defaults will not reproduce what ships." A split takes its numbers from TW_* variables that vanish with the shell,
and the tables were filled in afterwards from memory or not at all. The script now writes them itself: its arguments,
every TW_* variable, the source files by hash, Blender's version, and each file it wrote by hash.

WHERE. A run into the project (an out folder under Assets/) writes Tools/splits/<script>-<folder>.json, outside
Assets so Unity makes no .meta for it and no build carries it. A run anywhere else leaves split.json in its out
folder. Outputs are the files under the folders named in the arguments (and beside them: mechsplit.py writes its
atlas one level up) that the run created or changed.

A WITNESS is how art from before this tool gets its record without being rewritten: run the split into a scratch
folder with the command pipelines.md gives, then --witness that run's split.json against the project folder. It is
taken only when every model the run wrote is, by shape, the one the project holds under the same name.

THE CHECK (Tools/assetgate.py runs it): a model FBX a record lists must still be the model the run wrote (its bytes,
or failing that its shape: Blender stamps the time into every FBX, so a second run of one split differs in bytes
only), or the record no longer says how to make what ships. Edited on purpose (a part re-parented in the file):
--adopt, with why.
Textures and manifests are listed but not held to their hash: grading and atlas steps rewrite them by design.
"""
import argparse
import datetime
import hashlib
import json
import os
import sys
import time
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
START = time.time()                      # a split imports this first thing: what it writes is newer than this
PROJECT = Path(__file__).resolve().parent.parent
RECORDS = 'Tools/splits'
BLENDER = '"C:\\Program Files\\Blender Foundation\\Blender 5.0\\blender.exe" -b --factory-startup -P'


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()[:16]


def _rel(path, project):
    """A path inside the project as project-relative posix, anything else absolute posix."""
    p = Path(path).resolve()
    try:
        return p.relative_to(Path(project).resolve()).as_posix()
    except ValueError:
        return p.as_posix()


def shape(path):
    """Tools/assetgate.py's hash of the model in an FBX (not of its bytes: Blender stamps the time into each file)."""
    import assetgate
    return assetgate.fingerprint(assetgate.load(path))


def _textures(source):
    """One hash over the texture folder Tripo ships beside an FBX (m.fbx + m.fbm/), or None."""
    folder = Path(source).with_suffix('.fbm')
    if not folder.is_dir():
        return None
    h = hashlib.sha256()
    for f in sorted(p for p in folder.rglob('*') if p.is_file()):
        h.update(f.name.encode('utf-8'))
        h.update(f.read_bytes())
    return h.hexdigest()[:16]


def _fresh(path, since):
    return Path(path).stat().st_mtime >= since - 1.0


def _folders(argv, since):
    """The out folders of a run: every folder argument, and the folder of a file argument the run itself wrote
    (frogrig.py names its out FBX, not a folder)."""
    out = []
    for a in argv:
        p = Path(a)
        folder = p if p.is_dir() else p.parent if p.is_file() and _fresh(p, since) else None
        if folder is not None and folder.resolve() not in out:
            out.append(folder.resolve())
    return out


def build(script, argv, project=PROJECT, since=None, env=None):
    """The record of a run, as a dict (write() saves it)."""
    project, since = Path(project), START if since is None else since
    env = os.environ if env is None else env
    folders = _folders(argv, since)
    seen, outputs = set(), []
    for folder in folders:
        beside = [p for p in folder.resolve().parent.iterdir() if p.is_file()]
        for p in sorted(q for q in folder.resolve().rglob('*') if q.is_file()) + sorted(beside):
            if p in seen or not _fresh(p, since) or p.name == 'split.json' or p.suffix == '.meta':
                continue
            seen.add(p)
            line = dict(path=_rel(p, project), sha=sha(p))
            if p.suffix.lower() == '.fbx':
                line['shape'] = shape(p)
            outputs.append(line)
    inputs = []
    for a in argv:
        if Path(a).is_file() and not _fresh(a, since):
            line = dict(path=Path(a).resolve().as_posix(), bytes=Path(a).stat().st_size, sha=sha(a))
            if _textures(a):
                line['textures'] = _textures(a)
            inputs.append(line)
    flags = {k: env[k] for k in sorted(env) if k.startswith('TW_') and k != 'TW_IN_TOOLCHECK'}
    try:
        import bpy
        blender = bpy.app.version_string
    except Exception:
        blender = None
    name = _rel(script, project)
    command = ' '.join([f'{k}={v}' for k, v in flags.items()] + [BLENDER, name, '--']
                       + [_rel(a, project) if Path(a).exists() else a for a in argv])
    return dict(script=name, script_sha=sha(script), blender=blender, date=datetime.date.today().isoformat(),
                args=list(argv), env=flags, command=command, inputs=inputs, outputs=outputs)


def where(script, argv, project=PROJECT, since=None):
    """The file a run's record goes to: Tools/splits/ for a run into the project's Assets, else its out folder."""
    project = Path(project).resolve()
    folders = _folders(argv, START if since is None else since)
    for folder in folders:
        try:
            inside = folder.relative_to(project / 'Assets')
        except ValueError:
            continue
        parts = [p for p in inside.parts if p != '_Project']
        return project / RECORDS / f'{Path(script).stem}-{"_".join(parts)}.json'
    return (folders[0] if folders else Path.cwd()) / 'split.json'


def write(script, argv, project=PROJECT):
    """Call as a split's last line. Never fails the split: a record that cannot be written says so and returns."""
    try:
        record = build(script, argv, project)
        path = where(script, argv, project)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes((json.dumps(record, indent=1) + '\n').encode('utf-8'))
        print(f'SPLITLOG {path}  {len(record["inputs"])} inputs, {len(record["outputs"])} outputs')
        return path
    except Exception as e:
        print(f'SPLITLOG FAILED ({type(e).__name__}: {e}): this run is NOT recorded; write its command into pipelines.md by hand')
        return None


def records(project=PROJECT):
    folder = Path(project) / RECORDS
    return sorted(folder.glob('*.json')) if folder.is_dir() else []


def verify(project=PROJECT):
    """Error lines: a recorded model FBX that is gone, or whose bytes are no longer the run's (or the adopted edit's)."""
    project, errors = Path(project), []
    for path in records(project):
        name = path.relative_to(project).as_posix()
        try:
            record = json.loads(path.read_bytes().decode('utf-8'))
        except Exception as e:
            errors.append(f'{name}: not readable ({type(e).__name__}: {e})')
            continue
        adopted = {e['path']: e for e in record.get('edited', [])}
        for out in record.get('outputs', []):
            rel = out['path']
            if not rel.lower().endswith('.fbx') or Path(rel).is_absolute():
                continue
            if not (project / rel).exists():
                errors.append(f'{name}: lists {rel}, which is gone: delete the record, or run the split again')
            elif (sha(project / rel) not in (out['sha'], adopted.get(rel, {}).get('sha'))
                  and shape(project / rel) not in (out.get('shape'), adopted.get(rel, {}).get('shape'))):
                errors.append(f'{name}: {rel} is no longer what this run wrote: run the split again (the record is rewritten), '
                              f'or after a deliberate edit: python Tools/splitlog.py --adopt {rel} --why "..."')
    return errors


def adopt(rel, why, project=PROJECT):
    project = Path(project)
    rel = _rel(rel if Path(rel).is_absolute() else project / rel, project)
    if not why.strip():
        return ['--adopt needs --why: what was changed in the file, and why not by the split']
    hit = False
    for path in records(project):
        record = json.loads(path.read_bytes().decode('utf-8'))
        if any(o['path'] == rel for o in record.get('outputs', [])):
            edits = [e for e in record.get('edited', []) if e['path'] != rel]
            edits.append(dict(path=rel, sha=sha(project / rel), shape=shape(project / rel), why=why.strip(),
                              date=datetime.date.today().isoformat()))
            record['edited'] = edits
            path.write_bytes((json.dumps(record, indent=1) + '\n').encode('utf-8'))
            hit = True
    return [] if hit else [f'no record in {RECORDS}/ lists {rel}']


def witness(record_path, folder, project=PROJECT):
    """Error lines, or none after writing the record of a run made elsewhere against the project folder it matches."""
    project, folder = Path(project).resolve(), Path(folder).resolve()
    try:
        folder.relative_to(project / 'Assets')
    except ValueError:
        return [f'{folder} is not under this project: its Assets is where a witness goes']
    record = json.loads(Path(record_path).read_bytes().decode('utf-8'))
    models = [o for o in record.get('outputs', []) if o['path'].lower().endswith('.fbx')]
    if not models:
        return [f'{record_path} lists no model']
    errors, outputs = [], []
    for o in models:
        mine = folder / Path(o['path']).name
        if not mine.exists():
            errors.append(f'the run wrote {Path(o["path"]).name}, which {_rel(folder, project)} does not hold')
        elif shape(mine) != o.get('shape'):
            errors.append(f'{_rel(mine, project)} is not the model the run wrote (shape {shape(mine)}, the run {o.get("shape")})')
        else:
            outputs.append(dict(path=_rel(mine, project), sha=sha(mine), shape=o['shape']))
    if errors:
        return errors
    # say the command as it would be run into the project: the run's own folders were scratch
    ran_in = Path(models[0]['path']).parent.as_posix()
    other = sorted({Path(o['path']).parent.as_posix() for o in record['outputs']} - {ran_in}, key=len, reverse=True)

    def placed(text):
        text = text.replace(ran_in, _rel(folder, project))
        for scratch in other:
            if not ran_in.startswith(scratch):
                text = text.replace(scratch, '<renderdir>')
        return text
    record['args'] = [placed(Path(a).resolve().as_posix()) if Path(a).is_absolute() else a for a in record['args']]
    record['command'] = placed(record['command'])
    record['witness'] = dict(date=datetime.date.today().isoformat(),
                             how='run into a scratch folder; every model it wrote is, by shape, the one recorded here')
    record['outputs'] = outputs
    parts = [p for p in folder.relative_to(project / 'Assets').parts if p != '_Project']
    path = project / RECORDS / f'{Path(record["script"]).stem}-{"_".join(parts)}.json'
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes((json.dumps(record, indent=1) + '\n').encode('utf-8'))
    print(f'SPLITLOG {_rel(path, project)}  witnessed: {len(outputs)} models match the run')
    return []


def main(argv=None):
    ap = argparse.ArgumentParser(description='the records of the Blender splits')
    ap.add_argument('--adopt')
    ap.add_argument('--witness', nargs=2, metavar=('SPLIT_JSON', 'DIR'))
    ap.add_argument('--why', default='')
    ap.add_argument('--project', default=str(PROJECT))
    a = ap.parse_args(argv)
    if a.witness:
        errors = witness(a.witness[0], a.witness[1], a.project)
    else:
        errors = adopt(a.adopt, a.why, a.project) if a.adopt else verify(a.project)
    for e in errors:
        print(e)
    if not a.adopt and not a.witness:
        for path in records(a.project):
            record = json.loads(path.read_bytes().decode('utf-8'))
            print(f'{path.name}  {record.get("date")}  {record.get("command", "")[:140]}')
        print('splitlog FAILED' if errors else f'splitlog OK: {len(records(a.project))} records')
    return 1 if errors else 0


if __name__ == '__main__':
    sys.exit(main())
