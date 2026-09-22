# Usage: python Tools/fbxscan.py "<folder of Mixamo .fbx>" out.csv   (no Unity, no libraries; behind docs/reference/animation-clips.md)
"""Read Mixamo binary FBX files without any library and report what each clip does:
seconds, frames, whether it loops (first pose ~ last pose), root motion (hips XZ travel), hips height
(stance), yaw turned (hips rotation Y end - start), and which bones move most. Writes clips.csv."""
import os, struct, zlib, sys, math, json, csv

KTIME = 46186158000.0

def read_fbx(path):
    data = open(path, 'rb').read()
    assert data[:20] == b'Kaydara FBX Binary  ', path
    version = struct.unpack_from('<I', data, 23)[0]
    pos = 27
    big = version >= 7500
    def read_node(pos):
        if big:
            end, nprops, plen = struct.unpack_from('<QQQ', data, pos); pos += 24
        else:
            end, nprops, plen = struct.unpack_from('<III', data, pos); pos += 12
        nlen = data[pos]; pos += 1
        if end == 0: return None, pos
        name = data[pos:pos+nlen].decode('latin1'); pos += nlen
        props = []
        pend = pos + plen
        while pos < pend:
            t = chr(data[pos]); pos += 1
            if t == 'Y': props.append(struct.unpack_from('<h', data, pos)[0]); pos += 2
            elif t == 'C': props.append(bool(data[pos])); pos += 1
            elif t == 'I': props.append(struct.unpack_from('<i', data, pos)[0]); pos += 4
            elif t == 'F': props.append(struct.unpack_from('<f', data, pos)[0]); pos += 4
            elif t == 'D': props.append(struct.unpack_from('<d', data, pos)[0]); pos += 8
            elif t == 'L': props.append(struct.unpack_from('<q', data, pos)[0]); pos += 8
            elif t in 'fdlib':
                n, enc, clen = struct.unpack_from('<III', data, pos); pos += 12
                raw = data[pos:pos+clen]; pos += clen
                if enc == 1: raw = zlib.decompress(raw)
                fmt = {'f': 'f', 'd': 'd', 'l': 'q', 'i': 'i', 'b': 'b'}[t]
                props.append(list(struct.unpack('<%d%s' % (n, fmt), raw)))
            elif t in 'SR':
                n = struct.unpack_from('<I', data, pos)[0]; pos += 4
                s = data[pos:pos+n]; pos += n
                props.append(s.decode('latin1', 'replace') if t == 'S' else s)
            else: raise ValueError('prop type %r at %d in %s' % (t, pos, path))
        children = []
        while pos < end:
            child, pos = read_node(pos)
            if child is None: break
            children.append(child)
        return (name, props, children), end
    root = []
    while pos < len(data):
        node, pos = read_node(pos)
        if node is None: break
        root.append(node)
    return root

def find(nodes, name):
    return [n for n in nodes if n[0] == name]

def scan(path):
    root = read_fbx(path)
    objects = find(root, 'Objects')[0][2]
    conns = find(root, 'Connections')[0][2]
    models = {}   # id -> name
    curvenodes = {}  # id -> 'T'/'R'/'S'
    curves = {}   # id -> (times, values)
    stack_seconds = None
    for n in objects:
        if n[0] == 'Model':
            models[n[1][0]] = n[1][1].split('\x00')[0].replace('Model::', '')
        elif n[0] == 'AnimationCurveNode':
            curvenodes[n[1][0]] = n[1][1].split('\x00')[0].replace('AnimCurveNode::', '')
        elif n[0] == 'AnimationCurve':
            t = v = None
            for c in n[2]:
                if c[0] == 'KeyTime': t = c[1][0]
                elif c[0] == 'KeyValueFloat': v = c[1][0]
            curves[n[1][0]] = (t, v)
        elif n[0] == 'AnimationStack':
            for c in n[2]:
                if c[0] == 'Properties70':
                    for p in c[2]:
                        if p[1][0] == 'LocalStop': stack_seconds = p[1][4] / KTIME
    # curvenode -> model, curve -> curvenode channel
    node_of_curvenode = {}; channel_of_curve = {}
    for c in conns:
        p = c[1]
        if p[0] == 'OP':
            src, dst, prop = p[1], p[2], p[3]
            if src in curvenodes and dst in models: node_of_curvenode[src] = (dst, prop)
            elif src in curves and dst in curvenodes: channel_of_curve[src] = (dst, prop)
    # assemble per bone: T xyz and R xyz
    bones = {}
    for cid, (cn, prop) in channel_of_curve.items():
        if cn not in node_of_curvenode: continue
        mid, mprop = node_of_curvenode[cn]
        bone = models[mid].replace('mixamorig:', '')
        kind = 'T' if 'Translation' in mprop else 'R' if 'Rotation' in mprop else 'S'
        ch = prop.split('|')[-1]
        bones.setdefault(bone, {}).setdefault(kind, {})[ch] = curves[cid]
    return stack_seconds, bones

def summarise(path):
    seconds, bones = scan(path)
    hips = bones.get('Hips', {})
    out = {'file': os.path.basename(path)[:-4], 'seconds': round(seconds or 0, 2)}
    frames = 0
    if 'T' in hips and 'Y' in hips['T']:
        t, y = hips['T']['Y']; x = hips['T']['X'][1]; z = hips['T']['Z'][1]
        frames = len(t)
        out['frames'] = frames
        out['hipY_start'] = round(y[0], 1); out['hipY_min'] = round(min(y), 1); out['hipY_max'] = round(max(y), 1); out['hipY_end'] = round(y[-1], 1)
        out['travelXZ'] = round(math.hypot(x[-1] - x[0], z[-1] - z[0]), 1)
        out['wanderXZ'] = round(max(math.hypot(x[i] - x[0], z[i] - z[0]) for i in range(len(x))), 1)
    if 'R' in hips and 'Y' in hips['R']:
        ry = hips['R']['Y'][1]
        d = ry[-1] - ry[0]
        while d > 180: d -= 360
        while d < -180: d += 360
        out['yawTurn'] = round(d, 0)
    # loop: sum of |rot(first) - rot(last)| over all bones, degrees
    diff = 0; moving = []
    for bone, kinds in bones.items():
        if 'R' not in kinds: continue
        amp = 0
        for ch, (t, v) in kinds['R'].items():
            dd = abs(v[-1] - v[0]); dd = min(dd, 360 - dd); diff += dd
            amp += max(v) - min(v)
        moving.append((amp, bone))
    out['loopDiff'] = round(diff, 0)
    moving.sort(reverse=True)
    out['busiest'] = ' '.join(b for a, b in moving[:4])
    legs = sum(a for a, b in moving if b in ('LeftUpLeg', 'RightUpLeg', 'LeftLeg', 'RightLeg'))
    arms = sum(a for a, b in moving if b in ('LeftArm', 'RightArm', 'LeftForeArm', 'RightForeArm'))
    spine = sum(a for a, b in moving if b in ('Spine', 'Spine1', 'Spine2', 'Hips'))
    out['legAmp'] = round(legs); out['armAmp'] = round(arms); out['spineAmp'] = round(spine)
    return out

if __name__ == '__main__':
    folder = sys.argv[1]
    rows = []
    for f in sorted(os.listdir(folder)):
        if not f.lower().endswith('.fbx'): continue
        try: rows.append(summarise(os.path.join(folder, f)))
        except Exception as e: rows.append({'file': f, 'error': str(e)[:80]})
    keys = ['file', 'seconds', 'frames', 'loopDiff', 'travelXZ', 'wanderXZ', 'yawTurn', 'hipY_start', 'hipY_min', 'hipY_max', 'hipY_end', 'legAmp', 'armAmp', 'spineAmp', 'busiest', 'error']
    with open(sys.argv[2], 'w', newline='') as fh:
        w = csv.DictWriter(fh, fieldnames=keys); w.writeheader()
        for r in rows: w.writerow(r)
    print(len(rows), 'clips')
