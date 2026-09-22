"""animforge: read Mixamo binary FBX clips, edit them as per-frame bone curves, write new clips back out as FBX.
Pure Python (no numpy, no FBX SDK). Used by make_missing_clips.py to manufacture the clips the download lacks.

A clip is {bone: {'R': [[x,y,z] per frame], 'T': [[x,y,z]] (Hips only)}} at 30 fps. Operations: reverse, slice,
retime, layer one clip's bones over another, add an offset with an envelope, loop-blend, concatenate, solve an
arm to a target (coordinate-descent IK), forward kinematics, stick-figure PNG contact sheets for checking.
Writing clones a template file (one of the originals) and replaces its curve arrays and stack length, so the
result is byte-for-byte a Mixamo file: same rig, same bone names, same import path.
"""
import struct, zlib, math, os, io

KTIME = 46186158000
FPS = 30

# ------------------------------------------------------------------------------------------------ FBX read / write
class Node:
    __slots__ = ('name', 'props', 'children', 'sentinel')
    def __init__(self, name, props, children, sentinel):
        self.name, self.props, self.children, self.sentinel = name, props, children, sentinel
    def child(self, name):
        for c in self.children:
            if c.name == name: return c
        return None

def read_fbx(path):
    data = open(path, 'rb').read()
    assert data[:20] == b'Kaydara FBX Binary  ', path
    version = struct.unpack_from('<I', data, 23)[0]
    big = version >= 7500
    pos = 27
    def read_node(pos):
        if big: end, nprops, plen = struct.unpack_from('<QQQ', data, pos); pos += 24
        else: end, nprops, plen = struct.unpack_from('<III', data, pos); pos += 12
        nlen = data[pos]; pos += 1
        if end == 0: return None, pos
        name = data[pos:pos + nlen].decode('latin1'); pos += nlen
        props = []; pend = pos + plen
        while pos < pend:
            t = chr(data[pos]); pos += 1
            if t == 'Y': props.append((t, struct.unpack_from('<h', data, pos)[0])); pos += 2
            elif t == 'C': props.append((t, data[pos] != 0)); pos += 1
            elif t == 'I': props.append((t, struct.unpack_from('<i', data, pos)[0])); pos += 4
            elif t == 'F': props.append((t, struct.unpack_from('<f', data, pos)[0])); pos += 4
            elif t == 'D': props.append((t, struct.unpack_from('<d', data, pos)[0])); pos += 8
            elif t == 'L': props.append((t, struct.unpack_from('<q', data, pos)[0])); pos += 8
            elif t in 'fdlib':
                n, enc, clen = struct.unpack_from('<III', data, pos); pos += 12
                raw = data[pos:pos + clen]; pos += clen
                if enc == 1: raw = zlib.decompress(raw)
                fmt = {'f': 'f', 'd': 'd', 'l': 'q', 'i': 'i', 'b': 'b'}[t]
                props.append((t, list(struct.unpack('<%d%s' % (n, fmt), raw))))
            elif t in 'SR':
                n = struct.unpack_from('<I', data, pos)[0]; pos += 4
                props.append((t, data[pos:pos + n])); pos += n
            else: raise ValueError('property type %r' % t)
        children = []; sentinel = False
        while pos < end:
            child, pos = read_node(pos)
            if child is None: sentinel = True; break
            children.append(child)
        return Node(name, props, children, sentinel), end
    roots = []
    while True:
        node, pos = read_node(pos)
        if node is None: break
        roots.append(node)
    footer_id = data[pos:pos + 16]
    magic = data[-16:]
    return version, roots, footer_id, magic

def write_fbx(path, version, roots, footer_id, magic):
    out = bytearray(b'Kaydara FBX Binary  \x00\x1a\x00' + struct.pack('<I', version))
    big = version >= 7500
    def write_node(node):
        start = len(out)
        out.extend(b'\0' * (25 if big else 13))
        body = bytearray()
        for t, v in node.props:
            body.append(ord(t))
            if t == 'Y': body.extend(struct.pack('<h', v))
            elif t == 'C': body.append(1 if v else 0)
            elif t == 'I': body.extend(struct.pack('<i', v))
            elif t == 'F': body.extend(struct.pack('<f', v))
            elif t == 'D': body.extend(struct.pack('<d', v))
            elif t == 'L': body.extend(struct.pack('<q', v))
            elif t in 'fdlib':
                fmt = {'f': 'f', 'd': 'd', 'l': 'q', 'i': 'i', 'b': 'b'}[t]
                raw = struct.pack('<%d%s' % (len(v), fmt), *v); enc = 0
                if len(raw) > 256: raw = zlib.compress(raw, 6); enc = 1
                body.extend(struct.pack('<III', len(v), enc, len(raw))); body.extend(raw)
            elif t in 'SR': body.extend(struct.pack('<I', len(v))); body.extend(v)
        name = node.name.encode('latin1')
        hdr_end = start + (25 if big else 13)
        out[hdr_end - 1] = len(name)
        out.extend(name)
        out.extend(body)
        for c in node.children: write_node(c)
        if node.children or node.sentinel: out.extend(b'\0' * (25 if big else 13))
        end = len(out)
        if big: out[start:start + 24] = struct.pack('<QQQ', end, len(node.props), len(body))
        else: out[start:start + 12] = struct.pack('<III', end, len(node.props), len(body))
    for r in roots: write_node(r)
    out.extend(b'\0' * (25 if big else 13))
    out.extend(footer_id)
    while len(out) % 16: out.append(0)
    out.extend(b'\0' * 4); out.extend(struct.pack('<I', version)); out.extend(b'\0' * 120); out.extend(magic)
    open(path, 'wb').write(out)

def _s(b): return b.decode('latin1').split('\x00')[0]

# ------------------------------------------------------------------------------------------------ rig and clip
class Rig:
    """Bone hierarchy with rest translation and pre-rotation, from any of the clip files (they all share it)."""
    def __init__(self, roots):
        objects = [n for n in roots if n.name == 'Objects'][0]
        conns = [n for n in roots if n.name == 'Connections'][0]
        self.ids = {}; self.names = {}; self.lclT = {}; self.pre = {}; self.lclR = {}
        for n in objects.children:
            if n.name != 'Model': continue
            mid = n.props[0][1]; name = _s(n.props[1][1]).replace('Model::', '').replace('mixamorig:', '')
            self.ids[name] = mid; self.names[mid] = name
            self.lclT[name] = [0.0, 0.0, 0.0]; self.pre[name] = [0.0, 0.0, 0.0]; self.lclR[name] = [0.0, 0.0, 0.0]
            p70 = n.child('Properties70')
            if p70:
                for p in p70.children:
                    key = _s(p.props[0][1])
                    if key == 'Lcl Translation': self.lclT[name] = [p.props[4][1], p.props[5][1], p.props[6][1]]
                    elif key == 'PreRotation': self.pre[name] = [p.props[4][1], p.props[5][1], p.props[6][1]]
                    elif key == 'Lcl Rotation': self.lclR[name] = [p.props[4][1], p.props[5][1], p.props[6][1]]
        self.parent = {}
        for c in conns.children:
            if c.props[0][1] == b'OO' and c.props[1][1] in self.names and c.props[2][1] in self.names:
                self.parent[self.names[c.props[1][1]]] = self.names[c.props[2][1]]
        self.order = []
        def visit(b):
            self.order.append(b)
            for k, v in self.parent.items():
                if v == b: visit(k)
        for b in self.ids:
            if b not in self.parent: visit(b)
    def chain(self, tip):
        out = []
        while tip in self.parent: out.append(tip); tip = self.parent[tip]
        out.append(tip); return out[::-1]
    def descendants(self, root):
        out = [root]
        for k, v in self.parent.items():
            if v == root: out += self.descendants(k)
        return out

def _curves(roots):
    """bone -> {'R': {'X': (times, values), ...}, 'T': {...}} plus the curve node objects for writing."""
    objects = [n for n in roots if n.name == 'Objects'][0]
    conns = [n for n in roots if n.name == 'Connections'][0]
    models = {}; curvenodes = {}; curves = {}
    for n in objects.children:
        if n.name == 'Model': models[n.props[0][1]] = _s(n.props[1][1]).replace('Model::', '').replace('mixamorig:', '')
        elif n.name == 'AnimationCurveNode': curvenodes[n.props[0][1]] = n
        elif n.name == 'AnimationCurve': curves[n.props[0][1]] = n
    node_of = {}; chan_of = {}
    for c in conns.children:
        if c.props[0][1] != b'OP': continue
        src, dst, prop = c.props[1][1], c.props[2][1], _s(c.props[3][1])
        if src in curvenodes and dst in models: node_of[src] = (models[dst], prop)
        elif src in curves and dst in curvenodes: chan_of[src] = (dst, prop)
    table = {}
    for cid, (cn, prop) in chan_of.items():
        if cn not in node_of: continue
        bone, mprop = node_of[cn]
        kind = 'T' if 'Translation' in mprop else 'R' if 'Rotation' in mprop else 'S'
        table.setdefault(bone, {}).setdefault(kind, {})[prop.split('|')[-1]] = curves[cid]
    return table

class Clip:
    def __init__(self, n, data, name='clip'):
        self.n = n; self.data = data; self.name = name; self.template = None
    @staticmethod
    def load(path):
        version, roots, fid, magic = read_fbx(path)
        table = _curves(roots)
        # a constant channel is stored as one key: sample every curve at the stack's frame times
        stop = 0
        for n_ in [n for n in roots if n.name == 'Objects'][0].children:
            if n_.name == 'AnimationStack':
                for p in n_.child('Properties70').children:
                    if _s(p.props[0][1]) == 'LocalStop': stop = p.props[4][1]
        n = int(round(stop * FPS / KTIME)) + 1
        def sample(node):
            times = node.child('KeyTime').props[0][1]; vals = node.child('KeyValueFloat').props[0][1]
            if len(vals) == n: return list(vals)
            out = []; j = 0
            for i in range(n):
                t = i * KTIME / FPS
                while j + 1 < len(times) and times[j + 1] <= t: j += 1
                if j + 1 < len(times) and times[j + 1] > times[j]:
                    w = (t - times[j]) / (times[j + 1] - times[j]); out.append(vals[j] + (vals[j + 1] - vals[j]) * max(0.0, min(1.0, w)))
                else: out.append(vals[j])
            return out
        data = {}
        for bone, kinds in table.items():
            for kind, chans in kinds.items():
                if kind == 'S' or len(chans) < 3: continue
                vals = {ch: sample(node) for ch, node in chans.items()}
                data.setdefault(bone, {})[kind] = [[vals['X'][i], vals['Y'][i], vals['Z'][i]] for i in range(n)]
        c = Clip(n, data, os.path.basename(path)[:-4]); c.template = path; return c
    def copy(self, name=None):
        c = Clip(self.n, {b: {k: [list(f) for f in v] for k, v in d.items()} for b, d in self.data.items()}, name or self.name); c.template = self.template; return c
    def bones(self): return list(self.data)
    def seconds(self): return self.n / FPS
    # ---- editing
    def reverse(self, name=None):
        c = self.copy(name)
        for b in c.data:
            for k in c.data[b]: c.data[b][k].reverse()
        return c
    def slice(self, start, end, name=None):
        """Frames [start, end)."""
        c = self.copy(name); c.n = end - start
        for b in c.data:
            for k in c.data[b]: c.data[b][k] = c.data[b][k][start:end]
        return c
    def resample(self, n, name=None):
        """Stretch or squeeze to n frames (linear between frames; angles by shortest arc)."""
        c = self.copy(name)
        for b in c.data:
            for k in c.data[b]:
                src = c.data[b][k]; out = []
                for i in range(n):
                    t = i * (len(src) - 1) / max(1, n - 1); i0 = int(math.floor(t)); i1 = min(i0 + 1, len(src) - 1); f = t - i0
                    out.append(_mix(src[i0], src[i1], f, k == 'R'))
                c.data[b][k] = out
        c.n = n; return c
    def retime(self, speed, name=None): return self.resample(max(2, int(round(self.n / speed))), name)
    def loopify(self, fade=6, name=None):
        """Blend the last `fade` frames toward the first so the cycle closes."""
        c = self.copy(name)
        for b in c.data:
            for k in c.data[b]:
                v = c.data[b][k]
                for i in range(fade):
                    w = (i + 1) / (fade + 1); j = c.n - fade + i
                    v[j] = _mix(v[j], v[0], w, k == 'R')
        return c
    def layer(self, other, bones, weight=1.0, envelope=None, name=None):
        """Take `bones` from `other` (frame by frame, other resampled to this length), blended by weight x envelope(i)."""
        o = other if other.n == self.n else other.resample(self.n)
        c = self.copy(name)
        for b in bones:
            if b not in o.data or b not in c.data: continue
            for k in c.data[b]:
                if k not in o.data[b]: continue
                for i in range(c.n):
                    w = weight * (envelope(i / max(1, c.n - 1)) if envelope else 1.0)
                    c.data[b][k][i] = _mix(c.data[b][k][i], o.data[b][k][i], w, k == 'R')
        return c
    def offset(self, bone, delta, envelope=None, kind='R', name=None):
        """Add delta (x,y,z) to a bone's channel, scaled by envelope(t 0..1)."""
        c = self.copy(name)
        if bone not in c.data or kind not in c.data[bone]: return c
        for i in range(c.n):
            w = envelope(i / max(1, c.n - 1)) if envelope else 1.0
            for a in range(3): c.data[bone][kind][i][a] += delta[a] * w
        return c
    def scale_motion(self, bone, factor, kind='R', name=None):
        """Exaggerate a bone's motion about its mean (1 = unchanged)."""
        c = self.copy(name)
        if bone not in c.data or kind not in c.data[bone]: return c
        v = c.data[bone][kind]
        mean = [sum(f[a] for f in v) / len(v) for a in range(3)]
        for f in v:
            for a in range(3): f[a] = mean[a] + (f[a] - mean[a]) * factor
        return c
    def set_pose(self, bone, values, kind='R', envelope=None, name=None):
        """Drive a bone toward a fixed pose (blend weight = envelope)."""
        c = self.copy(name)
        if bone not in c.data or kind not in c.data[bone]: return c
        for i in range(c.n):
            w = envelope(i / max(1, c.n - 1)) if envelope else 1.0
            c.data[bone][kind][i] = _mix(c.data[bone][kind][i], list(values), w, kind == 'R')
        return c
    def hold(self, frame, n, name=None):
        c = self.copy(name); c.n = n
        for b in c.data:
            for k in c.data[b]: c.data[b][k] = [list(c.data[b][k][frame]) for _ in range(n)]
        return c
    def pose(self, frame):
        return {b: {k: list(v[frame]) for k, v in d.items()} for b, d in self.data.items()}
    def repeat(self, times, name=None):
        c = self.copy(name); c.n = self.n * times
        for b in c.data:
            for k in c.data[b]: c.data[b][k] = [list(f) for _ in range(times) for f in self.data[b][k]]
        return c
    @staticmethod
    def concat(clips, fade=5, name='concat'):
        """Join clips, cross-fading `fade` frames at each seam (the seam frames overlap)."""
        out = clips[0].copy(name)
        for nxt in clips[1:]:
            a = out; b = nxt if set(nxt.data) >= set(a.data) else nxt
            n = a.n + b.n - fade
            data = {}
            for bone in a.data:
                data[bone] = {}
                for k in a.data[bone]:
                    va = a.data[bone][k]; vb = b.data[bone][k] if bone in b.data and k in b.data[bone] else [va[-1]] * b.n
                    v = [list(f) for f in va[:a.n - fade]]
                    for i in range(fade):
                        w = (i + 1) / (fade + 1)
                        v.append(_mix(va[a.n - fade + i], vb[i], w, k == 'R'))
                    v += [list(f) for f in vb[fade:]]
                    data[bone][k] = v
            out = Clip(n, data, name); out.template = clips[0].template
        return out
    # ---- writing
    def save(self, path, template=None):
        template = template or self.template
        version, roots, fid, magic = read_fbx(template)
        table = _curves(roots)
        n = self.n
        times = [int(round(i * KTIME / FPS)) for i in range(n)]
        for bone, kinds in table.items():
            for kind, chans in kinds.items():
                if kind == 'S': continue
                for ch, node in chans.items():
                    a = 'XYZ'.index(ch)
                    if bone in self.data and kind in self.data[bone]: vals = [float(f[a]) for f in self.data[bone][kind]]
                    else: vals = [node.child('KeyValueFloat').props[0][1][0]] * n
                    node.child('KeyTime').props[0] = ('l', times)
                    node.child('KeyValueFloat').props[0] = ('f', vals)
                    node.child('KeyAttrRefCount').props[0] = ('i', [n])
                    flags = node.child('KeyAttrFlags').props[0][1]; node.child('KeyAttrFlags').props[0] = ('i', flags[:1])
                    dataf = node.child('KeyAttrDataFloat').props[0][1]; node.child('KeyAttrDataFloat').props[0] = ('f', dataf[:4])
        stop = times[-1]
        for root in roots:
            if root.name == 'Objects':
                for o in root.children:
                    if o.name in ('AnimationStack', 'AnimationLayer'):
                        p70 = o.child('Properties70')
                        if p70:
                            for p in p70.children:
                                key = _s(p.props[0][1])
                                if key in ('LocalStop', 'ReferenceStop'): p.props[4] = ('L', stop)
                                if key in ('LocalStart', 'ReferenceStart'): p.props[4] = ('L', 0)
            if root.name == 'Takes':
                for t in root.children:
                    if t.name == 'Take':
                        for c in t.children:
                            if c.name in ('LocalTime', 'ReferenceTime'): c.props = [('L', 0), ('L', stop)]
        write_fbx(path, version, roots, fid, magic)

def _mix(a, b, w, angular):
    out = []
    for i in range(3):
        d = b[i] - a[i]
        if angular:
            while d > 180: d -= 360
            while d < -180: d += 360
        out.append(a[i] + d * w)
    return out

# ------------------------------------------------------------------------------------------------ maths and FK
def _rot(e):
    x, y, z = [math.radians(v) for v in e]
    cx, sx, cy, sy, cz, sz = math.cos(x), math.sin(x), math.cos(y), math.sin(y), math.cos(z), math.sin(z)
    Rx = [[1, 0, 0], [0, cx, -sx], [0, sx, cx]]
    Ry = [[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]]
    Rz = [[cz, -sz, 0], [sz, cz, 0], [0, 0, 1]]
    return _mm(Rz, _mm(Ry, Rx))   # FBX eEulerXYZ: X first, then Y, then Z
def _mm(a, b): return [[sum(a[i][k] * b[k][j] for k in range(3)) for j in range(3)] for i in range(3)]
def _mv(m, v): return [sum(m[i][k] * v[k] for k in range(3)) for i in range(3)]
def _add(a, b): return [a[i] + b[i] for i in range(3)]
def _sub(a, b): return [a[i] - b[i] for i in range(3)]
def _len(a): return math.sqrt(sum(x * x for x in a))

def fk(rig, clip, frame, bones=None):
    """World position and rotation matrix of every bone (or of `bones` and their ancestors) at a frame."""
    want = None
    if bones:
        want = set()
        for b in bones: want.update(rig.chain(b))
    pos = {}; rot = {}
    for b in rig.order:
        if want is not None and b not in want: continue
        d = clip.data.get(b, {})
        t = d['T'][frame] if 'T' in d else rig.lclT[b]
        r = d['R'][frame] if 'R' in d else rig.lclR[b]
        local = _mm(_rot(rig.pre[b]), _rot(r))
        p = rig.parent.get(b)
        if p is None or p not in pos: pos[b] = list(t); rot[b] = local
        else: pos[b] = _add(pos[p], _mv(rot[p], t)); rot[b] = _mm(rot[p], local)
    return pos, rot

def solve_arm(rig, clip, frame, side, target, rounds=4, step=10, span=100):
    """Coordinate-descent IK: find Arm and ForeArm Euler values that put the Hand at target (world, cm).
    Returns (armEuler, foreEuler, error cm). The clip is not changed."""
    arm, fore, hand = side + 'Arm', side + 'ForeArm', side + 'Hand'
    c = clip.copy()
    best = None
    def err():
        p, _ = fk(rig, c, frame, [hand]); return _len(_sub(p[hand], target))
    cur = err()
    for _ in range(rounds):
        for bone in (arm, fore):
            base = list(c.data[bone]['R'][frame])
            for axis in range(3):
                bestv, beste = base[axis], cur
                v = base[axis] - span
                while v <= base[axis] + span:
                    c.data[bone]['R'][frame][axis] = v
                    e = err()
                    if e < beste: beste, bestv = e, v
                    v += step
                c.data[bone]['R'][frame][axis] = bestv; base[axis] = bestv; cur = beste
        step = max(2, step / 2); span = max(20, span / 2)
    return list(c.data[arm]['R'][frame]), list(c.data[fore]['R'][frame]), cur

# ------------------------------------------------------------------------------------------------ preview PNGs
STICK = ['Hips', 'Spine', 'Spine1', 'Spine2', 'Neck', 'Head', 'HeadTop_End', 'LeftShoulder', 'LeftArm', 'LeftForeArm', 'LeftHand',
         'RightShoulder', 'RightArm', 'RightForeArm', 'RightHand', 'LeftUpLeg', 'LeftLeg', 'LeftFoot', 'LeftToeBase',
         'RightUpLeg', 'RightLeg', 'RightFoot', 'RightToeBase']

class Canvas:
    def __init__(self, w, h, bg=(24, 26, 30)):
        self.w, self.h = w, h; self.px = bytearray(bg * (w * h))
    def dot(self, x, y, c):
        if 0 <= x < self.w and 0 <= y < self.h:
            i = (y * self.w + x) * 3; self.px[i:i + 3] = bytes(c)
    def line(self, x0, y0, x1, y1, c):
        x0, y0, x1, y1 = int(round(x0)), int(round(y0)), int(round(x1)), int(round(y1))
        dx, dy = abs(x1 - x0), -abs(y1 - y0); sx = 1 if x0 < x1 else -1; sy = 1 if y0 < y1 else -1; e = dx + dy
        while True:
            self.dot(x0, y0, c); self.dot(x0 + 1, y0, c)
            if x0 == x1 and y0 == y1: break
            e2 = 2 * e
            if e2 >= dy: e += dy; x0 += sx
            if e2 <= dx: e += dx; y0 += sy
    def save(self, path):
        raw = b''.join(b'\0' + bytes(self.px[y * self.w * 3:(y + 1) * self.w * 3]) for y in range(self.h))
        def chunk(t, d): return struct.pack('>I', len(d)) + t + d + struct.pack('>I', zlib.crc32(t + d) & 0xffffffff)
        png = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', self.w, self.h, 8, 2, 0, 0, 0)) + chunk(b'IDAT', zlib.compress(raw, 6)) + chunk(b'IEND', b'')
        open(path, 'wb').write(png)

def contact_sheet(rig, clips, path, frames_per_clip=8, cell=110, scale=0.42):
    """One row per clip: the stick figure at evenly spaced frames, side view (left) and front view (right) in each cell."""
    rows = len(clips); cw = cell * 2; ch = cell
    cv = Canvas(cw * frames_per_clip + 12, ch * rows + 12)
    colours = {'left': (120, 200, 255), 'right': (255, 160, 90), 'mid': (230, 230, 230)}
    for r, clip in enumerate(clips):
        for k in range(frames_per_clip):
            f = int(round(k * (clip.n - 1) / max(1, frames_per_clip - 1)))
            pos, _ = fk(rig, clip, f, STICK)
            ox = 6 + k * cw; oy = 6 + r * ch + ch - 8
            for view in (0, 1):
                vx = ox + view * cell + cell * 0.5
                for b in STICK:
                    p = rig.parent.get(b)
                    if p is None or p not in pos or b not in pos: continue
                    c = colours['left'] if b.startswith('Left') else colours['right'] if b.startswith('Right') else colours['mid']
                    a, q = pos[p], pos[b]
                    hx = (lambda v: v[2]) if view == 0 else (lambda v: v[0])
                    cv.line(vx + hx(a) * scale, oy - a[1] * scale, vx + hx(q) * scale, oy - q[1] * scale, c)
                cv.line(vx - cell * 0.45, oy, vx + cell * 0.45, oy, (70, 70, 80))
    cv.save(path)
