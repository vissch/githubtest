"""Manufacture the clips the Mixamo download lacks (docs/15-character-controller.md section 13) from the ones it has.
Usage: python Tools/make_missing_clips.py "<folder of Mixamo .fbx>" [out folder = <folder>/Made]
Writes 15 FBX files on the Mixamo rig plus made-clips.png (stick-figure contact sheet) and made-clips.csv.
Every clip is either a reversal, a retiming, a layering of one clip's limbs over another, an IK pose held under an
envelope, or a procedural cycle; the recipes are in MAKE below and print what they did.
"""
import sys, os, math, csv
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import animforge as af

src = sys.argv[1]
out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(src, 'Made')
os.makedirs(out, exist_ok=True)
_cache = {}
def clip(name):
    if name not in _cache: _cache[name] = af.Clip.load(os.path.join(src, name + '.fbx'))
    return _cache[name]
_, roots, _, _ = af.read_fbx(os.path.join(src, 'Rifle Idle.fbx'))
rig = af.Rig(roots)
ARMS = ['LeftShoulder', 'LeftArm', 'LeftForeArm', 'LeftHand', 'RightShoulder', 'RightArm', 'RightForeArm', 'RightHand']
LEFT_ARM = ['LeftArm', 'LeftForeArm']; RIGHT_ARM = ['RightArm', 'RightForeArm']
UPPER = ['Spine', 'Spine1', 'Spine2', 'Neck', 'Head'] + ARMS

def smooth(t): t = max(0.0, min(1.0, t)); return t * t * (3 - 2 * t)
def trap(a, b, c, d):
    """0 before a, rises to 1 by b, holds to c, falls to 0 by d."""
    return lambda t: smooth((t - a) / max(1e-6, b - a)) if t < b else 1.0 if t < c else 1.0 - smooth((t - c) / max(1e-6, d - c))
def bump(centre, width): return lambda t: smooth(1.0 - abs(t - centre) / width) if abs(t - centre) < width else 0.0

def ik_hold(c, side, target_of, frame, envelope, name=None):
    """Solve the arm at `frame` so the hand sits at target_of(pos) (world cm), then blend the whole clip toward it."""
    pos, _ = af.fk(rig, c, frame)
    target = target_of(pos)
    arm, fore, err = af.solve_arm(rig, c, frame, side, target)
    print('   %s hand IK error %.1f cm' % (side, err))
    c = c.set_pose(side + 'Arm', arm, envelope=envelope); c = c.set_pose(side + 'ForeArm', fore, envelope=envelope, name=name)
    return c

def wade():
    base = clip('Walk With Rifle (1)').retime(1 / 1.5, 'Wade Forward')          # slower: the water drags
    for leg in ('Left', 'Right'):
        base = base.scale_motion(leg + 'UpLeg', 1.6); base = base.scale_motion(leg + 'Leg', 1.4)   # knees come up out of the water
    base = base.offset('Spine', (8, 0, 0)); base = base.offset('Hips', (0, -6, 0), kind='T')
    base = base.layer(clip('Rifle Crouch Walk').hold(0, base.n), ARMS)          # rifle held high at port arms
    return base.loopify(6)

def ladder():
    n = 36
    c = clip('Rifle Idle').hold(0, n, 'Ladder Climb')
    for i in range(n):
        ph = 2 * math.pi * i / n
        for side, sgn in (('Left', 1), ('Right', -1)):
            lift = 0.5 + 0.5 * math.sin(ph * sgn)                               # 1 = this knee up on the next rung
            c.data[side + 'UpLeg']['R'][i][0] = 20 + 55 * lift
            c.data[side + 'Leg']['R'][i][0] = -15 - 70 * lift
            c.data[side + 'Foot']['R'][i][0] = 10 * lift
        c.data['Spine']['R'][i][0] += 8; c.data['Hips']['R'][i][0] += 4
        c.data['Hips']['T'][i][2] += 10; c.data['Hips']['T'][i][0] += 3 * math.sin(ph)
        c.data['Head']['R'][i][0] -= 18                                          # looks up the ladder
    # hands: each reaches for the rung above as the opposite knee rises, solved every fourth frame and blended
    for side, sgn, x in (('Left', 1, 18), ('Right', -1, -18)):
        keys = {}
        for i in range(0, n, 4):
            ph = 2 * math.pi * i / n; reach = 0.5 + 0.5 * math.sin(ph * sgn + math.pi)
            pos, _ = af.fk(rig, c, i)
            target = [x, pos['Head'][1] + 20 + 30 * reach, pos['Hips'][2] + 32]
            keys[i] = af.solve_arm(rig, c, i, side, target)[:2]
        ks = sorted(keys)
        for i in range(n):
            k0 = max(k for k in ks if k <= i); k1 = min([k for k in ks if k > i] + [ks[0] + n])
            w = (i - k0) / max(1, k1 - k0); a1 = keys[k1 % n]
            c.data[side + 'Arm']['R'][i] = af._mix(keys[k0][0], a1[0], w, True)
            c.data[side + 'ForeArm']['R'][i] = af._mix(keys[k0][1], a1[1], w, True)
    return c.loopify(4)

def crawl(): return clip('Moving Backward In Crawl Position').reverse('Crawl Forward').loopify(4)
def prone_crawl_alt(): return clip('Moving Backward In Prone Position').reverse('Prone Crawl Forward Alt').loopify(4)

def prone_death():
    n = 60
    c = clip('Prone Idle').hold(0, n, 'Prone Death')
    hit = bump(0.08, 0.10); settle = trap(0.10, 0.75, 1.0, 1.0)
    c = c.offset('Spine', (12, 0, 0), hit); c = c.offset('Head', (-14, 0, 0), hit)                  # the round lands
    c = c.offset('Head', (6, 0, 38), settle); c = c.offset('Neck', (10, 0, 6), settle)              # the head goes over
    c = c.offset('Spine1', (-5, 0, 3), settle); c = c.offset('Hips', (0, 0, 7), settle)
    c = c.offset('Hips', (0, -3, 0), settle, kind='T')
    c = c.offset('RightUpLeg', (0, 0, 9), settle); c = c.offset('LeftUpLeg', (0, 0, -6), settle)   # legs splay on the ground
    c = ik_hold(c, 'Left', lambda p: [p['LeftShoulder'][0] + 38, 3, p['LeftShoulder'][2] + 14], n - 1, settle)
    c = ik_hold(c, 'Right', lambda p: [p['RightShoulder'][0] - 30, 3, p['RightShoulder'][2] - 16], n - 1, settle)
    return c

def prone_flinch():
    c = clip('Prone Idle').slice(0, 18, 'Prone Flinch'); e = bump(0.35, 0.4)
    c = c.offset('Neck', (18, 0, 0), e); c = c.offset('Head', (12, 0, 0), e); c = c.offset('Spine1', (5, 0, 0), e)
    c = c.offset('RightForeArm', (0, 0, -15), e); c = c.offset('LeftForeArm', (0, 0, 15), e); c = c.offset('Hips', (0, -2, 0), e, kind='T')
    return c

def kneel_flinch():
    c = clip('Rifle Kneel Idle').slice(0, 18, 'Kneel Flinch'); e = bump(0.35, 0.4)
    c = c.offset('Spine', (14, 0, 0), e); c = c.offset('Spine1', (8, 0, 0), e); c = c.offset('Neck', (12, 0, 0), e); c = c.offset('Head', (10, 0, 0), e)
    c = c.offset('Hips', (0, -6, 0), e, kind='T'); c = c.offset('RightForeArm', (0, 0, -12), e); c = c.offset('LeftForeArm', (0, 0, 12), e); c = c.offset('RightArm', (8, 0, 0), e)
    return c

def get_up():
    return af.Clip.concat([clip('Rifle Prone To Kneel').retime(1.4), clip('Rifle Kneel To Stand').retime(1.3)], fade=4, name='Get Up From Prone')

def stumble():
    c = clip('Rifle Run').repeat(3, 'Stumble Running'); e = bump(0.42, 0.22)
    c = c.offset('Spine', (28, 0, 0), e); c = c.offset('Hips', (12, 0, 0), e); c = c.offset('Neck', (10, 0, 0), e)
    c = c.offset('Hips', (0, -14, 0), e, kind='T')
    c = c.offset('RightArm', (0, 35, 0), e); c = c.offset('LeftArm', (0, -35, 0), e)                # arms fly forward
    c = c.offset('RightForeArm', (0, 0, 30), e); c = c.offset('LeftForeArm', (0, 0, -30), e)
    return c

def mask():
    n = 78
    c = clip('Rifle Idle').slice(0, n, 'Mask Donning'); e = trap(0.0, 0.22, 0.70, 1.0)
    c = ik_hold(c, 'Left', lambda p: [p['Head'][0] + 7, p['Head'][1] - 4, p['Head'][2] + 15], 0, e)
    c = ik_hold(c, 'Right', lambda p: [p['Head'][0] - 7, p['Head'][1] - 4, p['Head'][2] + 15], 0, e)
    c = c.offset('Head', (14, 0, 0), trap(0.05, 0.25, 0.45, 0.6)); c = c.offset('Head', (-10, 0, 0), trap(0.5, 0.62, 0.72, 0.9))   # chin down, then pulls the mask over
    c = c.offset('Spine', (5, 0, 0), e)
    return c

def burning():
    c = clip('Sprint Forward').repeat(2, 'Burning Run')
    n = c.n
    for i in range(n):
        c.data['Head']['R'][i][0] -= 22; c.data['Neck']['R'][i][0] -= 8; c.data['Spine']['R'][i][0] += 6
        c.data['Hips']['R'][i][2] += 6 * math.sin(4 * math.pi * i / n)
    # each arm swings between two solved poses: thrown up over the head, and clawing at the face; out of step with each other
    for side, x, ph in (('Left', 1, 0.0), ('Right', -1, 1.9)):
        pos, _ = af.fk(rig, c, 0)
        up = af.solve_arm(rig, c, 0, side, [pos['Head'][0] + 22 * x, pos['Head'][1] + 38, pos['Head'][2] + 4])[:2]
        face = af.solve_arm(rig, c, 0, side, [pos['Head'][0] + 10 * x, pos['Head'][1] - 2, pos['Head'][2] + 26])[:2]
        for i in range(n):
            w = 0.5 + 0.5 * math.sin(4 * math.pi * i / n + ph)
            c.data[side + 'Arm']['R'][i] = af._mix(up[0], face[0], w, True)
            c.data[side + 'ForeArm']['R'][i] = af._mix(up[1], face[1], w, True)
    return c.loopify(4)

def point():
    c = clip('Idle').resample(66, 'Officer Point')
    c = ik_hold(c, 'Right', lambda p: [p['RightShoulder'][0] - 6, p['RightShoulder'][1] + 10, p['RightShoulder'][2] + 60], 0, trap(0.0, 0.22, 0.72, 1.0))
    c = c.offset('Spine', (0, -8, 0), trap(0.0, 0.25, 0.72, 1.0))   # turns a little into the point
    return c

def whistle():
    c = clip('Idle').resample(48, 'Officer Whistle')
    c = ik_hold(c, 'Right', lambda p: [p['Head'][0] - 2, p['Head'][1] - 7, p['Head'][2] + 12], 0, trap(0.0, 0.28, 0.68, 1.0))
    c = c.offset('Head', (7, 0, 0), trap(0.2, 0.32, 0.62, 0.75))
    return c

def mg_carry():
    c = clip('Rifle Walk').retime(1 / 1.15, 'MG Carry Walk')
    c = ik_hold(c, 'Right', lambda p: [p['Hips'][0] - 20, p['Hips'][1] - 6, p['Hips'][2] + 24], 0, None)
    c = ik_hold(c, 'Left', lambda p: [p['Hips'][0] + 4, p['Hips'][1] + 14, p['Hips'][2] + 44], 0, None)
    c = c.offset('Spine', (4, 0, 6)); c = c.offset('Hips', (0, -2, 0), kind='T')
    return c.loopify(4)

def wire():
    base = clip('Walk Crouching Forward').retime(1 / 1.6, 'Wire Crossing')
    for leg in ('Left', 'Right'):
        base = base.scale_motion(leg + 'UpLeg', 1.7); base = base.scale_motion(leg + 'Leg', 1.5)   # high, careful steps over the strands
    base = base.offset('Spine', (10, 0, 0)); base = base.offset('Neck', (10, 0, 0)); base = base.offset('Hips', (0, -6, 0), kind='T')
    base = base.layer(clip('Rifle Crouch Walk').hold(0, base.n), ARMS)
    return base.loopify(6)

MAKE = [
    ('Wade Forward', wade, 'loop', 'water 0.5 to 1.0 m: Walk With Rifle (1) slowed 1.5x, knees lifted 1.6x, rifle at port arms, leaning in'),
    ('Ladder Climb', ladder, 'loop', 'procedural: alternate knees to the next rung, hands reach the rung above (IK), in place; the sim lifts the man'),
    ('Crawl Forward', crawl, 'loop', 'Moving Backward In Crawl Position reversed'),
    ('Prone Crawl Forward Alt', prone_crawl_alt, 'loop', 'Moving Backward In Prone Position reversed: a second belly crawl'),
    ('Prone Death', prone_death, 'once', 'Prone Idle: a jolt, then the head goes over, the arms slide out (IK to the floor), the legs splay'),
    ('Prone Flinch', prone_flinch, 'once', 'Prone Idle with the head pressed down and the elbows pulled in, 0.6 s'),
    ('Kneel Flinch', kneel_flinch, 'once', 'Rifle Kneel Idle hunching for 0.6 s'),
    ('Get Up From Prone', get_up, 'once', 'Rifle Prone To Kneel at 1.4x into Rifle Kneel To Stand at 1.3x, cross-faded'),
    ('Stumble Running', stumble, 'once', 'three cycles of Rifle Run; in the middle the torso pitches forward, the hips drop and the arms fly out, then it recovers'),
    ('Mask Donning', mask, 'once', 'Rifle Idle with both hands brought to the face (IK) for 1.2 s, chin down then the head pulls back through the mask'),
    ('Burning Run', burning, 'loop', 'Sprint Forward with the arms thrown between over the head and the face (two IK poses each, out of step), head back, torso rocking'),
    ('Officer Point', point, 'once', 'Idle, the right arm raised level and forward (IK), a small turn of the spine, held 1.1 s'),
    ('Officer Whistle', whistle, 'once', 'Idle, the right hand to the mouth (IK), chin down, held 0.6 s'),
    ('MG Carry Walk', mg_carry, 'loop', 'Rifle Walk at 0.87x with the right hand at the hip and the left forward on the barrel (IK), leaning off the weight'),
    ('Wire Crossing', wire, 'loop', 'Walk Crouching Forward at 0.62x, steps 1.7x higher, stooped further, watching the ground, rifle up'),
]

made = []
with open(os.path.join(out, 'made-clips.csv'), 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow(['clip', 'kind', 'seconds', 'frames', 'template', 'recipe'])
    for name, fn, kind, recipe in MAKE:
        print(name)
        c = fn(); c.name = name
        template = getattr(c, 'template', None) or clip('Rifle Idle').template
        c.save(os.path.join(out, name + '.fbx'), template=template)
        made.append(c)
        w.writerow([name, kind, round(c.n / af.FPS, 2), c.n, os.path.basename(template), recipe])
        print('   %d frames, %.2f s -> %s' % (c.n, c.n / af.FPS, name + '.fbx'))
af.contact_sheet(rig, made, os.path.join(out, 'made-clips.png'), frames_per_clip=8, cell=120, scale=0.46)
print('sheet', os.path.join(out, 'made-clips.png'))
