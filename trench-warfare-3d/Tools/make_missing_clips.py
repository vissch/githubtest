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
def react(at, hold=0.1, out=0.45, rise=0.05):
    """A reaction: in over `rise` of the clip, held, then out slowly (a body flinches in 2-4 frames and recovers in 15)."""
    return trap(at, at + rise, at + rise + hold, at + rise + hold + out)

def ik_hold(c, side, target_of, frame, envelope, name=None):
    """Solve the arm at `frame` so the hand sits at target_of(pos) (world cm), then blend the whole clip toward it."""
    pos, _ = af.fk(rig, c, frame)
    target = target_of(pos)
    arm, fore, err = af.solve_arm(rig, c, frame, side, target)
    print('   %s hand IK error %.1f cm' % (side, err))
    c = c.set_pose(side + 'Arm', arm, envelope=envelope); c = c.set_pose(side + 'ForeArm', fore, envelope=envelope, name=name)
    return c

def drop_hips(c, cm, envelope=None):
    """Lower the hips by cm, bending both knees to take it up so the feet stay on the floor (about 2 degrees a cm)."""
    c = c.offset('Hips', (0, -cm, 0), envelope, kind='T')
    for leg in ('Left', 'Right'):
        c = c.offset(leg + 'Leg', (-2.0 * cm, 0, 0), envelope); c = c.offset(leg + 'UpLeg', (1.0 * cm, 0, 0), envelope)
    return c

def high_step(base, lift, fwd, back, knee):
    """A careful high step: the thigh lifted by `lift` degrees, its forward swing scaled `fwd` and its trailing swing
    `back` (so the rear leg is not thrown out flat), the knee's bend scaled `knee`."""
    for leg in ('Left', 'Right'):
        base = base.offset(leg + 'UpLeg', (lift, 0, 0))
        base = base.scale_motion(leg + 'UpLeg', fwd, back=back, axis=0)
        base = base.scale_motion(leg + 'Leg', 1.0, back=knee, axis=0)   # the bend is the negative side
    return base

def wade():
    base = clip('Walk With Rifle (1)').retime(1 / 1.7, 'Wade Forward')          # slower: the water drags
    base = high_step(base, 14, 1.5, 0.6, 1.4)
    base = base.offset('Spine', (8, 0, 0)); base = drop_hips(base, 5)
    base = base.wave('Hips', 1, 3, 2, kind='T')                                  # the body bobs with each step
    base = base.layer(clip('Rifle Crouch Walk'), ARMS)                          # rifle high at port arms, arms alive
    return base.loopify(6)

def ladder():
    n = 36
    c = clip('Rifle Idle').hold(0, n, 'Ladder Climb')
    for i in range(n):
        ph = 2 * math.pi * i / n
        for side, sgn in (('Left', 1), ('Right', -1)):
            lift = smooth(max(0.0, min(1.0, 1.5 * math.sin(ph * sgn) + 0.5)))     # 1 = this knee on the next rung; dwells at both ends
            up = 20 + 55 * lift; leg = -15 - 70 * lift
            c.data[side + 'UpLeg']['R'][i][0] = up
            c.data[side + 'Leg']['R'][i][0] = leg
            c.data[side + 'Foot']['R'][i][0] = -(up + leg) - 10                  # the sole stays level on the rung
        c.data['Spine']['R'][i][0] -= 6; c.data['Hips']['R'][i][0] += 2          # leans back off the ladder, arms taking the weight
        c.data['Hips']['T'][i][2] += 10; c.data['Hips']['T'][i][0] += 3 * math.sin(ph)
        c.data['Head']['R'][i][0] -= 18                                          # looks up the ladder
    # hands: each reaches for the rung above as the opposite knee rises, elbows bent, solved every fourth frame and blended
    for side, sgn, x in (('Left', 1, 18), ('Right', -1, -18)):
        keys = {}
        for i in range(0, n, 4):
            ph = 2 * math.pi * i / n; reach = 0.5 + 0.5 * math.sin(ph * sgn + math.pi)
            pos, _ = af.fk(rig, c, i)
            target = [x, pos['Head'][1] + 12 + 16 * reach, pos['Hips'][2] + 30]
            keys[i] = af.solve_arm(rig, c, i, side, target)[:2]
        ks = sorted(keys)
        for i in range(n):
            k0 = max(k for k in ks if k <= i); k1 = min([k for k in ks if k > i] + [ks[0] + n])
            w = (i - k0) / max(1, k1 - k0); a1 = keys[k1 % n]
            c.data[side + 'Arm']['R'][i] = af._mix(keys[k0][0], a1[0], w, True)
            c.data[side + 'ForeArm']['R'][i] = af._mix(keys[k0][1], a1[1], w, True)
    return c.loopify(4)

def crawl(): return clip('Moving Backward In Crawl Position').reverse('Crawl Forward').loopify(4)
def prone_crawl_alt():
    c = clip('Moving Backward In Prone Position').reverse('Prone Crawl Forward Alt')
    c = c.offset('Head', (12, 0, 0)); c = c.offset('Neck', (6, 0, 0))          # helmet kept down under fire
    return c.loopify(4)

def prone_death():
    n = 45
    c = clip('Prone Idle').hold(0, n, 'Prone Death')
    hit = bump(0.08, 0.10); settle = trap(0.10, 0.30, 1.0, 1.0); over = bump(0.32, 0.08)
    c = c.offset('Spine', (12, 0, 0), hit); c = c.offset('Head', (-14, 0, 0), hit)                  # the round lands
    c = c.offset('Head', (6, 0, 38), settle); c = c.offset('Neck', (10, 0, 6), settle)              # the head goes over
    c = c.offset('Head', (-6, 0, 0), over)                                                          # and overshoots
    c = c.offset('Spine1', (-5, 0, 3), settle); c = c.offset('Hips', (0, 0, 7), settle)
    c = c.offset('Hips', (0, -3, 0), settle, kind='T')
    c = c.offset('RightUpLeg', (0, 0, 9), settle); c = c.offset('LeftUpLeg', (0, 0, -6), settle)   # legs splay on the ground
    c = c.offset('RightLeg', (38, 0, 0), settle); c = c.offset('RightFoot', (20, 0, 0), settle)     # the bent knee goes flat, the toe points
    c = ik_hold(c, 'Left', lambda p: [p['LeftShoulder'][0] + 38, 3, p['LeftShoulder'][2] + 14], n - 1, settle)
    c = ik_hold(c, 'Right', lambda p: [p['RightShoulder'][0] - 36, 3, p['RightShoulder'][2] + 22], n - 1, settle)
    return c

def prone_flinch():
    c = clip('Prone Idle').slice(0, 18, 'Prone Flinch'); e = react(0.1, 0.15, 0.6)
    c = c.offset('Neck', (20, 0, 0), e); c = c.offset('Head', (25, 0, 0), e); c = c.offset('Spine1', (-8, 0, 0), e)   # the chest flattens
    c = c.offset('LeftArm', (15, 0, 0), e); c = c.offset('RightArm', (15, 0, 0), e)                                     # shoulders bunch
    c = c.offset('RightForeArm', (0, 0, -15), e); c = c.offset('LeftForeArm', (0, 0, 15), e)
    return c

def kneel_flinch():
    c = clip('Rifle Kneel Idle').slice(0, 18, 'Kneel Flinch'); e = react(0.05, 0.15, 0.65)
    c = c.offset('Spine', (20, 0, 0), e); c = c.offset('Spine1', (8, 0, 0), e); c = c.offset('Neck', (8, 0, 0), e); c = c.offset('Head', (4, 0, 0), e)
    c = c.offset('LeftArm', (12, 0, 0), e); c = c.offset('RightArm', (12, 0, 0), e)                 # the hands come with the curl
    c = c.offset('RightForeArm', (0, 0, -32), e); c = c.offset('LeftForeArm', (0, 0, 32), e)
    return c

def get_up():
    c = af.Clip.concat([clip('Rifle Prone To Kneel').retime(1.15), clip('Rifle Kneel To Stand').retime(1.3)], fade=8, name='Get Up From Prone')
    return c.strip_root()   # the sim owns his place; a drift here would slide him and snap him back

def stumble():
    c = clip('Rifle Run').repeat(3, 'Stumble Running'); e = trap(0.38, 0.42, 0.46, 0.68); late = trap(0.40, 0.44, 0.48, 0.70)
    c = c.offset('Spine', (28, 0, 0), e); c = c.offset('Hips', (12, 0, 0), e); c = c.offset('Neck', (10, 0, 0), e)
    c = c.offset('Head', (15, 0, 0), late)                                                          # the head follows, two frames late
    c = drop_hips(c, 12, e)
    c = c.offset('RightArm', (45, 0, 0), e); c = c.offset('LeftArm', (45, 0, 0), e)                # arms reach forward and down to catch
    c = c.offset('RightForeArm', (0, 0, 20), e); c = c.offset('LeftForeArm', (0, 0, -20), e)      # elbows straighten
    return c

def mask():
    n = 78
    c = clip('Rifle Idle').slice(0, n, 'Mask Donning')
    bag = trap(0.0, 0.12, 0.22, 0.30); face = trap(0.22, 0.32, 0.55, 0.62); straps = trap(0.55, 0.62, 0.80, 0.92)
    c = ik_hold(c, 'Right', lambda p: [p['Hips'][0] - 5, p['Hips'][1] + 30, p['Hips'][2] + 25], 0, bag)            # the hand goes to the bag on the chest
    c = ik_hold(c, 'Left', lambda p: [p['Head'][0] + 7, p['Head'][1] - 6, p['Head'][2] + 14], 0, face)              # both hands bring the mask up
    c = ik_hold(c, 'Right', lambda p: [p['Head'][0] - 7, p['Head'][1] - 6, p['Head'][2] + 14], 0, face)
    c = ik_hold(c, 'Left', lambda p: [p['Head'][0] + 8, p['Head'][1] + 4, p['Head'][2] - 8], 0, straps)             # then reach behind the head for the straps
    c = ik_hold(c, 'Right', lambda p: [p['Head'][0] - 8, p['Head'][1] + 4, p['Head'][2] - 8], 0, straps)
    c = c.offset('Head', (20, 0, 0), trap(0.0, 0.12, 0.22, 0.30)); c = c.offset('Head', (30, 0, 0), face)          # face goes down into the mask
    c = c.offset('Head', (-15, 0, 0), straps)                                                                       # chin lifts through the straps
    c = c.offset('Spine', (8, 0, 0), face)
    return c

def burning():
    c = clip('Sprint Forward').repeat(2, 'Burning Run').resample(40)
    for leg in ('Left', 'Right'):
        c = c.scale_motion(leg + 'UpLeg', 0.7, axis=0); c = c.scale_motion(leg + 'Leg', 0.8, axis=0)   # a stagger, not a stride
    n = c.n
    for i in range(n):
        c.data['Head']['R'][i][0] -= 10; c.data['Neck']['R'][i][0] -= 8; c.data['Spine']['R'][i][0] += 14   # folding over
    c = c.wave('Hips', 2, 12, 1); c = c.wave('Hips', 0, 8, 1, kind='T'); c = c.wave('Head', 1, 25, 3)        # rolls, lurches, the head thrashes
    # each arm swings between two solved poses: thrown up over the head, and clawing at the face; out of step with each other
    for side, x, ph in (('Left', 1, 0.0), ('Right', -1, 1.9)):
        pos, _ = af.fk(rig, c, 0)
        up = af.solve_arm(rig, c, 0, side, [pos['Head'][0] + 22 * x, pos['Head'][1] + 22, pos['Head'][2] + 4])[:2]
        face = af.solve_arm(rig, c, 0, side, [pos['Head'][0] + 10 * x, pos['Head'][1] - 2, pos['Head'][2] + 26])[:2]
        for i in range(n):
            w = 0.5 + 0.5 * math.sin(4 * math.pi * i / n + ph)
            c.data[side + 'Arm']['R'][i] = af._mix(up[0], face[0], w, True)
            c.data[side + 'ForeArm']['R'][i] = af._mix(up[1], face[1], w, True)
    return c.loopify(4)

def point():
    c = clip('Idle').resample(66, 'Officer Point')
    raise_ = trap(0.0, 0.18, 0.18, 0.28); chop = trap(0.18, 0.28, 0.72, 1.0)
    c = ik_hold(c, 'Right', lambda p: [p['RightShoulder'][0] - 6, p['RightShoulder'][1] + 40, p['RightShoulder'][2] + 34], 0, raise_)   # up
    c = ik_hold(c, 'Right', lambda p: [p['RightShoulder'][0] - 6, p['RightShoulder'][1] + 10, p['RightShoulder'][2] + 54], 0, chop)     # and chops to the point
    c = ik_hold(c, 'Left', lambda p: [p['Hips'][0] + 18, p['Hips'][1] + 2, p['Hips'][2] + 8], 0, trap(0.0, 0.25, 0.75, 1.0))          # the other hand on the hip
    c = c.offset('Spine', (0, -15, 0), chop); c = c.offset('Head', (-8, 0, 0), chop)   # turns into the point, chin up
    return c

def whistle():
    c = clip('Idle').resample(48, 'Officer Whistle')
    c = ik_hold(c, 'Right', lambda p: [p['Head'][0] - 2, p['Head'][1] - 7, p['Head'][2] + 12], 0, trap(0.0, 0.15, 0.70, 0.85))
    c = ik_hold(c, 'Left', lambda p: [p['LeftShoulder'][0] + 8, p['LeftShoulder'][1] + 52, p['LeftShoulder'][2] + 8], 0, trap(0.05, 0.25, 0.70, 0.90))   # the other arm straight up: follow me
    c = c.offset('Head', (-8, 0, 0), trap(0.2, 0.3, 0.6, 0.75)); c = c.offset('Spine', (-4, 0, 0), trap(0.15, 0.3, 0.5, 0.75))   # head back to blow, a breath in
    return c

def mg_carry():
    c = clip('Rifle Walk').retime(1 / 1.3, 'MG Carry Walk')
    c = c.scale_motion('LeftUpLeg', 0.8, axis=0); c = c.scale_motion('RightUpLeg', 0.8, axis=0)   # short steps under the weight
    c = ik_hold(c, 'Right', lambda p: [p['Hips'][0] - 20, p['Hips'][1] - 6, p['Hips'][2] + 24], 0, None)
    c = ik_hold(c, 'Left', lambda p: [p['Hips'][0] + 4, p['Hips'][1] + 14, p['Hips'][2] + 44], 0, None)
    c = c.offset('Spine', (-6, 0, 10)); c = drop_hips(c, 2)                 # leans back off the weight
    c = c.wave('Hips', 1, 2, 2, kind='T')                                   # bobs with the stride
    c = c.wave('LeftArm', 0, 3, 2); c = c.wave('RightArm', 0, 3, 2)          # the gun heaves with each step
    return c.loopify(4)

def wire():
    base = clip('Walk Crouching Forward').retime(1 / 2.0, 'Wire Crossing')
    base = high_step(base, 15, 1.5, 0.5, 1.5)
    base = base.offset('Spine', (10, 0, 0)); base = base.offset('Neck', (10, 0, 0)); base = drop_hips(base, 5)
    base = base.layer(clip('Rifle Crouch Walk'), ARMS)
    return base.loopify(6)

MAKE = [
    ('Wade Forward', wade, 'loop', 'water 0.5 to 1.0 m: Walk With Rifle (1) slowed 1.7x, thighs lifted with the forward swing scaled 1.5x and the trailing 0.6x, knees bent to the lower hips, bobbing, arms from Rifle Crouch Walk'),
    ('Ladder Climb', ladder, 'loop', 'procedural: alternate knees dwelling on each rung, soles level, hands to the rung above with bent elbows (IK), leaning back; in place, the sim lifts the man'),
    ('Crawl Forward', crawl, 'loop', 'Moving Backward In Crawl Position reversed'),
    ('Prone Crawl Forward Alt', prone_crawl_alt, 'loop', 'Moving Backward In Prone Position reversed, helmet kept down: a second belly crawl'),
    ('Prone Death', prone_death, 'once', 'Prone Idle: a jolt, the head goes over in 0.3 s and overshoots, the arms slide out (IK to the floor), the bent knee goes flat, 1.5 s'),
    ('Prone Flinch', prone_flinch, 'once', 'Prone Idle: head pressed down, chest flattened, shoulders bunched; fast in, slow out, 0.6 s'),
    ('Kneel Flinch', kneel_flinch, 'once', 'Rifle Kneel Idle curling forward with the hands, fast in, slow out, 0.6 s'),
    ('Get Up From Prone', get_up, 'once', 'Rifle Prone To Kneel at 1.15x into Rifle Kneel To Stand at 1.3x, cross-faded over 8 frames, root travel stripped'),
    ('Stumble Running', stumble, 'once', 'three cycles of Rifle Run; a 3-frame trip: torso pitches, knees buckle, arms reach down to catch, 13 frames to recover'),
    ('Mask Donning', mask, 'once', 'Rifle Idle: right hand to the chest bag, both hands bring the mask up as the face goes down into it, then behind the head for the straps as the chin lifts'),
    ('Burning Run', burning, 'loop', 'Sprint Forward shortened to a stagger, folded over, hips rolling and lurching, head thrashing, arms thrown between over the head and the face (two IK poses each, out of step)'),
    ('Officer Point', point, 'once', 'Idle, the right arm raised high then chopped forward to the point (IK), the left hand on the hip, the body turned into it, chin up'),
    ('Officer Whistle', whistle, 'once', 'Idle, the right hand to the mouth, the left arm straight up (follow me), head back to blow, 0.6 s'),
    ('MG Carry Walk', mg_carry, 'loop', 'Rifle Walk at 0.77x, short steps, right hand at the hip and left on the barrel (IK), leaning back off the weight, the gun heaving with the stride'),
    ('Wire Crossing', wire, 'loop', 'Walk Crouching Forward at 0.5x, thighs lifted with the forward swing 1.5x and the trailing 0.5x, stooped, watching the ground, arms from Rifle Crouch Walk'),
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
