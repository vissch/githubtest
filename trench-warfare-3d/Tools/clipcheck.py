"""Motion checks a contact sheet cannot make: for each made clip, how far the planted foot slides while it is
the low foot (skate, cm per step), how far the hips travel over the clip (root drift, cm), the lowest any foot or
hand goes (below 0 = through the floor), and the closest a hand comes to the head joint (helmet clipping).
Usage: python Tools/clipcheck.py "<download folder>/Made" [Rifle Idle.fbx template folder = parent]
"""
import sys, os, math, glob
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import animforge as af

made = sys.argv[1]
src = sys.argv[2] if len(sys.argv) > 2 else os.path.dirname(made.rstrip('/\\'))
_, roots, _, _ = af.read_fbx(os.path.join(src, 'Rifle Idle.fbx'))
rig = af.Rig(roots)

def dist(a, b): return math.sqrt(sum((a[i] - b[i]) ** 2 for i in range(3)))

print('%-26s %6s %6s %7s %7s %7s  %s' % ('clip', 'skate', 'drift', 'footMin', 'handMin', 'hand-hd', 'note'))
for path in sorted(glob.glob(os.path.join(made, '*.fbx'))):
    c = af.Clip.load(path)
    frames = [af.fk(rig, c, i)[0] for i in range(c.n)]
    hips = [f['Hips'] for f in frames]
    drift = dist([hips[0][0], 0, hips[0][2]], [hips[-1][0], 0, hips[-1][2]])
    feet = ['LeftFoot', 'RightFoot']; hands = ['LeftHand', 'RightHand']
    foot_min = min(f[b][1] for f in frames for b in feet)
    hand_min = min(f[b][1] for f in frames for b in hands)
    head_gap = min(dist(f[h], f['Head']) for f in frames for h in hands)
    # skate: while a foot is the lower one and near its lowest, how far it moves in XZ per frame, summed per plant
    skate = 0.0; worst = 0.0; planted = None; run = 0.0
    for i in range(1, c.n):
        low = min(feet, key=lambda b: frames[i][b][1])
        if frames[i][low][1] > foot_min + 6: cur = None
        else: cur = low
        if cur is not None and cur == planted:
            d = dist([frames[i][cur][0], 0, frames[i][cur][2]], [frames[i - 1][cur][0], 0, frames[i - 1][cur][2]])
            run += d
        else:
            worst = max(worst, run); run = 0.0
        planted = cur
    worst = max(worst, run)
    note = []
    if worst > 12: note.append('foot skates %.0f cm a plant' % worst)
    if drift > 25: note.append('root drifts %.0f cm' % drift)
    if foot_min < -4: note.append('foot %.0f cm under the floor' % -foot_min)
    if head_gap < 14: note.append('hand %.0f cm from the head joint (helmet)' % head_gap)
    print('%-26s %6.1f %6.1f %7.1f %7.1f %7.1f  %s' % (os.path.basename(path)[:-4], worst, drift, foot_min, hand_min, head_gap, '; '.join(note)))
