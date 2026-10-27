#!/usr/bin/env python3
"""Side-by-side comps of the chlorine bank at night: today's patches against a rolling-cloud stand-in.

WHY. The owner, on the Decide page, to "The chlorine bank at night: flat green patches, or a new sheet for a
rolling cloud": "i need visuals to decide, give me visuals". Nothing about the look is decided here. Option (a)
is the build as it stands (`be5e6444fc`, patches, 3.25% of the zoom-30 night frame). Option (b) needs art that
does not exist yet, so its picture is drawn with the SmokeBank sheet as a STAND-IN for a rolling-cloud sheet: its
ink fills its whole cell (`Ink` 0.03-0.87) where GasBank's fills the bottom third (0.00-0.35), which is exactly
the difference the owner is choosing between. A yes to (b) buys a new sheet, not this one.

This reuses `ability-fx-captures-r2-2-g1/probe.py` unchanged for the masking, the 40-lift diff and the camera
maths, which are already proven there: it is imported with its own HERE pointed at this folder.

USAGE
    python comps.py --selftest                           probe's own camera-maths selftest (0.34311 m)
    python comps.py --pack <gasdir> --plate <platedir>    pack gas-t1.png, gas-t1-cloud.png, gas-t1-plate.png
    python comps.py                                       measure both against the plate, write comps.json and
                                                           comp-t1.jpg

Exit 0 only when: both stills and the plate are present; pose_error_m < 0.50 for both; patches bank_area_frac is
0.0325 +/- 0.004 (the number commit be5e6444fc measured three times on this same build with this same harness);
cloud bank_area_frac > 0.005 (it drew); and the two stills differ on at least 1% of the frame (the comparison is
not two copies of one picture). The cloud gets no bar: it is a visual for the owner, nothing about it is decided.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
PROBE_DIR = os.path.join(HERE, '..', 'ability-fx-captures-r2-2-g1')
sys.path.insert(0, os.path.abspath(PROBE_DIR))
import probe  # noqa: E402

probe.HERE = HERE

PATCHES_AREA_LO = 0.0325 - 0.004
PATCHES_AREA_HI = 0.0325 + 0.004
CLOUD_AREA_FLOOR = 0.005
DIFF_FRAC_FLOOR = 0.01
COMPS_JSON = os.path.join(HERE, 'comps.json')
COMP_JPG = os.path.join(HERE, 'comp-t1.jpg')


def do_pack(gasdir, platedir):
    probe.pack(gasdir, 'gas-t1', 't1-patches')
    probe.pack(gasdir, 'gas-t1-cloud', 't1-cloud')
    probe.pack(platedir, 'gas-t1-plate', 't1-plate')
    print('packed t1-patches, t1-cloud, t1-plate')


def compose(patches_img, cloud_img, patches_row, cloud_row):
    from PIL import Image, ImageDraw

    w, h, strip = 1280, 720, 40
    canvas = Image.new('RGB', (w * 2, h + strip), (20, 20, 20))
    canvas.paste(patches_img, (0, strip))
    canvas.paste(cloud_img, (w, strip))
    draw = ImageDraw.Draw(canvas)
    left = '(a) flat green patches - today   bank_area_frac %.4f' % patches_row['bank_area_frac']
    right = '(b) a rolling cloud - stand-in sheet   bank_area_frac %.4f' % cloud_row['bank_area_frac']
    draw.text((8, 10), left, fill=(255, 255, 255))
    draw.text((w + 8, 10), right, fill=(255, 255, 255))
    canvas.save(COMP_JPG, 'JPEG', quality=90)


def do_verdict():
    missing = [s for s in ('t1-patches', 't1-cloud', 't1-plate')
               if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing stills: ' + ', '.join(missing))
        return 1

    patches = probe.measure('t1-patches', 't1-plate')
    cloud = probe.measure('t1-cloud', 't1-plate')

    patches_img, _ = probe.load('t1-patches')
    cloud_img, _ = probe.load('t1-cloud')
    diff = probe.distances(patches_img, cloud_img)
    diff_frac = sum(1 for d in diff if d >= probe.DIFF_BAR) / float(probe.FRAME_PX)

    ok = True
    checks = []

    def check(name, cond):
        nonlocal ok
        ok = ok and cond
        checks.append((name, cond))
        print('%-45s %s' % (name, 'ok' if cond else 'FAILED'))

    check('patches pose_error_m < 0.50 (%.3f)' % patches['pose_error_m'], patches['pose_error_m'] < 0.50)
    check('cloud pose_error_m < 0.50 (%.3f)' % cloud['pose_error_m'], cloud['pose_error_m'] < 0.50)
    check('patches bank_area_frac 0.0325 +/- 0.004 (%.5f)' % patches['bank_area_frac'],
          PATCHES_AREA_LO <= patches['bank_area_frac'] <= PATCHES_AREA_HI)
    check('cloud bank_area_frac > 0.005 (%.5f)' % cloud['bank_area_frac'],
          cloud['bank_area_frac'] > CLOUD_AREA_FLOOR)
    check('stills differ on >= 1%% of the frame (%.4f)' % diff_frac, diff_frac >= DIFF_FRAC_FLOOR)

    result = {'patches': patches, 'cloud': cloud, 'diff_frac': round(diff_frac, 5), 'ok': ok}
    with open(COMPS_JSON, 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)

    if ok:
        compose(patches_img, cloud_img, patches, cloud)
        print('wrote ' + COMP_JPG)

    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


def arg_dirs(flag, count):
    if flag not in sys.argv:
        return None
    at = sys.argv.index(flag) + 1
    got = sys.argv[at:at + count]
    if len(got) < count or any(g.startswith('--') for g in got):
        raise SystemExit('%s wants %d directories' % (flag, count))
    return got


def main():
    if '--selftest' in sys.argv:
        return probe.selftest()
    gas = arg_dirs('--pack', 1)
    if gas is not None:
        plate = arg_dirs('--plate', 1)
        if plate is None:
            raise SystemExit('--pack also needs --plate <dir>')
        do_pack(gas[0], plate[0])
        return 0
    return do_verdict()


if __name__ == '__main__':
    sys.exit(main())
