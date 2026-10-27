#!/usr/bin/env python3
"""Draw the gas bank as a PICTURE a person can look at, from the same stills `probe.py` measures.

WHY. The owner's answer to the fix chain was "show me visual results". The stills are night shots: raw, the gas
reads almost black, so a reviewer opening `run1/gas-t1.jpg` sees a dark field and nothing else. What makes the
bank visible to a person is the PLATE DIFFERENCE boosted: the gas still minus its no-gas plate, times `BOOST`.
So every sheet is three panels side by side -- the gas still, the plate, and the boosted difference -- with the
probe's own largest coherent patch outlined and labelled.

THE NUMBERS ARE IMPORTED, NEVER RE-DERIVED. The outline comes from `probe.bank_mask(..., blob=True)` and the
labels from `probe.measure_t1` / `probe.measure_o240`, so the picture and `probe.json` cannot disagree: if the
mask moves, the outline moves with it. This file measures nothing of its own.

USAGE
    python sheet.py              write sheets/run<N>-t1.png, sheets/run<N>-o240.png and the two contact sheets
    python sheet.py --selftest   the same, then assert each run's gas paint is 8,000-20,000 px and every PNG is there

Exit 0 when every sheet is written (and, with --selftest, when every assertion holds), 1 otherwise.
"""
import os
import sys

from PIL import Image, ImageDraw, ImageFont

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe   # noqa: E402  the one source of every number and of the mask

HERE = os.path.dirname(os.path.abspath(__file__))
SHEETS = os.path.join(HERE, 'sheets')
BOOST = 4                  # the plate difference is multiplied by this: a night bank is invisible at 1x
PANEL_W = 560              # each of the three panels, 16:9 (1280x720 scaled down to fit three across)
LABEL_H = 34               # the strip under each row that carries the measures
OUTLINE = (255, 196, 0)    # the patch outline: a colour no night still holds
# --selftest's window on the gas-direction paint. Source: the landed README of
# docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g2 (commit 87c6d95beb), `gas_paint_px` 13,477 / 13,884 /
# 14,137, widened both ways for a build that moved 181 commits. Not recomputed here.
PAINT_PX_MIN, PAINT_PX_MAX = 8000, 20000


def font(size=15):
    for name in ('arial.ttf', 'DejaVuSans.ttf'):
        try:
            return ImageFont.truetype(name, size)
        except OSError:
            continue
    return ImageFont.load_default()


def boosted(img, plate, boost=BOOST):
    """The plate difference, times `boost`, clamped: what makes the bank visible to a person."""
    a, b = list(img.getdata()), list(plate.getdata())
    out = Image.new('RGB', img.size)
    out.putdata([(min(255, abs(p[0] - q[0]) * boost),
                  min(255, abs(p[1] - q[1]) * boost),
                  min(255, abs(p[2] - q[2]) * boost)) for p, q in zip(a, b)])
    return out


def bbox_of(mask, w, h):
    """The bounding box of a flat mask, or None. The outline the reviewer sees is the probe's own patch."""
    xs, ys = [], []
    for i, on in enumerate(mask):
        if on:
            xs.append(i % w)
            ys.append(i // w)
    if not xs:
        return None
    return min(xs), min(ys), max(xs), max(ys)


def panel(img, box, w, h):
    """One panel: the still scaled to `PANEL_W`, with the patch box outlined in `OUTLINE`."""
    out = img.copy()
    if box:
        ImageDraw.Draw(out).rectangle(box, outline=OUTLINE, width=3)
    scale = PANEL_W / float(w)
    return out.resize((PANEL_W, int(round(h * scale))), Image.LANCZOS)


def row(run_dir, stem, label):
    """A three-panel row for one still: gas, plate, boosted difference, under one label strip."""
    img, _ = probe.load(run_dir, stem)
    plate, _ = probe.load(run_dir, stem + '-plate')
    w, h = img.size
    if stem == 'gas-t1':
        mask, _, _, _ = probe.bank_mask(img, plate, bar=probe.T1_DIFF_BAR, blob=True)
    else:
        # O240 is five separated calls, not one patch (probe.measure_o240's own comment): outline the biggest.
        full, _, _, _ = probe.bank_mask(img, plate, blob=False)
        mask = probe.largest_blob(full, w, h)
    box = bbox_of(mask, w, h)
    panels = [panel(img, box, w, h), panel(plate, None, w, h),
              panel(boosted(img, plate), box, w, h)]
    ph = panels[0].size[1]
    sheet = Image.new('RGB', (PANEL_W * 3, ph + LABEL_H), (16, 16, 16))
    for i, p in enumerate(panels):
        sheet.paste(p, (PANEL_W * i, 0))
    d = ImageDraw.Draw(sheet)
    d.text((6, ph + 8), label, fill=(235, 235, 235), font=font())
    for i, cap in enumerate(('gas', 'no-gas plate', 'plate difference x%d' % BOOST)):
        d.text((PANEL_W * i + 6, 6), cap, fill=(235, 235, 235), font=font(14))
    return sheet


def t1_label(name, m):
    return ('%s  gas-t1   bank_width_m %.3f   bank_rise_m %.3f   bank_aspect %.3f   gas_paint_px %d'
            % (name, m['bank_width_m'], m['bank_rise_m'], m['bank_aspect'], m['gas_paint_px']))


def o240_label(name, m):
    return ('%s  gas-o240   o240_area_frac %.4f   mask %d px (raw %d, patch %d)'
            % (name, m['o240_area_frac'], m['o240_mask_px'], m['o240_mask_px_raw'], m['o240_mask_px_patch']))


def contact(rows, path):
    """The runs stacked: one picture that shows the bank is a sheet on three different frames, not one lucky one."""
    width = max(r.size[0] for r in rows)
    sheet = Image.new('RGB', (width, sum(r.size[1] for r in rows)), (16, 16, 16))
    y = 0
    for r in rows:
        sheet.paste(r, (0, y))
        y += r.size[1]
    sheet.save(path)
    return path


def main():
    runs = probe.find_runs()
    if not runs:
        print('no runs in %s: pack the captures first (probe.py --pack <dir> --into run1)' % HERE)
        return 1
    os.makedirs(SHEETS, exist_ok=True)
    written, paint = [], {}
    t1_rows, o240_rows = [], []
    for d in runs:
        name = os.path.basename(d)
        mt1 = probe.measure_t1(d)
        mo = probe.measure_o240(d)
        paint[name] = mt1['gas_paint_px']
        r1 = row(d, 'gas-t1', t1_label(name, mt1))
        p1 = os.path.join(SHEETS, '%s-t1.png' % name)
        r1.save(p1)
        written.append(p1)
        t1_rows.append(r1)
        r2 = row(d, 'gas-o240', o240_label(name, mo))
        p2 = os.path.join(SHEETS, '%s-o240.png' % name)
        r2.save(p2)
        written.append(p2)
        o240_rows.append(r2)
        print(t1_label(name, mt1))
        print(o240_label(name, mo))
    written.append(contact(t1_rows, os.path.join(SHEETS, 'contact.png')))
    written.append(contact(o240_rows, os.path.join(SHEETS, 'contact-o240.png')))
    for p in written:
        print('wrote %s (%d KB)' % (os.path.relpath(p, HERE), os.path.getsize(p) // 1024))

    if '--selftest' not in sys.argv:
        return 0
    ok = True
    for name in sorted(paint):
        inside = PAINT_PX_MIN <= paint[name] <= PAINT_PX_MAX
        print('selftest %-6s gas_paint_px %d  window %d-%d -> %s'
              % (name, paint[name], PAINT_PX_MIN, PAINT_PX_MAX, 'OK' if inside else 'FAILED'))
        ok = inside and ok
    for p in written:
        there = os.path.exists(p) and os.path.getsize(p) > 0
        if not there:
            print('selftest MISSING %s' % p)
        ok = there and ok
    if len(paint) < 3:
        print('selftest FAILED: %d runs, three needed' % len(paint))
        ok = False
    print('selftest OK' if ok else 'selftest FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
