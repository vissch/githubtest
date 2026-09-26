"""Rescale the prop looks in Resources/Layouts/Battlefield1917.asset to the soldier (docs/21 phase 1).

The looks were learned from the owner's hand edits on 2026-09-22, when a man was drawn 2.67 m tall, and never
rescaled after the 25 % cut of 2026-09-23. This patches the YAML text of the asset with the corrected baselines
(the derivations are in docs/21-overhaul-2026-09.md and docs/13), and rescales every hand edit of those kinds by
new baseline / old baseline so a placed prop keeps its place in the picture. No editor needed.

  python Tools/looks.py --check   # print what would change
  python Tools/looks.py --apply   # write it

Asset text is Unity's own YAML: a look is a "- Module: X" block under "Looks:", an edit a "- Key: ..." block under
"Edits:". Fields are rewritten in place; the newline style of the file is kept.
"""
import io, re, sys

ASSET = "Assets/_Project/Resources/Layouts/Battlefield1917.asset"

# module -> the new look. Scale (0,0,0) means "a multiplier of the composer's own scale" (Size does the scaling);
# Sink is metres at the new baseline. A missing field keeps its old value.
NEW = {
    "Stones/Sandbag":       {"Scale": (1.20, 1.20, 1.20), "ScaleRange": 0.15, "Sink": 0.13},
    "Weapons/FieldGun":     {"Scale": (1.10, 0.89, 1.75), "ScaleRange": 0.10, "Sink": 0.19},
    "Siege/ArmouredStand":  {"Scale": (1.41, 1.36, 1.41), "ScaleRange": 0.10, "Sink": 1.00},
    "Siege/SodShelterRuin": {"Scale": (1.30, 1.30, 1.30), "ScaleRange": 0.08, "Sink": 0.11},
    "Siege/MGNest":         {"Scale": (0.85, 0.85, 0.85), "ScaleRange": 0.10, "Sink": 0.17},
    "Plants/SplitStump":    {"Scale": (0, 0, 0), "Size": 1.4, "ScaleRange": 0.30, "YawRange": 180, "Lean": (5, 5), "LeanRange": 1.0},
    "Plants/FallenLog":     {"Scale": (0, 0, 0), "Size": 1.5, "ScaleRange": 0.30, "Sink": 0.30},
    "Stones/Gabion":        {"ScaleRange": 0.15},
}

VEC = re.compile(r"\{x: ([-\d.eE+]+), y: ([-\d.eE+]+), z: ([-\d.eE+]+)\}")


def fmt(v):
    s = ("%.6g" % v)
    return s if "." in s or "e" in s else s


def vec(t):
    return "{x: %s, y: %s, z: %s}" % (fmt(t[0]), fmt(t[1]), fmt(t[2]))


def blocks(lines, header):
    """Yield (start, end) line ranges of the list items under a top-level '  Header:' line."""
    i = next((k for k, l in enumerate(lines) if l.rstrip("\r\n") == "  " + header + ":"), None)
    if i is None:
        return
    j = i + 1
    while j < len(lines) and lines[j].startswith("  - "):
        k = j + 1
        while k < len(lines) and lines[k].startswith("    "):
            k += 1
        yield (j, k)
        j = k


def field(lines, start, end, name):
    for k in range(start, end):
        body = lines[k].lstrip(" -").rstrip("\r\n")
        if body.startswith(name + ":"):
            return k, body[len(name) + 1:].strip()
    return None, None


def set_field(lines, start, end, name, value):
    k, _ = field(lines, start, end, name)
    if k is None:
        raise SystemExit("no field %s in block at line %d" % (name, start + 1))
    nl = "\r\n" if lines[k].endswith("\r\n") else "\n"
    prefix = lines[k][: len(lines[k]) - len(lines[k].lstrip(" -"))]
    lines[k] = prefix + name + ": " + value + nl


def baseline(scale, size):
    s = scale if scale != (0.0, 0.0, 0.0) else (1.0, 1.0, 1.0)
    return tuple(c * size for c in s)


def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "--check"
    raw = io.open(ASSET, "rb").read().decode("utf-8")
    lines = raw.splitlines(True)
    old_base, old_size, changes = {}, {}, []

    # the looks
    for (a, b) in list(blocks(lines, "Looks")):
        _, module = field(lines, a, b, "Module")
        if module not in NEW:
            continue
        _, scale_txt = field(lines, a, b, "Scale")
        _, size_txt = field(lines, a, b, "Size")
        m = VEC.match(scale_txt)
        scale = tuple(float(x) for x in m.groups())
        size = float(size_txt)
        new = NEW[module]
        new_scale = tuple(float(c) for c in new.get("Scale", scale))
        new_size = float(new.get("Size", size))
        old_base[module] = (baseline(scale, size), size)
        old_size[module] = size
        for name, value in new.items():
            if name in ("Scale", "Lean"):
                txt = vec(value) if name == "Scale" else "{x: %s, y: %s}" % (fmt(value[0]), fmt(value[1]))
            else:
                txt = fmt(value)
            k, cur = field(lines, a, b, name)
            if cur != txt:
                changes.append("look %s: %s %s -> %s" % (module, name, cur, txt))
                set_field(lines, a, b, name, txt)
        NEW[module]["_new_base"] = baseline(new_scale, new_size)
        NEW[module]["_new_size"] = new_size

    # the hand edits: drawn = edit.Scale * Size (the look's Scale never applied to an edit), so a placed prop keeps
    # its share of the new baseline: edit' = edit * oldSize * (newBase / oldBase) / newSize
    for (a, b) in list(blocks(lines, "Edits")):
        _, module = field(lines, a, b, "Module")
        if module not in NEW or "_new_base" not in NEW[module]:
            continue
        _, key = field(lines, a, b, "Key")
        _, scale_txt = field(lines, a, b, "Scale")
        m = VEC.match(scale_txt)
        e = tuple(float(x) for x in m.groups())
        ob, osz = old_base[module]
        nb, nsz = NEW[module]["_new_base"], NEW[module]["_new_size"]
        if nb == ob and nsz == osz:
            continue   # the kind's baseline did not move: its edits stand as they are
        ne = tuple(e[i] * osz * (nb[i] / ob[i]) / nsz for i in range(3))
        txt = vec(ne)
        if txt != scale_txt:
            changes.append("edit %s: Scale %s -> %s" % (key, scale_txt, txt))
            set_field(lines, a, b, "Scale", txt)

    for c in changes:
        print(c)
    print("%d change(s)" % len(changes))
    if mode == "--apply" and changes:
        io.open(ASSET, "wb").write("".join(lines).encode("utf-8"))
        print("written", ASSET)
    elif mode == "--apply":
        print("nothing to write")


if __name__ == "__main__":
    main()
