# Tests for animforge.py (the pure-Python FBX clip reader/writer). Run: python Tools/test_animforge.py
import os, struct, sys, tempfile, unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from animforge import Node, Clip, write_fbx, KTIME, FPS

MAGIC = b'\xf8\x5a\x8c\x6a\xde\xf5\xd9\x7e\xec\xe9\x0c\xe3\x75\x8f\x29\x0b'
FID = b'\xfa\xbc\xab\x09' + b'\0' * 12          # the footer id Mixamo's files carry


def S(text): return ('S', text.encode('latin1'))


def p70(key, *values):
    """One Properties70 entry: the key, three unused type words, then the values from props[4] on."""
    return Node('P', [S(key), S(''), S(''), S('')] + [('D', v) for v in values], [], False)


def curve(cid, times, values):
    return Node('AnimationCurve', [('L', cid), S('AnimCurve::'), S('')], [
        Node('KeyTime', [('l', list(times))], [], False),
        Node('KeyValueFloat', [('f', list(values))], [], False),
    ], False)


class FooterTest(unittest.TestCase):
    def test_r12_the_footer_matches_the_fbx_layout(self):
        """[R12] footer id, 4 zeros, 1..16 pad bytes, version, 120 zeros, magic (Blender io_scene_fbx/encode_bin.py)."""
        path = os.path.join(tempfile.mkdtemp(), 'footer.fbx')
        roots = [Node('Objects', [], [Node('Model', [('L', 1), S('Model::Hips'), S('LimbNode')], [], False)], True),
                 Node('Connections', [], [], True)]
        write_fbx(path, 7400, roots, FID, MAGIC)
        with open(path, 'rb') as f: data = f.read()
        self.assertEqual(MAGIC, data[-16:], "[R12] the file does not end with the FBX magic")
        self.assertEqual(b'\0' * 120, data[-136:-16], "[R12] the 120 zero bytes before the magic are missing")
        self.assertEqual(7400, struct.unpack_from('<I', data, len(data) - 140)[0],
                         "[R12] the version word must sit 140 bytes from the end")
        self.assertEqual(0, (len(data) - 140) % 16,
                         "[R12] the version word is not 16-byte aligned: the padding was written before the four "
                         "zero bytes instead of after them")
        after_id = data.index(FID) + 16
        self.assertEqual(b'\0' * 4, data[after_id:after_id + 4],
                         "[R12] the four zero bytes must come straight after the footer id")
        pad = (len(data) - 140) - (after_id + 4)
        self.assertTrue(1 <= pad <= 16, "[R12] the padding must be 1..16 bytes, was %d" % pad)
        self.assertEqual(b'\0' * pad, data[after_id + 4:after_id + 4 + pad],
                         "[R12] the padding must be zero bytes")


class FlatBoneTest(unittest.TestCase):
    """[R12] a bone animated on one or two axes must keep the motion it has."""

    def build(self):
        xs, ys = [0.0, 10.0, 20.0], [1.0, 2.0, 3.0]
        times = [int(round(i * KTIME / FPS)) for i in range(3)]
        cx, cy = curve(101, times, xs), curve(102, times, ys)
        cn = Node('AnimationCurveNode', [('L', 200), S('AnimCurveNode::R'), S('')], [
            Node('Properties70', [], [p70('d|X', 0.0), p70('d|Y', 0.0), p70('d|Z', 7.5)], False)], False)
        stack = Node('AnimationStack', [('L', 300), S('AnimStack::take'), S('')], [
            Node('Properties70', [], [
                Node('P', [S('LocalStart'), S(''), S(''), S(''), ('L', 0)], [], False),
                Node('P', [S('LocalStop'), S(''), S(''), S(''), ('L', times[-1])], [], False)], False)], False)
        model = Node('Model', [('L', 1), S('Model::mixamorig:Spine'), S('LimbNode')], [], False)
        objects = Node('Objects', [], [model, cn, cx, cy, stack], True)
        conns = Node('Connections', [], [
            Node('C', [S('OP'), ('L', 200), ('L', 1), S('Lcl Rotation')], [], False),
            Node('C', [S('OP'), ('L', 101), ('L', 200), S('d|X')], [], False),
            Node('C', [S('OP'), ('L', 102), ('L', 200), S('d|Y')], [], False),
        ], True)
        path = os.path.join(tempfile.mkdtemp(), 'clip.fbx')
        write_fbx(path, 7400, [objects, conns], FID, MAGIC)
        return path, xs, ys

    def test_r12_a_bone_on_two_axes_keeps_its_curves(self):
        """[R12] X and Y come back as written, Z takes the curve node's own d|Z constant."""
        path, xs, ys = self.build()
        c = Clip.load(path)
        self.assertIn('Spine', c.bones(), "[R12] a bone animated on two axes was dropped from the clip")
        frames = c.data['Spine']['R']
        self.assertEqual(3, len(frames), "[R12] the clip must have the stack's 3 frames")
        self.assertEqual(xs, [f[0] for f in frames], "[R12] the X curve was lost, so the bone saves flat")
        self.assertEqual(ys, [f[1] for f in frames], "[R12] the Y curve was lost, so the bone saves flat")
        self.assertEqual([7.5] * 3, [f[2] for f in frames],
                         "[R12] the axis without a curve must hold its AnimationCurveNode's d|Z value")


if __name__ == '__main__':
    unittest.main(verbosity=2)
