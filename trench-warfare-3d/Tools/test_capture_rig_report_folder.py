"""A bare report path must not ask CreateDirectory for an empty folder name.

Directory.CreateDirectory("") throws ArgumentException. Path.GetDirectoryName("stress.json")
is "". CaptureRig used to call that at every write, so a stress report with a bare file name
never finished and StressRun spun in Play.
"""
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
RIG = os.path.join(HERE, '..', 'Assets', '_Project', 'Editor', 'CaptureRig.cs')
TAG = '[rebase-create-the-report-folder-only-when-the-p-20261019]'


def _src():
    with open(RIG, encoding='utf-8') as f:
        return f.read()


class CaptureRigReportFolderTest(unittest.TestCase):
    def test_a_bare_report_path_does_not_create_an_empty_folder(self):
        src = _src()
        direct = 'Directory.CreateDirectory(Path.GetDirectoryName('
        hits = src.count(direct)
        self.assertEqual(
            hits, 0,
            '%s CaptureRig still creates the folder of a path before checking the path names one (%d sites)'
            % (TAG, hits))
        match = re.search(
            r'static void EnsureFolder\(string path\)\s*\{(?P<body>.*?)\n        \}',
            src, re.S)
        self.assertIsNotNone(match, '%s EnsureFolder is missing' % TAG)
        body = match.group('body')
        self.assertIn('IsNullOrEmpty', body, '%s a bare name still reaches CreateDirectory' % TAG)
        self.assertIn('CreateDirectory', body, '%s a nested path creates no folder' % TAG)
        calls = re.findall(r'^\s+EnsureFolder\([^)]+\);$', src, re.M)
        self.assertGreaterEqual(
            len(calls), 6,
            '%s expected the six report writes to go through EnsureFolder, found %d' % (TAG, len(calls)))


if __name__ == '__main__':
    unittest.main()
