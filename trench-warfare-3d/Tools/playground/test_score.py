# Tests for score.py: run it over a temp capture folder and read the scoreboard it writes.
#   python Tools/playground/test_score.py   -> prints OK, exit 0
import json, os, subprocess, sys, tempfile

SCORE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "score.py")


def a_padded_squad_shot_counts_for_fps_median():
    # [Y16] the fps filter read only the first 200 characters, so a squad shot (whose JSON carries ~40 keys
    # before "fps") never reached "fps median". Any shot with an fps number must be counted.
    with tempfile.TemporaryDirectory() as d:
        shot = {"pad%02d" % i: "x" * 8 for i in range(40)}
        shot["fps"] = 60
        json.dump(shot, open(os.path.join(d, "tst_u9_squad.json"), "w"))
        r = subprocess.run([sys.executable, SCORE, "tst", "-", d], capture_output=True, text=True)
        assert r.returncode == 0, "[Y16] score.py failed: " + r.stderr
        sc = json.load(open(os.path.join(d, "tst_scores.json")))
        assert sc.get("fps median") == 60, "[Y16] a padded squad shot was left out of fps median: %r" % (sc,)


if __name__ == "__main__":
    a_padded_squad_shot_counts_for_fps_median()
    print("OK")
