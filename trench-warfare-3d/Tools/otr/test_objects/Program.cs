// [O14] drives FakeObjects.NormalMatrix and the three JsonUtility rules FakeJson used to miss.
using System;

static class Program
{
    static int Main()
    {
        bool allOk = true;

        // [O14] a mirrored instance: scale(-1,1,1) must turn the normal (1,0,0) into (-1,0,0).
        // The bare cofactor matrix left it (1,0,0), so the triangles flipped and the normals did not.
        var mirror = Mat(-1f, 1f, 1f);
        var n = Apply(FakeObjects.NormalMatrix(mirror, true), 1f, 0f, 0f);
        allOk &= Is("[O14]", Near(n, -1f, 0f, 0f),
                   "a mirrored instance's normal flips with its winding", "got " + V(n));
        var plain = Apply(FakeObjects.NormalMatrix(Mat(1f, 2f, 4f), true), 1f, 0f, 0f);
        allOk &= Is("[O14]", Near(plain, 8f, 0f, 0f),
                   "an unmirrored scale keeps the normal's direction", "got " + V(plain));
        var id = Apply(FakeObjects.NormalMatrix(new float[16], false), 0f, 0f, 1f);
        allOk &= Is("[O14]", Near(id, 0f, 0f, 1f), "no matrices means the identity", "got " + V(id));

        // [O14] a 64-bit field keeps every digit: through double, 9007199254740993 comes back as ...992
        const long Big = 9007199254740993L;
        var read = (Wide)FakeJson.FromJson("{\"big\":9007199254740993}", null, typeof(Wide));
        allOk &= Is("[O14]", read.big == Big, "a long field reads back every digit",
                   "got " + read.big + ", wanted " + Big);
        var back = (Wide)FakeJson.FromJson(FakeJson.ToJson(new Wide { big = Big }, false), null, typeof(Wide));
        allOk &= Is("[O14]", back.big == Big, "a long field round-trips unchanged",
                   "got " + back.big + ", wanted " + Big);

        // [O14] nesting is refused past MaxDepth instead of overflowing the stack
        allOk &= Is("[O14]", NoThrow(Deep(200)), "200 levels of nesting still parse", Throw(Deep(200)));
        string why = Throw(Deep(5000));
        allOk &= Is("[O14]", why != null && why.StartsWith("ArgumentException") && why.Contains("nested"),
                   "5000 levels are an ArgumentException, not a stack overflow", "got " + (why ?? "no throw"));

        // [O14] the serialization callbacks run, once each
        CallbackAsset.Before = CallbackAsset.After = 0;
        string json = FakeJson.ToJson(new CallbackAsset { value = 7 }, false);
        allOk &= Is("[O14]", CallbackAsset.Before == 1, "OnBeforeSerialize runs once per write",
                   "it ran " + CallbackAsset.Before + " times");
        FakeJson.FromJson(json, null, typeof(CallbackAsset));
        allOk &= Is("[O14]", CallbackAsset.After == 1, "OnAfterDeserialize runs once per read",
                   "it ran " + CallbackAsset.After + " times");

        return allOk ? 0 : 1;
    }

    static float[] Mat(float sx, float sy, float sz)
    {
        var m = new float[16];                       // column-major, as a CombineInstance holds it
        m[0] = sx; m[5] = sy; m[10] = sz; m[15] = 1f;
        return m;
    }

    static float[] Apply(float[] nm, float a, float b, float c) =>
        new[] { nm[0] * a + nm[1] * b + nm[2] * c, nm[3] * a + nm[4] * b + nm[5] * c, nm[6] * a + nm[7] * b + nm[8] * c };

    static bool Near(float[] v, float x, float y, float z) =>
        Math.Abs(v[0] - x) < 1e-4f && Math.Abs(v[1] - y) < 1e-4f && Math.Abs(v[2] - z) < 1e-4f;

    static string V(float[] v) => "(" + v[0] + ", " + v[1] + ", " + v[2] + ")";

    static string Deep(int n) => "{\"xs\":" + new string('[', n) + new string(']', n) + "}";

    static string Throw(string json)
    {
        try { FakeJson.FromJson(json, null, typeof(Wide)); return null; }
        catch (Exception e) { return e.GetType().Name + ": " + e.Message; }
    }

    static bool NoThrow(string json) => Throw(json) == null;

    static bool Is(string id, bool ok, string what, string how)
    {
        Console.WriteLine(id + " " + (ok ? "PASS" : "FAIL") + ": " + what + (ok ? "" : " -- " + id + " " + how));
        return ok;
    }
}
