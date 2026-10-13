// [O9] drives Runner.Classify (private, invoked via reflection) on crafted err strings.
using System;
using System.Reflection;

static class Program
{
    static int Main()
    {
        var m = typeof(Runner).GetMethod("Classify", BindingFlags.NonPublic | BindingFlags.Static);
        bool allOk = true;

        allOk &= Case(m, "MissingMethodException: otr engine: no such method @ frame", "ENGINE",
            "a real stand-in throw prefixed \"otr engine: \"");
        allOk &= Case(m, "MissingMethodException: Foo.Bar not found @ frame", "ERROR",
            "an unrelated MissingMethodException (e.g. a stale/mismatched test dll)");
        allOk &= Case(m,
            "AssertionException: nope @ frame || Foo.Bar <- wrapper managed-to-native <- Baz.Qux",
            "FAIL",
            "wrapper managed-to-native only in the appended TW_OTR_STACK dump, not the head");

        Console.WriteLine(allOk ? "[O9] PASS all cases" : "[O9] FAIL one or more cases");
        return allOk ? 0 : 1;
    }

    static bool Case(MethodInfo m, string err, string expected, string label)
    {
        string got = (string)m.Invoke(null, new object[] { err });
        bool ok = got == expected;
        Console.WriteLine((ok ? "[O9] PASS " : "[O9] FAIL ") + label + " -> expected " + expected + " got " + got);
        return ok;
    }
}
