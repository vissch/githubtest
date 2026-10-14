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
            "NullReferenceException: boom @ at TW.Foo.Bar() || TW.Foo.Bar <- wrapper managed-to-native <- X.Y",
            "ERROR",
            "wrapper managed-to-native only in the appended TW_OTR_STACK dump, not the head");
        allOk &= Case(m,
            "NullReferenceException: boom @ at wrapper managed-to-native X.Y() || X.Y <- Z",
            "ENGINE",
            "wrapper managed-to-native in the head frame itself");

        Console.WriteLine(allOk ? "[O9] PASS all cases" : "[O9] FAIL one or more cases");

        // [O11] run Runner.Main over this very assembly, filtered to the O11 fixtures, and read its verdicts.
        var lines = RunRunner("O11");
        allOk &= Verdict(lines, "IGNORE", "O11IgnoreCases.Ignored", "[Ignore] is not run");
        allOk &= Verdict(lines, "RUNNER", "O11SourceCases.FromSource", "[TestCaseSource] is seen and left to NUnit");
        allOk &= Verdict(lines, "FAIL", "O11ExpectedCases.Wrong", "[Test(ExpectedResult)] with a wrong return fails");
        allOk &= Verdict(lines, "PASS", "O11ExpectedCases.Right", "[Test(ExpectedResult)] with the right return passes");
        allOk &= Verdict(lines, "FAIL", "O11ExpectedCases.CaseWrong(2)", "[TestCase(ExpectedResult)] with a wrong return fails");
        allOk &= Verdict(lines, "LOGERR", "O11LogAssertCases.Logs", "a LogAssert test runs; the error it logs still fails");
        allOk &= Verdict(lines, "RUNNER", "O11NsCases.Works", "a [SetUpFixture] namespace is left to NUnit");

        return allOk ? 0 : 1;
    }

    static string[] RunRunner(string filter)
    {
        string loc = typeof(Program).Assembly.Location;
        Environment.SetEnvironmentVariable("TW_RUN_DIRS", System.IO.Path.GetDirectoryName(loc));
        var old = Console.Out;
        var buf = new System.IO.StringWriter();
        try
        {
            Console.SetOut(buf);
            var main = typeof(Runner).GetMethod("Main", BindingFlags.NonPublic | BindingFlags.Static);
            main.Invoke(null, new object[] { new[] { loc, filter } });
        }
        finally { Console.SetOut(old); }
        return buf.ToString().Split(new[] { Environment.NewLine, "\n" }, StringSplitOptions.None);
    }

    static bool Verdict(string[] lines, string verdict, string label, string what)
    {
        string got = null;
        foreach (var l in lines)
        {
            var parts = l.Split(new[] { "  " }, 2, StringSplitOptions.None);
            if (parts.Length == 2 && parts[1].Split(new[] { "  " }, 2, StringSplitOptions.None)[0] == label) got = parts[0];
        }
        bool ok = got == verdict;
        Console.WriteLine((ok ? "[O11] PASS " : "[O11] FAIL ") + what + " -> " + label + " expected " + verdict + " got " + (got ?? "(absent)"));
        return ok;
    }

    static bool Case(MethodInfo m, string err, string expected, string label)
    {
        string got = (string)m.Invoke(null, new object[] { err });
        bool ok = got == expected;
        Console.WriteLine((ok ? "[O9] PASS " : "[O9] FAIL ") + label + " -> expected " + expected + " got " + got);
        return ok;
    }
}
