// Offline NUnit-ish runner: loads a compiled TW test dll outside Unity and runs every [Test]/[TestCase] with
// [OneTimeSetUp]/[SetUp]/[TearDown]/[OneTimeTearDown]. Classifies: PASS, FAIL (assertion), IGNORE, ENGINE (the test
// reached native Unity code, which cannot run here), ERROR (any other exception: likely a real bug), TIMEOUT.
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;

static class Runner
{
    static string[] dirs;
    static int Main(string[] args)
    {
        string dll = args[0]; string filter = args.Length > 1 ? args[1] : "";
        dirs = Environment.GetEnvironmentVariable("TW_RUN_DIRS").Split(';');
        AppDomain.CurrentDomain.AssemblyResolve += (s, e) =>
        {
            string name = new AssemblyName(e.Name).Name + ".dll";
            foreach (var d in dirs) { var p = Path.Combine(d, name); if (File.Exists(p)) return Assembly.LoadFrom(p); }
            return null;
        };
        var asm = Assembly.LoadFrom(dll);
        Type[] types;
        try { types = asm.GetTypes(); }
        catch (ReflectionTypeLoadException e)
        {
            types = e.Types.Where(t => t != null).ToArray();
            foreach (var le in e.LoaderExceptions.Take(5)) Console.WriteLine("LOADER " + le.Message);
        }
        var counts = new Dictionary<string, int>();
        foreach (var t in types.OrderBy(t => t.FullName))
        {
            if (t.IsAbstract && !t.IsSealed) continue;
            if (filter != "" && !t.Name.Contains(filter)) continue;
            var methods = t.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
            var tests = methods.Where(m => Has(m, "TestAttribute") || Has(m, "TestCaseAttribute") || Has(m, "UnityTestAttribute")).ToList();
            if (tests.Count == 0) continue;
            object inst = null;
            string fixtureError = null;
            try { if (!(t.IsAbstract && t.IsSealed)) inst = Activator.CreateInstance(t, true); } catch (Exception e) { fixtureError = Describe(e); }
            if (fixtureError == null) fixtureError = RunAll(methods, "OneTimeSetUpAttribute", inst);
            foreach (var m in tests.OrderBy(m => m.Name))
            {
                if (Has(m, "UnityTestAttribute")) { Tally(counts, "SKIP-UNITYTEST"); continue; }
                if (Has(m, "ExplicitAttribute")) { Tally(counts, "SKIP-EXPLICIT"); continue; }
                foreach (var argset in Cases(m))
                {
                    string label = t.Name + "." + m.Name + (argset.Length > 0 ? "(" + string.Join(",", argset.Select(a => a == null ? "null" : a.ToString())) + ")" : "");
                    string verdict, detail = "";
                    if (fixtureError != null) { verdict = Classify(fixtureError); detail = "fixture: " + fixtureError; }
                    else
                    {
                        string err = null;
                        var th = new Thread(() =>
                        {
                            err = RunAll(methods, "SetUpAttribute", inst);
                            if (err == null) { try { m.Invoke(m.IsStatic ? null : inst, argset); } catch (Exception e) { err = Describe(e); } }
                            string td = RunAll(methods, "TearDownAttribute", inst);
                            if (err == null && td != null) err = "teardown: " + td;
                        }, 256 * 1024 * 1024);
                        th.Start();
                        if (!th.Join(60000)) { try { th.Abort(); } catch { } verdict = "TIMEOUT"; }
                        else if (err == null) verdict = "PASS";
                        else { verdict = Classify(err); detail = err; }
                    }
                    Tally(counts, verdict);
                    Console.WriteLine(verdict + "  " + label + (detail != "" ? "  " + Trim(detail) : ""));
                }
            }
            RunAll(methods, "OneTimeTearDownAttribute", inst);
        }
        Console.WriteLine("---- " + string.Join("  ", counts.OrderBy(k => k.Key).Select(k => k.Key + "=" + k.Value)));
        return 0;
    }

    static bool Has(MemberInfo m, string attr) => m.GetCustomAttributes(true).Any(a => a.GetType().Name == attr);

    static IEnumerable<object[]> Cases(MethodInfo m)
    {
        var tc = m.GetCustomAttributes(true).Where(a => a.GetType().Name == "TestCaseAttribute").ToList();
        if (tc.Count == 0)
        {
            // [Values] / [ValueSource] on the parameters: every combination
            var sets = new List<object[]>();
            foreach (var pi in m.GetParameters())
            {
                object[] vals = null;
                foreach (var a in pi.GetCustomAttributes(true))
                {
                    var at = a.GetType();
                    if (at.Name == "ValuesAttribute")
                    {
                        var f = at.GetField("data", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
                        vals = f != null ? (object[])f.GetValue(a) : null;
                        if ((vals == null || vals.Length == 0) && pi.ParameterType == typeof(bool)) vals = new object[] { false, true };
                        if ((vals == null || vals.Length == 0) && pi.ParameterType.IsEnum) vals = Enum.GetValues(pi.ParameterType).Cast<object>().ToArray();
                    }
                    else if (at.Name == "ValueSourceAttribute")
                    {
                        string src = (string)at.GetProperty("SourceName").GetValue(a, null);
                        var owner = (Type)at.GetProperty("SourceType").GetValue(a, null) ?? m.DeclaringType;
                        var mem = owner.GetMember(src, BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic).FirstOrDefault();
                        object raw = mem is FieldInfo fi ? fi.GetValue(null) : mem is PropertyInfo pp ? pp.GetValue(null, null) : mem is MethodInfo mi ? mi.Invoke(null, null) : null;
                        if (raw is System.Collections.IEnumerable en) vals = en.Cast<object>().ToArray();
                    }
                }
                if (vals == null) { sets = null; break; }
                sets.Add(vals.Select(v => Coerce(v, pi.ParameterType)).ToArray());
            }
            if (sets == null || sets.Count == 0) { yield return new object[0]; yield break; }
            IEnumerable<object[]> combos = new[] { new object[0] };
            foreach (var s in sets) { var cur = s; combos = combos.SelectMany(c => cur.Select(v => c.Concat(new[] { v }).ToArray())).ToList(); }
            foreach (var c in combos) yield return c;
            yield break;
        }
        foreach (var a in tc)
        {
            var args = (object[])a.GetType().GetProperty("Arguments").GetValue(a, null);
            var ps = m.GetParameters();
            var conv = new object[ps.Length];
            for (int i = 0; i < ps.Length; i++)
            {
                object v = i < args.Length ? args[i] : null;
                try
                {
                    var pt = ps[i].ParameterType;
                    conv[i] = v == null || pt.IsInstanceOfType(v) ? v : (pt.IsEnum ? Enum.ToObject(pt, v) : Convert.ChangeType(v, pt));
                }
                catch { conv[i] = v; }
            }
            yield return conv;
        }
    }

    static object Coerce(object v, Type pt)
    {
        try { return v == null || pt.IsInstanceOfType(v) ? v : (pt.IsEnum ? Enum.ToObject(pt, v) : Convert.ChangeType(v, pt)); } catch { return v; }
    }

    static string RunAll(MethodInfo[] methods, string attr, object inst)
    {
        foreach (var m in methods.Where(x => Has(x, attr)))
        {
            try { m.Invoke(m.IsStatic ? null : inst, null); }
            catch (Exception e) { return Describe(e); }
        }
        return null;
    }

    static string Describe(Exception e)
    {
        while ((e is TargetInvocationException || e is TypeInitializationException) && e.InnerException != null) e = e.InnerException;
        var st = (e.StackTrace ?? "").Split('\n').Select(l => l.Trim()).Where(l => l.Length > 0).ToList();
        string frame = st.Count > 0 ? st[0] : "";
        string ours = st.FirstOrDefault(l => l.Contains(" TW.")) ?? "";
        return e.GetType().Name + ": " + e.Message.Replace("\n", " ").Replace("\r", "") + " @ " + frame + (ours != "" && ours != frame ? " | ours " + ours : "");
    }

    static string Classify(string err)
    {
        if (err.Contains("@ at NUnit.Framework.TestContext")) return "RUNNER";   // TestContext exists only under NUnit's own runner
        if (err.StartsWith("AssertionException") || err.StartsWith("MultipleAssertException")) return "FAIL";
        if (err.StartsWith("IgnoreException") || err.StartsWith("InconclusiveException")) return "IGNORE";
        if (err.StartsWith("SuccessException")) return "PASS";
        if (err.Contains("ECall") || err.Contains("SecurityException") || err.StartsWith("MissingMethodException")) return "ENGINE";
        if (err.Contains("wrapper managed-to-native")) return "ENGINE";
        return "ERROR";
    }

    static string Trim(string s) => s.Length > 900 ? s.Substring(0, 900) + "..." : s;
    static void Tally(Dictionary<string, int> c, string k) { c[k] = c.TryGetValue(k, out var n) ? n + 1 : 1; }
}
