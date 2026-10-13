// [O11] stand-in NUnit attributes (matched by Runner through GetType().Name only) and fixtures that use the five
// attributes Runner used to mis-run: [Ignore], [TestCaseSource], ExpectedResult, LogAssert.Expect, [SetUpFixture].
using System;
using System.Collections.Generic;

[AttributeUsage(AttributeTargets.Method)]
sealed class TestAttribute : Attribute
{
    object expected;
    public object ExpectedResult { get { return expected; } set { expected = value; HasExpectedResult = true; } }
    public bool HasExpectedResult { get; private set; }
}

[AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
sealed class TestCaseAttribute : Attribute
{
    public TestCaseAttribute(params object[] args) { Arguments = args; }
    public object[] Arguments { get; private set; }
    object expected;
    public object ExpectedResult { get { return expected; } set { expected = value; HasExpectedResult = true; } }
    public bool HasExpectedResult { get; private set; }
}

[AttributeUsage(AttributeTargets.Method | AttributeTargets.Class)]
sealed class IgnoreAttribute : Attribute { public IgnoreAttribute(string reason = "") { } }

[AttributeUsage(AttributeTargets.Method)]
sealed class TestCaseSourceAttribute : Attribute { public TestCaseSourceAttribute(string name) { } }

[AttributeUsage(AttributeTargets.Class)]
sealed class SetUpFixtureAttribute : Attribute { }

[AttributeUsage(AttributeTargets.Method)]
sealed class SetUpAttribute : Attribute { }

/// <summary>Unity's LogAssert, as far as Runner's IL scan has to see it: a call on a type named LogAssert. Expect
/// also files the message the way the stand-in engine files a logged error, so the old Runner came out LOGERR.</summary>
static class LogAssert
{
    public static void Expect(object type, string message) { FakeEngine.Logged.Add("Error: " + message); }
}

class O11IgnoreCases
{
    [Test, Ignore("not ready")] public void Ignored() { }
}

class O11SourceCases
{
    static IEnumerable<object[]> Src() { yield return new object[] { 1 }; }
    [TestCaseSource(nameof(Src))] public void FromSource(int n) { }
}

class O11ExpectedCases
{
    [Test(ExpectedResult = 7)] public int Wrong() { return 3; }
    [Test(ExpectedResult = 7)] public int Right() { return 7; }
    [TestCase(2, ExpectedResult = 5)] public int CaseWrong(int n) { return n; }
}

class O11LogAssertCases
{
    [Test] public void Logs() { LogAssert.Expect(null, "boom"); }
}

namespace O11Fixtured
{
    [SetUpFixture] class NamespaceSetUp { [SetUp] public void Once() { } }

    class O11NsCases
    {
        [Test] public void Works() { }
    }
}
