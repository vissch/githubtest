using System.Collections.Generic;

static unsafe class FakeEngine
{
    public static bool Verbose;
    public static List<string> Logged = new List<string>();
    public static void Install(string cwd) { }
}
