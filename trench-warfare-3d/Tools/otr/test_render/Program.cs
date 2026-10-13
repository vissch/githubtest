// [O12] [O13] drives FakeRender's extracted decisions: which asset links are unresolved, and which shader names exist.
using System;
using System.Collections.Generic;

static class Program
{
    static int Main()
    {
        bool allOk = true;

        // [O12] a link nested inside a list of assets must be found, not dropped silently
        var nested = new Dictionary<string, object>
        {
            { "m_Name", "Roster" },
            { "entries", new List<object> {
                new Dictionary<string, object> { { "slots", "3" } },
                new Dictionary<string, object> { { "fileID", "11400000" }, { "guid", "deadbeef" } } } }
        };
        allOk &= Is("[O12]", FakeRender.UnresolvedLink(nested) == "entries[1]",
                    "a {fileID, guid} link inside a List<> is found", "got " + (FakeRender.UnresolvedLink(nested) ?? "null"));

        // [O12] a plain asset, and a null link (fileID 0), are not a link
        var plain = new Dictionary<string, object>
        {
            { "m_Script", new Dictionary<string, object> { { "fileID", "11500000" }, { "guid", "aaaa" } } },
            { "speed", "4" },
            { "none", new Dictionary<string, object> { { "fileID", "0" } } },
            { "list", new List<object> { "a", "b" } }
        };
        allOk &= Is("[O12]", FakeRender.UnresolvedLink(plain) == null,
                    "a plain asset (m_Script skipped, fileID 0 ignored) has no unresolved link",
                    "got " + (FakeRender.UnresolvedLink(plain) ?? "null"));

        // [O13] Shader.Find answers only for a scanned file or a real built-in, never for a known prefix
        var files = new Dictionary<string, string> { { "Custom/Toon_URP", @"Assets\Shaders\Toon_URP.shader" } };
        allOk &= Is("[O13]", !FakeRender.ShaderExists(files, "Universal Render Pipeline/Nothing"),
                    "an unscanned name with a known prefix does not exist", "it answered");
        allOk &= Is("[O13]", FakeRender.ShaderExists(files, "Custom/Toon_URP"),
                    "a scanned .shader name exists", "it did not answer");
        allOk &= Is("[O13]", FakeRender.ShaderExists(files, "Hidden/InternalErrorShader"),
                    "a real built-in exists", "it did not answer");

        return allOk ? 0 : 1;
    }

    static bool Is(string id, bool ok, string what, string how)
    {
        Console.WriteLine(id + " " + (ok ? "PASS" : "FAIL") + ": " + what + (ok ? "" : " -- " + id + " " + how));
        return ok;
    }
}
