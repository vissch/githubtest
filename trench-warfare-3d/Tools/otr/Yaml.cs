// Tools/otr.py: the subset of Unity's YAML a ScriptableObject .asset uses (block mappings, sequences whose "- " sits at
// the key's own indent or deeper, flow mappings {x: 1, y: 2}, flow sequences [], plain and quoted scalars), read into
// the same tree FakeJson fills objects from (Dictionary / List / string), so an asset's fields land by Unity's
// serializer rules. Object references ({fileID: ...}) stay dictionaries and are not resolved.
using System;
using System.Collections.Generic;
using System.Linq;

static class FakeYaml
{
    /// <summary>The fields of the document's first object (the mapping under "MonoBehaviour:").</summary>
    public static Dictionary<string, object> AssetFields(string text)
    {
        var lines = text.Replace("\r\n", "\n").Split('\n')
            .Where(l => l.Trim().Length > 0 && !l.StartsWith("%") && !l.StartsWith("---") && !l.TrimStart().StartsWith("#"))
            .ToList();
        int i = 0;
        var root = ParseBlock(lines, ref i, 0);
        if (root is Dictionary<string, object> d && d.Values.FirstOrDefault() is Dictionary<string, object> body) return body;
        return root as Dictionary<string, object> ?? new Dictionary<string, object>();
    }

    static int Indent(string l) { int n = 0; while (n < l.Length && l[n] == ' ') n++; return n; }

    static object ParseBlock(List<string> lines, ref int i, int indent)
    {
        if (i >= lines.Count) return null;
        string first = lines[i].Substring(Indent(lines[i]));
        if (first.StartsWith("- ") || first == "-") return ParseSeq(lines, ref i, Indent(lines[i]));
        return ParseMap(lines, ref i, Indent(lines[i]));
    }

    static Dictionary<string, object> ParseMap(List<string> lines, ref int i, int indent)
    {
        var d = new Dictionary<string, object>();
        while (i < lines.Count)
        {
            int ind = Indent(lines[i]);
            string s = lines[i].Substring(ind);
            if (ind < indent || (ind == indent && s.StartsWith("-"))) break;
            if (ind > indent) { i++; continue; }   // stray deeper line: skip
            int c = KeyColon(s);
            if (c < 0) { i++; continue; }
            string key = s.Substring(0, c).Trim();
            string rest = s.Substring(c + 1).Trim();
            i++;
            if (rest.Length > 0) { d[key] = Scalar(rest); continue; }
            if (i < lines.Count)
            {
                int ni = Indent(lines[i]); string ns = lines[i].Substring(ni);
                if (ns.StartsWith("-") && ni >= indent) { d[key] = ParseSeq(lines, ref i, ni); continue; }
                if (ni > indent) { d[key] = ParseMap(lines, ref i, ni); continue; }
            }
            d[key] = "";
        }
        return d;
    }

    static List<object> ParseSeq(List<string> lines, ref int i, int indent)
    {
        var l = new List<object>();
        while (i < lines.Count)
        {
            int ind = Indent(lines[i]);
            string s = lines[i].Substring(ind);
            if (ind != indent || !s.StartsWith("-")) break;
            string item = s.Length > 1 ? s.Substring(1).TrimStart() : "";
            if (item.Length == 0) { i++; l.Add(i < lines.Count && Indent(lines[i]) > indent ? ParseBlock(lines, ref i, Indent(lines[i])) : ""); continue; }
            if (KeyColon(item) > 0 && !item.StartsWith("{") && !item.StartsWith("[") && !item.StartsWith("'") && !item.StartsWith("\""))
            {
                // "- key: value" opens a mapping whose further keys sit at the item's text column
                int col = ind + (s.Length - item.Length);
                lines[i] = new string(' ', col) + item;
                l.Add(ParseMap(lines, ref i, col));
                continue;
            }
            i++;
            l.Add(Scalar(item));
        }
        return l;
    }

    static int KeyColon(string s)
    {
        bool q1 = false, q2 = false; int depth = 0;
        for (int k = 0; k < s.Length; k++)
        {
            char ch = s[k];
            if (ch == '\'' && !q2) q1 = !q1;
            else if (ch == '"' && !q1) q2 = !q2;
            else if (!q1 && !q2 && (ch == '{' || ch == '[')) depth++;
            else if (!q1 && !q2 && (ch == '}' || ch == ']')) depth--;
            else if (!q1 && !q2 && depth == 0 && ch == ':' && (k + 1 == s.Length || s[k + 1] == ' ')) return k;
        }
        return -1;
    }

    static object Scalar(string s)
    {
        s = s.Trim();
        if (s.StartsWith("{")) { int k = 0; return Flow(s, ref k); }
        if (s.StartsWith("[")) { int k = 0; return Flow(s, ref k); }
        if (s.Length >= 2 && s[0] == '\'' && s[s.Length - 1] == '\'') return s.Substring(1, s.Length - 2).Replace("''", "'");
        if (s.Length >= 2 && s[0] == '"' && s[s.Length - 1] == '"') return s.Substring(1, s.Length - 2).Replace("\\\"", "\"").Replace("\\n", "\n");
        return s;
    }

    static object Flow(string s, ref int k)
    {
        while (k < s.Length && s[k] == ' ') k++;
        if (s[k] == '{')
        {
            var d = new Dictionary<string, object>(); k++;
            while (k < s.Length)
            {
                while (k < s.Length && (s[k] == ' ' || s[k] == ',')) k++;
                if (k < s.Length && s[k] == '}') { k++; break; }
                int c = s.IndexOf(':', k); string key = s.Substring(k, c - k).Trim(); k = c + 1;
                while (k < s.Length && s[k] == ' ') k++;
                if (s[k] == '{' || s[k] == '[') d[key] = Flow(s, ref k);
                else { int e = k; while (e < s.Length && s[e] != ',' && s[e] != '}') e++; d[key] = Scalar(s.Substring(k, e - k)); k = e; }
            }
            return d;
        }
        if (s[k] == '[')
        {
            var l = new List<object>(); k++;
            while (k < s.Length)
            {
                while (k < s.Length && (s[k] == ' ' || s[k] == ',')) k++;
                if (k < s.Length && s[k] == ']') { k++; break; }
                if (s[k] == '{' || s[k] == '[') l.Add(Flow(s, ref k));
                else { int e = k; while (e < s.Length && s[e] != ',' && s[e] != ']') e++; l.Add(Scalar(s.Substring(k, e - k))); k = e; }
            }
            return l;
        }
        return "";
    }
}
