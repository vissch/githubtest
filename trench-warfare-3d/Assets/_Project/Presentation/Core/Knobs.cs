// Phase: tooling (AOSA loop, 2026-09-25) - run-time knobs: a presentation constant set without a rebuild.
// A knob is a name (area.camelCase) and a string value. The values come from the TW_KNOBS env var, then from every
// `-twknob <value>` pair on the command line (the command line wins), then from Parse/Set (a bench string's `knobs=`).
// A reader asks once, in Awake/Start or where it allocates, with the old constant as the fallback: when nothing is
// set every value is the old constant and the build behaves as before. Every name ever asked for is logged with the
// value it resolved to, so a bench report can say what it measured (ToJson). Names match without regard to case.
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using UnityEngine;

namespace TW.Presentation
{
    public static class Knobs
    {
        public const string CommandLineArg = "-twknob", EnvVar = "TW_KNOBS";

        static readonly object gate = new object();
        static readonly Dictionary<string, string> overrides = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        static readonly Dictionary<string, string> read = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        static readonly HashSet<string> warned = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        static bool initialised;
        static volatile int generation;

        /// <summary>Goes up by one on every Parse, Set and Clear.</summary>
        public static int Generation => generation;

        /// <summary>A copy of what is set now (name -> raw value).</summary>
        public static IReadOnlyDictionary<string, string> Overrides
        {
            get { lock (gate) { EnsureInit(); return new Dictionary<string, string>(overrides, StringComparer.OrdinalIgnoreCase); } }
        }

        /// <summary>A copy of every name ever passed to Get, with the value it resolved to (invariant culture; a bool is
        /// "1" or "0"). The first read of a name is the one kept.</summary>
        public static IReadOnlyDictionary<string, string> Read
        {
            get { lock (gate) { return new Dictionary<string, string>(read, StringComparer.OrdinalIgnoreCase); } }
        }

        public static float Get(string name, float fallback)
        {
            string raw, warn = null;
            float value = fallback;
            lock (gate)
            {
                EnsureInit();
                if (overrides.TryGetValue(name, out raw))
                {
                    if (float.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture, out float v) && !float.IsNaN(v) && !float.IsInfinity(v)) value = v;
                    else warn = Bad(name, raw, "a number");
                }
                Log(name, value.ToString("R", CultureInfo.InvariantCulture));
            }
            if (warn != null) Debug.LogWarning(warn);
            return value;
        }

        public static int Get(string name, int fallback)
        {
            string raw, warn = null;
            int value = fallback;
            lock (gate)
            {
                EnsureInit();
                if (overrides.TryGetValue(name, out raw))
                {
                    if (int.TryParse(raw, NumberStyles.Integer, CultureInfo.InvariantCulture, out int v)) value = v;
                    // "2.0" from a sweep that stepped a float is still 2; "2.5" is not an integer and falls back
                    else if (double.TryParse(raw, NumberStyles.Float, CultureInfo.InvariantCulture, out double d)
                             && d == Math.Floor(d) && d >= int.MinValue && d <= int.MaxValue) value = (int)d;
                    else warn = Bad(name, raw, "an integer");
                }
                Log(name, value.ToString(CultureInfo.InvariantCulture));
            }
            if (warn != null) Debug.LogWarning(warn);
            return value;
        }

        /// <summary>"1"/"true" and "0"/"false" (any case); anything else falls back.</summary>
        public static bool Get(string name, bool fallback)
        {
            string raw, warn = null;
            bool value = fallback;
            lock (gate)
            {
                EnsureInit();
                if (overrides.TryGetValue(name, out raw))
                {
                    string t = raw.Trim();
                    if (t == "1" || string.Equals(t, "true", StringComparison.OrdinalIgnoreCase)) value = true;
                    else if (t == "0" || string.Equals(t, "false", StringComparison.OrdinalIgnoreCase)) value = false;
                    else warn = Bad(name, raw, "1, 0, true or false");
                }
                Log(name, value ? "1" : "0");
            }
            if (warn != null) Debug.LogWarning(warn);
            return value;
        }

        /// <summary>Adds or overrides knobs from "a=1,b=2"; '|' and ';' separate entries as well as ','.</summary>
        public static void Parse(string raw)
        {
            List<string> warns;
            lock (gate)
            {
                EnsureInit();
                warns = ParseInto(raw);
                generation++;
            }
            if (warns != null) foreach (var w in warns) Debug.LogWarning(w);
        }

        public static void Set(string name, string value)
        {
            if (string.IsNullOrWhiteSpace(name)) return;
            lock (gate)
            {
                EnsureInit();
                overrides[name.Trim()] = value == null ? "" : value.Trim();
                generation++;
            }
        }

        /// <summary>Drops every override and the read log (and the env var and command line are not read again).</summary>
        public static void Clear()
        {
            lock (gate)
            {
                overrides.Clear(); read.Clear(); warned.Clear();
                initialised = true;
                generation++;
            }
        }

        /// <summary>{"set":{...},"read":{...}}, keys sorted (ordinal), strings escaped.</summary>
        public static string ToJson()
        {
            var sb = new StringBuilder(256);
            lock (gate)
            {
                EnsureInit();
                sb.Append("{\"set\":");
                Append(sb, overrides);
                sb.Append(",\"read\":");
                Append(sb, read);
                sb.Append("}");
            }
            return sb.ToString();
        }

        // ---- inside the lock

        static void EnsureInit()
        {
            if (initialised) return;
            initialised = true;
            List<string> warns = null;
            try
            {
                string env = Environment.GetEnvironmentVariable(EnvVar);
                if (!string.IsNullOrEmpty(env)) warns = ParseInto(env);
                var args = Environment.GetCommandLineArgs();
                for (int i = 0; i + 1 < args.Length; i++)
                    if (string.Equals(args[i], CommandLineArg, StringComparison.OrdinalIgnoreCase))
                    {
                        var more = ParseInto(args[i + 1]);
                        if (more != null) { if (warns == null) warns = more; else warns.AddRange(more); }
                    }
            }
            catch (Exception e)
            {
                warns ??= new List<string>();
                warns.Add("Knobs: could not read " + EnvVar + " or the command line: " + e.Message);
            }
            // logged in the lock only at the first use; Debug.LogWarning never calls back into Knobs
            if (warns != null) foreach (var w in warns) Debug.LogWarning(w);
        }

        static List<string> ParseInto(string raw)
        {
            if (string.IsNullOrEmpty(raw)) return null;
            List<string> warns = null;
            foreach (var entry in raw.Split(Separators, StringSplitOptions.RemoveEmptyEntries))
            {
                string e = entry.Trim();
                if (e.Length == 0) continue;
                int eq = e.IndexOf('=');
                string name = eq > 0 ? e.Substring(0, eq).Trim() : "";
                if (name.Length == 0)
                {
                    warns ??= new List<string>();
                    warns.Add("Knobs: ignored '" + e + "' (expected name=value)");
                    continue;
                }
                overrides[name] = e.Substring(eq + 1).Trim();
            }
            return warns;
        }
        static readonly char[] Separators = { ',', '|', ';' };

        static string Bad(string name, string raw, string wanted)
        {
            if (!warned.Add(name)) return null;
            return "Knobs: " + name + "=" + raw + " is not " + wanted + "; the default is used";
        }

        static void Log(string name, string resolved)
        {
            if (!read.ContainsKey(name)) read[name] = resolved;
        }

        static void Append(StringBuilder sb, Dictionary<string, string> map)
        {
            var keys = new List<string>(map.Keys);
            keys.Sort(StringComparer.Ordinal);
            sb.Append("{");
            for (int i = 0; i < keys.Count; i++)
            {
                if (i > 0) sb.Append(",");
                Quote(sb, keys[i]); sb.Append(":"); Quote(sb, map[keys[i]]);
            }
            sb.Append("}");
        }

        static void Quote(StringBuilder sb, string s)
        {
            sb.Append("\"");
            foreach (char c in s)
            {
                switch (c)
                {
                    case '"': sb.Append("\\\""); break;
                    case '\\': sb.Append("\\\\"); break;
                    case '\n': sb.Append("\\n"); break;
                    case '\r': sb.Append("\\r"); break;
                    case '\t': sb.Append("\\t"); break;
                    default:
                        if (c < ' ') sb.Append("\\u").Append(((int)c).ToString("x4", CultureInfo.InvariantCulture));
                        else sb.Append(c);
                        break;
                }
            }
            sb.Append("\"");
        }
    }
}
