// Phase: A5b (2026-09-29) — where Tools/crabsplit.py put each node of a walker (and the Cutter), read from the manifest it
// wrote beside the models, Resources/Vehicles/crabs.json: every part at its pivot and every socket at its place, in the
// machine's frame (metres, Y up, front +Z). TankImport puts the imported nodes there, because Blender's FBX export moves
// a node two levels under the root by up to 1.4 times its parent's offset (Tools/battleform.py, compensate): the Kettle's
// legs stood 0.27 m off its body, the Redoubt's feet 0.86 m off their shins, the Cutter's gun 6.2 m off its hull.
// The file is json with objects keyed by name, which JsonUtility cannot read, so a small reader is here.
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using UnityEngine;

namespace TW.Editor
{
    public static class CrabManifest
    {
        public const string Path = TankImport.Folder + "crabs.json";

        /// <summary>Every node crabsplit.py placed for this machine (parts and sockets), by name, in the machine's frame;
        /// null when the manifest has no such machine.</summary>
        public static Dictionary<string, Vector3> Places(string machine)
        {
            if (!File.Exists(Path)) return null;
            if (!(Json.Parse(File.ReadAllText(Path)) is Dictionary<string, object> top)
                || !(top.TryGetValue("crabs", out var crabs) && crabs is Dictionary<string, object> all)
                || !(all.TryGetValue(machine, out var one) && one is Dictionary<string, object> crab)) return null;
            var places = new Dictionary<string, Vector3>();
            if (crab.TryGetValue("pivots", out var pv) && pv is Dictionary<string, object> pivots)
                foreach (var kv in pivots) places[kv.Key] = Vec(kv.Value);
            if (crab.TryGetValue("sockets", out var sv) && sv is Dictionary<string, object> sockets)
                foreach (var kv in sockets)
                    if (kv.Value is Dictionary<string, object> s && s.TryGetValue("pos", out var pos)) places[kv.Key] = Vec(pos);
            return places;
        }

        /// <summary>The machine a model file belongs to: "Kettle" for .../Kettle_LOD1.fbx.</summary>
        public static string MachineOf(string assetPath)
        {
            string file = System.IO.Path.GetFileNameWithoutExtension(assetPath);
            int lod = file.LastIndexOf("_LOD", System.StringComparison.Ordinal);
            return lod > 0 ? file.Substring(0, lod) : file;
        }

        static Vector3 Vec(object o)
        {
            var l = (List<object>)o;
            return new Vector3((float)(double)l[0], (float)(double)l[1], (float)(double)l[2]);
        }

        /// <summary>Objects, arrays, numbers, strings, true, false, null: enough for the manifests the split tools write.</summary>
        static class Json
        {
            public static object Parse(string s) { int i = 0; return Value(s, ref i); }

            static void Skip(string s, ref int i) { while (i < s.Length && char.IsWhiteSpace(s[i])) i++; }

            static object Value(string s, ref int i)
            {
                Skip(s, ref i);
                char c = s[i];
                if (c == '{')
                {
                    var d = new Dictionary<string, object>(); i++;
                    for (Skip(s, ref i); s[i] != '}'; Skip(s, ref i))
                    {
                        string k = Str(s, ref i); Skip(s, ref i); i++;   // the ':'
                        d[k] = Value(s, ref i); Skip(s, ref i);
                        if (s[i] == ',') i++;
                    }
                    i++; return d;
                }
                if (c == '[')
                {
                    var l = new List<object>(); i++;
                    for (Skip(s, ref i); s[i] != ']'; Skip(s, ref i))
                    {
                        l.Add(Value(s, ref i)); Skip(s, ref i);
                        if (s[i] == ',') i++;
                    }
                    i++; return l;
                }
                if (c == '"') return Str(s, ref i);
                if (string.CompareOrdinal(s, i, "true", 0, 4) == 0) { i += 4; return true; }
                if (string.CompareOrdinal(s, i, "false", 0, 5) == 0) { i += 5; return false; }
                if (string.CompareOrdinal(s, i, "null", 0, 4) == 0) { i += 4; return null; }
                int start = i;
                while (i < s.Length && "+-0123456789.eE".IndexOf(s[i]) >= 0) i++;
                return double.Parse(s.Substring(start, i - start), NumberStyles.Float, CultureInfo.InvariantCulture);
            }

            static string Str(string s, ref int i)
            {
                var b = new System.Text.StringBuilder(); i++;   // the opening quote
                for (; s[i] != '"'; i++)
                {
                    if (s[i] != '\\') { b.Append(s[i]); continue; }
                    char e = s[++i];
                    if (e == 'u') { b.Append((char)int.Parse(s.Substring(i + 1, 4), NumberStyles.HexNumber)); i += 4; }
                    else b.Append(e == 'n' ? '\n' : e == 't' ? '\t' : e == 'r' ? '\r' : e == 'b' ? '\b' : e == 'f' ? '\f' : e);
                }
                i++; return b.ToString();
            }
        }
    }
}
