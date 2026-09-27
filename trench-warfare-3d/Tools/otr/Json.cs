// Unity's JsonUtility rules in managed code, for Tools/otr.py's stand-in engine (Engine.cs registers it as
// JsonUtility's internal calls): public and [SerializeField] instance fields (not readonly, not [NonSerialized]) of
// [Serializable] classes and structs; primitives, enums (as numbers), strings, one-dimensional arrays, List<T>, nested
// serializable types (a null nested class writes as its defaults); no dictionaries, properties or statics. Reading:
// unknown keys are ignored, a missing key leaves the field as it was, malformed text throws ArgumentException as
// Unity's does. Floats are written round-trip ("R"). Pretty printing indents by four spaces.
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;

static class FakeJson
{
    const BindingFlags Inst = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
    static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

    static IEnumerable<FieldInfo> Fields(Type t)
    {
        var chain = new List<Type>();
        for (var c = t; c != null && c != typeof(object) && c != typeof(ValueType); c = c.BaseType) chain.Insert(0, c);
        foreach (var c in chain)
            foreach (var f in c.GetFields(Inst | BindingFlags.DeclaredOnly))
            {
                if (f.IsInitOnly || f.IsNotSerialized || f.IsLiteral) continue;
                bool marked = f.GetCustomAttributes(true).Any(a => a.GetType().Name == "SerializeField" || a.GetType().Name == "SerializeReference");
                if (!f.IsPublic && !marked) continue;
                if (!Supported(f.FieldType)) continue;
                yield return f;
            }
    }

    static Type ElementOf(Type t)
    {
        if (t.IsArray) return t.GetElementType();
        if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(List<>)) return t.GetGenericArguments()[0];
        return null;
    }

    static bool Supported(Type t)
    {
        if (t == typeof(IntPtr) || t == typeof(UIntPtr) || t.IsPointer) return false;
        if (t.IsPrimitive || t.IsEnum || t == typeof(string)) return true;
        var e = ElementOf(t);
        if (e != null) return (!t.IsArray || t.GetArrayRank() == 1) && ElementOf(e) == null && Supported(e);
        if (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(Dictionary<,>)) return false;
        if (typeof(Delegate).IsAssignableFrom(t) || t.IsAbstract || t.IsInterface) return false;
        if (t.Namespace == "UnityEngine" && t.IsValueType) return true;   // Vector3, Color, Quaternion...: their public fields
        return t.IsSerializable;
    }

    // ---- writing ----
    public static string ToJson(object o, bool pretty)
    {
        if (o == null) return "";
        var sb = new StringBuilder();
        WriteObject(sb, o, o.GetType(), pretty, 0);
        return sb.ToString();
    }

    static void Indent(StringBuilder sb, bool pretty, int depth) { if (pretty) { sb.Append('\n'); sb.Append(' ', depth * 4); } }

    static void WriteObject(StringBuilder sb, object o, Type t, bool pretty, int depth)
    {
        if (o == null) { try { o = Activator.CreateInstance(t, true); } catch { sb.Append("{}"); return; } }
        sb.Append('{');
        bool first = true;
        foreach (var f in Fields(t))
        {
            if (!first) sb.Append(',');
            first = false;
            Indent(sb, pretty, depth + 1);
            sb.Append('"').Append(f.Name).Append("\":");
            if (pretty) sb.Append(' ');
            WriteValue(sb, f.GetValue(o), f.FieldType, pretty, depth + 1);
        }
        if (!first) Indent(sb, pretty, depth);
        sb.Append('}');
    }

    static void WriteValue(StringBuilder sb, object v, Type t, bool pretty, int depth)
    {
        if (t == typeof(string)) { WriteString(sb, (string)v ?? ""); return; }
        if (t == typeof(bool)) { sb.Append((bool)v ? "true" : "false"); return; }
        if (t.IsEnum) { sb.Append(Convert.ToInt64(v, Inv).ToString(Inv)); return; }
        if (t == typeof(float)) { float f = (float)v; sb.Append(float.IsNaN(f) || float.IsInfinity(f) ? "0" : f.ToString("R", Inv)); return; }
        if (t == typeof(double)) { double d = (double)v; sb.Append(double.IsNaN(d) || double.IsInfinity(d) ? "0" : d.ToString("R", Inv)); return; }
        if (t == typeof(char)) { sb.Append(((int)(char)v).ToString(Inv)); return; }
        if (t.IsPrimitive) { sb.Append(Convert.ToString(v, Inv)); return; }
        var elem = ElementOf(t);
        if (elem != null)
        {
            sb.Append('[');
            var list = v as IList;
            if (list != null && list.Count > 0)
            {
                for (int i = 0; i < list.Count; i++)
                {
                    if (i > 0) sb.Append(',');
                    Indent(sb, pretty, depth + 1);
                    WriteValue(sb, list[i], elem, pretty, depth + 1);
                }
                Indent(sb, pretty, depth);
            }
            sb.Append(']');
            return;
        }
        WriteObject(sb, v, t, pretty, depth);
    }

    static void WriteString(StringBuilder sb, string s)
    {
        sb.Append('"');
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
                    if (c < 0x20) sb.Append("\\u").Append(((int)c).ToString("x4", Inv)); else sb.Append(c);
                    break;
            }
        }
        sb.Append('"');
    }

    // ---- reading ----
    public static object FromJson(string json, object target, Type type)
    {
        if (type == null && target != null) type = target.GetType();
        if (string.IsNullOrWhiteSpace(json)) return target;
        int i = 0;
        object tree = Parse(json, ref i);
        Skip(json, ref i);
        if (i != json.Length) throw new ArgumentException("JSON parse error: The document root must not follow by other values.");
        var obj = tree as Dictionary<string, object>;
        if (obj == null) throw new ArgumentException("JSON must represent an object type.");
        object o = target ?? Activator.CreateInstance(type, true);
        Fill(o, type, obj);
        return o;
    }

    static void Fill(object o, Type t, Dictionary<string, object> obj)
    {
        foreach (var f in Fields(t))
            if (obj.TryGetValue(f.Name, out var v))
                f.SetValue(o, To(v, f.FieldType, f.GetValue(o)));
    }

    /// <summary>Fill an existing object from a parsed tree (JSON or Unity YAML).</summary>
    public static void FillFrom(object o, Dictionary<string, object> tree) => Fill(o, o.GetType(), tree);

    static object To(object v, Type t, object current)
    {
        if (t == typeof(string)) return v as string ?? (v == null ? null : Convert.ToString(v, Inv));
        if (v is string sv && t != typeof(string))
        {
            // a YAML scalar: numbers, 0/1 booleans, enum numbers or names
            if (t.IsEnum) { if (long.TryParse(sv, NumberStyles.Integer, Inv, out long el)) return Enum.ToObject(t, el); try { return Enum.Parse(t, sv); } catch { return current ?? Activator.CreateInstance(t); } }
            if (t == typeof(bool)) return sv == "1" || sv.Equals("true", StringComparison.OrdinalIgnoreCase);
            if (double.TryParse(sv, NumberStyles.Float, Inv, out double parsed)) v = parsed;
            else if (t.IsPrimitive) return current ?? Activator.CreateInstance(t);
        }
        if (t == typeof(bool)) return v is bool b ? b : (v is double d0 && d0 != 0);
        double num = v is double dd ? dd : (v is bool bb ? (bb ? 1 : 0) : 0);
        if (t.IsEnum) return Enum.ToObject(t, (long)num);
        if (t.IsPrimitive)
        {
            if (t == typeof(float)) return (float)num;
            if (t == typeof(double)) return num;
            if (t == typeof(char)) return (char)(int)num;
            return Convert.ChangeType(Math.Truncate(num), t, Inv);
        }
        var elem = ElementOf(t);
        if (elem != null)
        {
            var src = v as List<object> ?? new List<object>();
            if (t.IsArray)
            {
                var arr = Array.CreateInstance(elem, src.Count);
                for (int k = 0; k < src.Count; k++) arr.SetValue(To(src[k], elem, null), k);
                return arr;
            }
            var list = (IList)Activator.CreateInstance(t);
            foreach (var e in src) list.Add(To(e, elem, null));
            return list;
        }
        if (v is Dictionary<string, object> obj)
        {
            object o = current ?? Activator.CreateInstance(t, true);
            Fill(o, t, obj);
            return o;
        }
        return current ?? (t.IsValueType ? Activator.CreateInstance(t) : null);
    }

    static void Skip(string s, ref int i) { while (i < s.Length && char.IsWhiteSpace(s[i])) i++; }
    static Exception Bad(int i) => new ArgumentException("JSON parse error: Invalid value (at " + i + ").");

    static object Parse(string s, ref int i)
    {
        Skip(s, ref i);
        if (i >= s.Length) throw Bad(i);
        char c = s[i];
        if (c == '{')
        {
            var d = new Dictionary<string, object>();
            i++; Skip(s, ref i);
            if (i < s.Length && s[i] == '}') { i++; return d; }
            while (true)
            {
                Skip(s, ref i);
                if (i >= s.Length || s[i] != '"') throw Bad(i);
                string key = ParseString(s, ref i);
                Skip(s, ref i);
                if (i >= s.Length || s[i] != ':') throw Bad(i);
                i++;
                d[key] = Parse(s, ref i);
                Skip(s, ref i);
                if (i < s.Length && s[i] == ',') { i++; continue; }
                if (i < s.Length && s[i] == '}') { i++; return d; }
                throw Bad(i);
            }
        }
        if (c == '[')
        {
            var l = new List<object>();
            i++; Skip(s, ref i);
            if (i < s.Length && s[i] == ']') { i++; return l; }
            while (true)
            {
                l.Add(Parse(s, ref i));
                Skip(s, ref i);
                if (i < s.Length && s[i] == ',') { i++; continue; }
                if (i < s.Length && s[i] == ']') { i++; return l; }
                throw Bad(i);
            }
        }
        if (c == '"') return ParseString(s, ref i);
        if (string.CompareOrdinal(s, i, "true", 0, 4) == 0) { i += 4; return true; }
        if (string.CompareOrdinal(s, i, "false", 0, 5) == 0) { i += 5; return false; }
        if (string.CompareOrdinal(s, i, "null", 0, 4) == 0) { i += 4; return null; }
        int start = i;
        while (i < s.Length && "+-0123456789.eE".IndexOf(s[i]) >= 0) i++;
        if (i == start) throw Bad(i);
        if (!double.TryParse(s.Substring(start, i - start), NumberStyles.Float, Inv, out double n)) throw Bad(start);
        return n;
    }

    static string ParseString(string s, ref int i)
    {
        var sb = new StringBuilder();
        i++;
        while (i < s.Length && s[i] != '"')
        {
            char c = s[i++];
            if (c != '\\') { sb.Append(c); continue; }
            if (i >= s.Length) throw Bad(i);
            char e = s[i++];
            switch (e)
            {
                case 'n': sb.Append('\n'); break;
                case 'r': sb.Append('\r'); break;
                case 't': sb.Append('\t'); break;
                case 'b': sb.Append('\b'); break;
                case 'f': sb.Append('\f'); break;
                case 'u': sb.Append((char)Convert.ToInt32(s.Substring(i, 4), 16)); i += 4; break;
                default: sb.Append(e); break;
            }
        }
        if (i >= s.Length) throw Bad(i);
        i++;
        return sb.ToString();
    }
}
