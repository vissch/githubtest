// A stand-in for the slice of Unity's native engine the sim and its tests call, registered as Mono internal calls
// from managed code (mono_add_internal_call in the runtime dll that is already loaded), so Tools/otr.py can run
// NativeArray/NativeList code and IJob / IJobParallelFor jobs with no editor. What it provides:
//   memory     UnsafeUtility.Malloc/Free(Tracked), MemCpy/Move/Set/Clear/Cmp/Replicate/Stride/Swap, type flags
//   safety     AtomicSafetyHandle with real version nodes: every check passes, Release bumps the node so a use after
//              Dispose still throws ObjectDisposedException exactly as in the editor
//   jobs       CreateJobReflectionData keeps the job's managed Execute delegate; Schedule / ScheduleParallelFor run it
//              at once on the calling thread (a valid order: every dependency was scheduled, so ran, before);
//              GetWorkStealingRange hands out the whole range once; every JobHandle is complete
//   burst      GetOrCreateSharedMemory (SharedStatic), execution mode; jobs run managed, as with Burst off
//   misc       Debug.Log (recorded, so a test that logs an error fails as under Unity's runner), Time (time scale
//              settable), Application.isPlaying = false (EditMode), profiler markers as no-ops
// What it is NOT: Burst's float semantics (Burst Strict mode matches managed IEEE maths for the sim's SimMath; any
// difference would show as a hash mismatch against the editor, never inside one run), the job system's threading and
// its race detection (the safety handles never refuse), native rendering, assets, physics, UI layout.
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;

static unsafe class FakeEngine
{
    [DllImport("mono-2.0-bdwgc.dll", CallingConvention = CallingConvention.Cdecl)]
    static extern void mono_add_internal_call([MarshalAs(UnmanagedType.LPStr)] string name, IntPtr method);
    [DllImport("mono-2.0-bdwgc.dll", CallingConvention = CallingConvention.Cdecl)]
    static extern uint mono_gchandle_new(IntPtr obj, int pinned);
    [DllImport("mono-2.0-bdwgc.dll", CallingConvention = CallingConvention.Cdecl)]
    static extern void mono_gchandle_free(uint handle);
    [DllImport("mono-2.0-bdwgc.dll", CallingConvention = CallingConvention.Cdecl)]
    static extern IntPtr mono_gchandle_get_target(uint handle);

    static readonly List<Delegate> keep = new List<Delegate>();
    public static string ProjectDir = "";
    public static readonly List<string> Logged = new List<string>();   // "Error: ..." lines logged during the current test
    public static bool Verbose;
    public static bool Trace = Environment.GetEnvironmentVariable("TW_OTR_TRACE") == "1";
    static readonly Stopwatch clock = Stopwatch.StartNew();
    static float timeScale = 1f;

    static readonly HashSet<string> registered = new HashSet<string>();
    static void Reg(string name, Delegate d) { keep.Add(d); registered.Add(name); mono_add_internal_call(name, Marshal.GetFunctionPointerForDelegate(d)); }

    /// <summary>A MonoObject* handed to an internal call, as the managed object.</summary>
    static object Obj(IntPtr p)
    {
        if (p == IntPtr.Zero) return null;
        uint h = mono_gchandle_new(p, 0);
        try { return GCHandle.FromIntPtr((IntPtr)h).Target; } finally { mono_gchandle_free(h); }
    }

    // ---- delegate shapes (cdecl; bools as one byte, as Mono passes MonoBoolean) ----
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void V();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool B();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int I();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate uint U();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate long L();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate float F();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate double D();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr P();
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VF(float a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VI(int a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VU(uint a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VB([MarshalAs(UnmanagedType.U1)] bool a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPP(IntPtr a, IntPtr b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPPP(IntPtr a, IntPtr b, IntPtr c, IntPtr d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPB(IntPtr a, [MarshalAs(UnmanagedType.U1)] bool b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BPP(IntPtr a, IntPtr b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IP(IntPtr a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IPI(IntPtr a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IPII(IntPtr a, int b, int c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IPIP(IntPtr a, int b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate int IPPL(IntPtr a, IntPtr b, long c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PLII(long size, int align, int allocator, int skip);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PLI(long size, int align, int allocator);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPL(IntPtr a, IntPtr b, long c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPBL(IntPtr a, byte b, long c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPII(IntPtr a, IntPtr b, int c, int d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIPIII(IntPtr a, int b, IntPtr c, int d, int e, int f);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPPPPP(IntPtr a, IntPtr b, IntPtr c, IntPtr d, IntPtr e);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIIP(IntPtr a, int b, int c, IntPtr d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIPPP(IntPtr a, int b, IntPtr c, IntPtr d, IntPtr e);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BPIPP(IntPtr a, int b, IntPtr c, IntPtr d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIPI(IntPtr a, int b, IntPtr c, int d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPUU(IntPtr a, uint b, uint c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VIIPP(int a, int b, IntPtr c, IntPtr d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate ushort SPII(IntPtr a, int b, int c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPIUSII(IntPtr a, int b, ushort c, ushort d, int e);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPSUI(IntPtr a, ushort b, ushort c, int d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate uint US(ushort a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VUI(uint a, int b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPIP(IntPtr a, int b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate float FFF(float a, float b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate float FF(float a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VIIPI(int a, int b, IntPtr c, IntPtr d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPI(IntPtr a, IntPtr b, int c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPBP(IntPtr a, [MarshalAs(UnmanagedType.U1)] bool b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PPPP(IntPtr a, IntPtr b, IntPtr c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate IntPtr PI(int a);

    // ---- memory ----
    static IntPtr Alloc(long size, int align)
    {
        if (align < 16) align = 16;
        IntPtr raw = Marshal.AllocHGlobal(new IntPtr(size + align + IntPtr.Size));
        long p = ((long)raw + IntPtr.Size + align - 1) & ~(long)(align - 1);
        *(IntPtr*)(p - IntPtr.Size) = raw;
        return new IntPtr(p);
    }
    static void Free(IntPtr p) { if (p != IntPtr.Zero) Marshal.FreeHGlobal(*(IntPtr*)((long)p - IntPtr.Size)); }
    static void Set(byte* d, byte v, long n) { for (long i = 0; i < n; i++) d[i] = v; }

    static readonly Dictionary<Type, int> sizes = new Dictionary<Type, int>();
    static int SizeOfType(Type t)
    {
        if (sizes.TryGetValue(t, out int s)) return s;
        var dm = new DynamicMethod("sizeof", typeof(int), Type.EmptyTypes, typeof(FakeEngine).Module, true);
        var il = dm.GetILGenerator(); il.Emit(OpCodes.Sizeof, t); il.Emit(OpCodes.Ret);
        s = (int)dm.Invoke(null, null); sizes[t] = s; return s;
    }
    static bool IsManaged(Type t, int depth = 0)
    {
        if (t.IsPointer || t.IsPrimitive || t.IsEnum || t == typeof(IntPtr) || t == typeof(UIntPtr)) return false;
        if (!t.IsValueType) return true;
        if (depth > 20) return false;
        return t.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic).Any(f => IsManaged(f.FieldType, depth + 1));
    }
    static int TypeFlags(Type t)
    {
        int f = 0;
        if (t == null || IsManaged(t)) f |= 1;
        if (t != null && t.GetCustomAttributes(true).Any(a => a.GetType().Name == "NativeContainerAttribute")) f |= 2;
        return f;
    }

    // ---- safety handles: {IntPtr versionNode; int version; int staticSafetyId} ----
    struct Ash { public IntPtr node; public int version; public int staticId; }
    static void NewHandle(Ash* h) { var node = Alloc(16, 16); *(int*)node = 16; h->node = node; h->version = 16; h->staticId = 0; }
    static Ash tempHandle;
    static int nextSafetyId = 1;

    // ---- jobs ----
    sealed class JobInfo { public Delegate Fn; public Action<Delegate, IntPtr, IntPtr, int> Call; }
    static readonly List<JobInfo> jobs = new List<JobInfo> { null };
    static readonly Dictionary<Type, Action<Delegate, IntPtr, IntPtr, int>> callers = new Dictionary<Type, Action<Delegate, IntPtr, IntPtr, int>>();
    [StructLayout(LayoutKind.Sequential)] struct Ranges { public int BatchSize, NumJobs, TotalIterationCount; public IntPtr StartEndIndex; }
    [StructLayout(LayoutKind.Sequential)] struct Sched { public ulong depGroup; public int depVersion, depDebugVersion; public IntPtr depDebugInfo; public int ScheduleMode; public IntPtr ReflectionData, JobDataPtr; }

    /// <summary>A caller for an ExecuteJobFunction&lt;T&gt;: (ref T data, IntPtr additional, IntPtr patch, ref JobRanges, int index),
    /// with the job's address and the ranges' address passed as raw pointers.</summary>
    static Action<Delegate, IntPtr, IntPtr, int> CallerFor(Type delType)
    {
        if (callers.TryGetValue(delType, out var c)) return c;
        var invoke = delType.GetMethod("Invoke");
        var dm = new DynamicMethod("callJob", typeof(void), new[] { typeof(Delegate), typeof(IntPtr), typeof(IntPtr), typeof(int) }, typeof(FakeEngine).Module, true);
        var il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0); il.Emit(OpCodes.Castclass, delType);
        il.Emit(OpCodes.Ldarg_1);                               // T& data (the job struct where Schedule's caller holds it)
        il.Emit(OpCodes.Ldc_I4_0); il.Emit(OpCodes.Conv_I);     // additionalPtr
        il.Emit(OpCodes.Ldc_I4_0); il.Emit(OpCodes.Conv_I);     // bufferRangePatchData
        il.Emit(OpCodes.Ldarg_2);                               // ref JobRanges
        il.Emit(OpCodes.Ldarg_3);                               // jobIndex
        il.Emit(OpCodes.Callvirt, invoke); il.Emit(OpCodes.Ret);
        c = (Action<Delegate, IntPtr, IntPtr, int>)dm.CreateDelegate(typeof(Action<Delegate, IntPtr, IntPtr, int>));
        callers[delType] = c; return c;
    }
    static int executing;
    // the real layouts, read from Unity's own types once (IL field order is not a promise of offsets)
    static int offReflection = -1, offJobData, rangesSize, offTotal, offNumJobs;
    static void Layouts()
    {
        if (offReflection >= 0) return;
        var sp = Type.GetType("Unity.Jobs.LowLevel.Unsafe.JobsUtility+JobScheduleParameters, UnityEngine.CoreModule", true);
        offReflection = (int)Marshal.OffsetOf(sp, "ReflectionData"); offJobData = (int)Marshal.OffsetOf(sp, "JobDataPtr");
        var jr = Type.GetType("Unity.Jobs.LowLevel.Unsafe.JobRanges, UnityEngine.CoreModule", true);
        rangesSize = Marshal.SizeOf(jr); offTotal = (int)Marshal.OffsetOf(jr, "TotalIterationCount"); offNumJobs = (int)Marshal.OffsetOf(jr, "NumJobs");
        if (Trace) Console.WriteLine("  [job] layout reflection@" + offReflection + " jobdata@" + offJobData + " ranges " + rangesSize + " total@" + offTotal + " numjobs@" + offNumJobs);
    }
    static void RunJob(Sched* sp, int length)
    {
        Layouts();
        byte* p = (byte*)sp;
        int idx = (int)*(IntPtr*)(p + offReflection);
        IntPtr data = *(IntPtr*)(p + offJobData);
        if (Trace) Console.WriteLine("  [job] schedule reflection=" + idx + " data=" + data + " len=" + length);
        if (idx <= 0 || idx >= jobs.Count) throw new InvalidOperationException("otr engine: unknown job reflection data " + idx);
        var info = jobs[idx];
        byte* r = stackalloc byte[Math.Max(rangesSize, 64)];
        Set(r, 0, Math.Max(rangesSize, 64));
        *(int*)(r + offTotal) = length;
        executing++;
        try { info.Call(info.Fn, data, (IntPtr)r, 0); }
        catch (TargetInvocationException e) when (e.InnerException != null) { throw e.InnerException; }
        finally { executing--; }
    }

    // ---- burst ----
    static readonly Dictionary<(ulong, ulong), IntPtr> shared = new Dictionary<(ulong, ulong), IntPtr>();

    static readonly List<GCHandle> pinnedReturns = new List<GCHandle>();
    /// <summary>A managed object as the MonoObject* an internal call returns (a strong handle keeps it alive: a runner leaks).</summary>
    internal static IntPtr Ptr(object o)
    {
        if (o == null) return IntPtr.Zero;
        var g = GCHandle.Alloc(o);
        pinnedReturns.Add(g);
        return mono_gchandle_get_target((uint)(long)GCHandle.ToIntPtr(g));
    }
    /// <summary>Write a string into an out ManagedSpanWrapper (UTF-16, freed by BindingsAllocator.Free).</summary>
    static void OutString(IntPtr wrapper, string s)
    {
        s = s ?? "";
        IntPtr buf = Alloc(Math.Max(2, s.Length * 2), 16);
        fixed (char* c = s) Buffer.MemoryCopy(c, (void*)buf, s.Length * 2, s.Length * 2);
        *(IntPtr*)wrapper = buf;
        *(int*)((long)wrapper + IntPtr.Size) = s.Length;
    }
    static readonly Dictionary<string, int> propertyIds = new Dictionary<string, int>();
    static readonly List<string> propertyNames = new List<string> { "" };
    public static string TempRoot = "";

    static string Span(IntPtr wrapper)
    {
        if (wrapper == IntPtr.Zero) return "";
        IntPtr begin = *(IntPtr*)wrapper; int len = *(int*)((long)wrapper + IntPtr.Size);
        return begin == IntPtr.Zero ? "" : new string((char*)begin, 0, len);
    }


    // Mathf.PerlinNoise: Ken Perlin's improved noise at z = 0 mapped by (n + 0.69) / 1.483, which gives Unity's well-known
    // 0.4652731 on every lattice point; a reimplementation, not a copy: compare Perlin-dependent sizes with a tolerance
    static readonly int[] perm = new int[512];
    static float Fade(float t) => t * t * t * (t * (t * 6 - 15) + 10);
    static float Lerp(float t, float a, float b) => a + t * (b - a);
    static float Grad(int hash, float x, float y) { int h = hash & 15; float u = h < 8 ? x : y; float v = h < 4 ? y : (h == 12 || h == 14 ? x : 0); return ((h & 1) == 0 ? u : -u) + ((h & 2) == 0 ? v : -v); }
    static float Perlin(float x, float y)
    {
        int fx = (int)Math.Floor(x), fy = (int)Math.Floor(y);
        int X = fx & 255, Y = fy & 255;
        x -= fx; y -= fy;
        float u = Fade(x), v = Fade(y);
        int A = perm[X] + Y, AA = perm[A], AB = perm[A + 1], B = perm[X + 1] + Y, BA = perm[B], BB = perm[B + 1];
        float n = Lerp(v, Lerp(u, Grad(perm[AA], x, y), Grad(perm[BA], x - 1, y)), Lerp(u, Grad(perm[AB], x, y - 1), Grad(perm[BB], x - 1, y - 1)));
        return (n + 0.69f) / 1.483f;
    }
    static readonly int[] permBase = { 151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,190,6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,65,25,63,161,1,216,80,73,209,76,132,187,208,89,18,169,200,196,135,130,116,188,159,86,164,100,109,198,173,186,3,64,52,217,226,250,124,123,5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,213,119,248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241,81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,157,184,84,204,176,115,121,50,45,127,4,150,254,138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180 };
    public static void Install(string projectDir)
    {
        for (int i = 0; i < 512; i++) perm[i] = permBase[i & 255];
        Reg("UnityEngine.Mathf::PerlinNoise", new FFF((x, y) => Perlin(x, y)));
        Reg("UnityEngine.Mathf::PerlinNoise1D", new FF(x => Perlin(x, 0f)));
        Reg("UnityEngine.Mathf::GammaToLinearSpace", new FF(c => c <= 0.04045f ? c / 12.92f : (float)Math.Pow((c + 0.055f) / 1.055f, 2.4f)));
        Reg("UnityEngine.Mathf::LinearToGammaSpace", new FF(c => c <= 0.0031308f ? c * 12.92f : 1.055f * (float)Math.Pow(c, 1f / 2.4f) - 0.055f));
        ProjectDir = projectDir;
        const string UU = "Unity.Collections.LowLevel.Unsafe.UnsafeUtility::";
        Reg(UU + "MallocTracked", new PLII((s, a, al, k) => Alloc(s, a)));
        Reg(UU + "Malloc", new PLI((s, a, al) => Alloc(s, a)));
        Reg(UU + "FreeTracked", new VPI((p, al) => Free(p)));
        Reg(UU + "Free", new VPI((p, al) => Free(p)));
        Reg(UU + "MemCpy", new VPPL((d, s, n) => Buffer.MemoryCopy((void*)s, (void*)d, n, n)));
        Reg(UU + "MemMove", new VPPL((d, s, n) => Buffer.MemoryCopy((void*)s, (void*)d, n, n)));
        Reg(UU + "MemSet", new VPBL((d, v, n) => Set((byte*)d, v, n)));
        Reg(UU + "MemClear", new VPPL((d, n, _) => Set((byte*)d, 0, (long)n)));
        Reg(UU + "MemCmp", new IPPL((a, b, n) => { byte* x = (byte*)a, y = (byte*)b; for (long i = 0; i < n; i++) if (x[i] != y[i]) return x[i] < y[i] ? -1 : 1; return 0; }));
        Reg(UU + "MemCpyReplicate", new VPPII((d, s, size, count) => { for (int i = 0; i < count; i++) Buffer.MemoryCopy((void*)s, (byte*)d + (long)i * size, size, size); }));
        Reg(UU + "MemCpyStride", new VPIPIII((d, ds, s, ss, es, count) => { for (int i = 0; i < count; i++) Buffer.MemoryCopy((byte*)s + (long)i * ss, (byte*)d + (long)i * ds, es, es); }));
        Reg(UU + "MemSwap", new VPPL((a, b, n) => { byte* x = (byte*)a, y = (byte*)b; for (long i = 0; i < n; i++) { byte t = x[i]; x[i] = y[i]; y[i] = t; } }));
        Reg(UU + "GetScriptingTypeFlags", new IP(t => TypeFlags(Obj(t) as Type)));
        Reg(UU + "SizeOf", new IP(t => SizeOfType((Type)Obj(t))));
        Reg(UU + "IsBlittable", new BP(t => { var ty = (Type)Obj(t); return ty.IsValueType && !IsManaged(ty) && !ty.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic).Any(f => f.FieldType == typeof(bool) || f.FieldType == typeof(char)); }));
        Reg(UU + "IsUnmanaged", new BP(t => !IsManaged((Type)Obj(t))));
        Reg(UU + "IsValidNativeContainerElementType", new BP(t => !IsManaged((Type)Obj(t))));
        Reg(UU + "GetFieldOffsetInStruct", new IP(f => (int)Marshal.OffsetOf(((FieldInfo)Obj(f)).DeclaringType, ((FieldInfo)Obj(f)).Name)));
        Reg(UU + "LeakRecord", new IPII((h, c, k) => 0));
        Reg(UU + "LeakErase", new IPI((h, c) => 0));
        Reg(UU + "CheckForLeaks", new I(() => 0));
        Reg(UU + "ForgiveLeaks", new I(() => 0));
        Reg(UU + "GetLeakDetectionMode", new I(() => 0));
        Reg(UU + "SetLeakDetectionMode", new VI(m => { }));
        Reg(UU + "LogError_Injected", new VPPI((m, f, l) => Log(0, Span(m))));

        const string AS = "Unity.Collections.LowLevel.Unsafe.AtomicSafetyHandle::";
        fixed (Ash* t = &tempHandle) NewHandle(t);
        Reg(AS + "Create_Injected", new VP(h => NewHandle((Ash*)h)));
        Reg(AS + "GetTempMemoryHandle_Injected", new VP(h => *(Ash*)h = tempHandle));
        Reg(AS + "GetTempUnsafePtrSliceHandle_Injected", new VP(h => *(Ash*)h = tempHandle));
        Reg(AS + "IsTempMemoryHandle_Injected", new BP(h => ((Ash*)h)->node == tempHandle.node));
        Reg(AS + "Release_Injected", new VP(h => { var a = (Ash*)h; if (a->node != IntPtr.Zero && a->node != tempHandle.node) *(int*)a->node += 16; }));
        foreach (var n in new[] { "CheckReadAndThrowNoEarlyOut_Injected", "CheckWriteAndThrowNoEarlyOut_Injected", "CheckDeallocateAndThrow_Injected", "CheckGetSecondaryDataPointerAndThrow_Injected", "PrepareUndisposable", "UseSecondaryVersion" })
            Reg(AS + n, new VP(h => { }));
        foreach (var n in new[] { "SetAllowSecondaryVersionWriting_Injected", "SetBumpSecondaryVersionOnScheduleWrite_Injected", "SetAllowReadOrWriteAccess_Injected", "SetNestedContainer_Injected", "SetExclusiveWeak" })
            Reg(AS + n, new VPB((h, b) => { }));
        Reg(AS + "GetAllowReadOrWriteAccess_Injected", new BP(h => true));
        Reg(AS + "GetNestedContainer_Injected", new BP(h => false));
        Reg(AS + "GetExclusiveWeak", new BP(h => false));
        Reg(AS + "IsDefaultValue", new BP(h => ((Ash*)h)->node == IntPtr.Zero && ((Ash*)h)->version == 0));
        foreach (var n in new[] { "EnforceAllBufferJobsHaveCompleted_Injected", "EnforceAllBufferJobsHaveCompletedAndRelease_Injected", "EnforceAllBufferJobsHaveCompletedAndDisableReadWrite_Injected" })
            Reg(AS + n, new IP(h => { if (n.Contains("Release")) { var a = (Ash*)h; if (a->node != IntPtr.Zero && a->node != tempHandle.node) *(int*)a->node += 16; } return 0; }));
        Reg(AS + "NewStaticSafetyId", new IPI((b, c) => nextSafetyId++));
        Reg(AS + "SetCustomErrorMessage", new VIIPI((id, t, m, c) => { }));
        Reg(AS + "GetReaderArray_Injected", new IPIP((h, m, o) => 0));
        Reg(AS + "GetWriter_Injected", new VPP((h, r) => Set((byte*)r, 0, 24)));

        const string JU = "Unity.Jobs.LowLevel.Unsafe.JobsUtility::";
        Reg(JU + "CreateJobReflectionData", new PPPPPP((w, u, f0, f1, f2) =>
        {
            var fn = (Delegate)Obj(f0);
            jobs.Add(new JobInfo { Fn = fn, Call = CallerFor(fn.GetType()) });
            if (Trace) Console.WriteLine("  [job] reflection " + (jobs.Count - 1) + " for " + fn.GetType());
            return (IntPtr)(jobs.Count - 1);
        }));
        Reg(JU + "Schedule_Injected", new VPP((p, ret) => { RunJob((Sched*)p, 1); Set((byte*)ret, 0, 24); }));
        Reg(JU + "ScheduleParallelFor_Injected", new VPIIP((p, len, batch, ret) => { RunJob((Sched*)p, len); Set((byte*)ret, 0, 24); }));
        Reg(JU + "ScheduleParallelForDeferArraySize_Injected", new VPIPPP((p, batch, list, len, ret) =>
        {
            // listData: the list whose Length is the array size (UnsafeList: void* Ptr; int m_length): read through lengthPtr when given
            int n = len != IntPtr.Zero ? *(int*)len : (list != IntPtr.Zero ? *(int*)((long)list + IntPtr.Size) : 0);
            RunJob((Sched*)p, n); Set((byte*)ret, 0, 24);
        }));
        Reg(JU + "GetWorkStealingRange", new BPIPP((r, job, b, e) =>
        {
            Layouts();
            int* handed = (int*)((byte*)r + offNumJobs);
            if (*handed != 0) return false;   // the whole range was handed out
            *handed = -1; *(int*)b = 0; *(int*)e = *(int*)((byte*)r + offTotal); return true;
        }));
        Reg(JU + "PatchBufferMinMaxRanges", new VPPII((patch, data, start, size) => { }));
        Reg(JU + "get_IsExecutingJob", new B(() => executing > 0));
        Reg(JU + "get_JobDebuggerEnabled", new B(() => false));
        Reg(JU + "set_JobDebuggerEnabled", new VB(v => { }));
        Reg(JU + "get_JobCompilerEnabled", new B(() => false));
        Reg(JU + "set_JobCompilerEnabled", new VB(v => { }));
        Reg(JU + "GetJobQueueWorkerThreadCount", new I(() => 0));
        Reg(JU + "SetJobQueueMaximumActiveThreadCount", new VI(c => { }));
        Reg(JU + "get_JobWorkerMaximumCount", new I(() => 1));
        Reg(JU + "ResetJobWorkerCount", new V(() => { }));
        Reg(JU + "get_ThreadIndex", new I(() => 0));
        Reg(JU + "get_ThreadIndexCount", new I(() => 1));
        Reg(JU + "GetJobBatchingEnabled", new B(() => false));
        Reg(JU + "ClearSystemIds", new V(() => { }));

        const string JH = "Unity.Jobs.JobHandle::";
        Reg(JH + "ScheduleBatchedJobs", new V(() => { }));
        Reg(JH + "ScheduleBatchedJobsAndComplete", new VP(h => { }));
        Reg(JH + "ScheduleBatchedJobsAndIsCompleted", new BP(h => true));
        Reg(JH + "ScheduleBatchedJobsAndCompleteAll", new VPI((h, c) => { }));
        Reg(JH + "CombineDependenciesInternal2_Injected", new VPPP((a, b, r) => Set((byte*)r, 0, 24)));
        Reg(JH + "CombineDependenciesInternal3_Injected", new VPPPP((a, b, c, r) => Set((byte*)r, 0, 24)));
        Reg(JH + "CombineDependenciesInternalPtr_Injected", new VPIP((a, c, r) => Set((byte*)r, 0, 24)));
        Reg(JH + "CheckFenceIsDependencyOrDidSyncFence_Injected", new BPP((a, b) => true));

        const string BC = "Unity.Burst.LowLevel.BurstCompilerService::";
        Reg(BC + "GetOrCreateSharedMemory", new PPUU((key, size, align) =>
        {
            var k = (*(ulong*)key, *(ulong*)((long)key + 8));
            if (!shared.TryGetValue(k, out var mem)) { mem = Alloc(size, (int)Math.Max(align, 16)); Set((byte*)mem, 0, size); shared[k] = mem; }
            return mem;
        }));
        Reg(BC + "get_IsInitialized", new B(() => false));
        Reg(BC + "GetCurrentExecutionMode", new U(() => 0));
        Reg(BC + "SetCurrentExecutionMode", new VU(m => { }));

        const string PU = "Unity.Profiling.LowLevel.Unsafe.ProfilerUnsafeUtility::";
        Reg(PU + "CreateMarker_Injected", new PPSUI((n, c, f, m) => (IntPtr)1));
        Reg(PU + "GetMarker_Injected", new P(() => (IntPtr)1));
        Reg(PU + "CreateMarker__Unmanaged", new PPIUSII((n, l, c, f, m) => (IntPtr)1));
        Reg(PU + "CreateMarker_Unsafe", new PPIUSII((n, l, c, f, m) => (IntPtr)1));
        Reg(PU + "CreateCategory_Injected", new SPII((n, c, _) => 0));
        Reg(PU + "CreateCategory__Unmanaged", new SPII((n, l, c) => 0));
        Reg(PU + "CreateCategory_Unsafe", new SPII((n, l, c) => 0));
        Reg(PU + "BeginSample", new VP(m => { }));
        Reg(PU + "EndSample", new VP(m => { }));
        Reg(PU + "BeginSampleWithMetadata", new VPIP((m, c, d) => { }));
        Reg(PU + "SingleSampleWithMetadata", new VPIP((m, c, d) => { }));
        Reg(PU + "get_Timestamp", new L(() => clock.ElapsedTicks));

        Reg("UnityEngine.DebugLogHandler::Internal_Log_Injected", new VIIPP((level, opt, msg, obj) => Log(level, Span(msg))));
        Reg("UnityEngine.DebugLogHandler::Internal_LogException_Injected", new VPP((ex, obj) => Log(4, (Obj(ex) as Exception)?.ToString() ?? "exception")));

        Reg("UnityEngine.Application::get_isPlaying", new B(() => false));
        FakeObjects.Install(Reg, Obj, Span, OutString, Log, () => false);
        FakeMath.Install(Reg);
        TempRoot = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tw-otr-" + Process.GetCurrentProcess().Id);
        System.IO.Directory.CreateDirectory(TempRoot);
        string assets = System.IO.Path.Combine(projectDir, "Assets").Replace('\\', '/');
        string temp = TempRoot.Replace('\\', '/');
        Reg("UnityEngine.Application::get_dataPath_Injected", new VP(r => OutString(r, assets)));
        Reg("UnityEngine.Application::get_streamingAssetsPath_Injected", new VP(r => OutString(r, assets + "/StreamingAssets")));
        // never the player's folders: a test that forgets to substitute a profile writes into a throwaway folder
        Reg("UnityEngine.Application::get_persistentDataPath_Injected", new VP(r => OutString(r, temp + "/persistent")));
        Reg("UnityEngine.Application::get_temporaryCachePath_Injected", new VP(r => OutString(r, temp + "/cache")));
        Reg("UnityEngine.Bindings.BindingsAllocator::Malloc", new PI(n => Alloc(n, 16)));
        Reg("UnityEngine.Bindings.BindingsAllocator::Free", new VP(q => Free(q)));
        Reg("UnityEngine.Bindings.BindingsAllocator::FreeNativeOwnedMemory", new VP(q => Free(q)));
        Reg("UnityEngine.JsonUtility::ToJsonInternal_Injected", new VPBP((o, pretty, r) => OutString(r, FakeJson.ToJson(Obj(o), pretty))));
        Reg("UnityEngine.JsonUtility::FromJsonInternal_Injected", new PPPP((json, target, type) => Ptr(FakeJson.FromJson(Span(json), Obj(target), (Type)Obj(type)))));
        Reg("UnityEngine.PropertyNameUtils::PropertyNameFromString_Injected", new VPP((n, r) =>
        {
            string s = Span(n);
            if (!propertyIds.TryGetValue(s, out int id)) { id = propertyNames.Count; propertyNames.Add(s); propertyIds[s] = id; }
            *(int*)r = s.Length == 0 ? 0 : id;
        }));
        Reg("UnityEngine.PropertyNameUtils::StringFromPropertyName_Injected", new VPP((pn, r) => { int id = *(int*)pn; OutString(r, id > 0 && id < propertyNames.Count ? propertyNames[id] : ""); }));
        Reg("UnityEngine.PropertyNameUtils::ConflictCountForID", new IP(id => 0));
        Reg("UnityEngine.Application::get_isBatchMode", new B(() => true));
        const string TM = "UnityEngine.Time::";
        Reg(TM + "get_time", new F(() => (float)clock.Elapsed.TotalSeconds));
        Reg(TM + "get_timeAsDouble", new D(() => clock.Elapsed.TotalSeconds));
        Reg(TM + "get_unscaledTime", new F(() => (float)clock.Elapsed.TotalSeconds));
        Reg(TM + "get_realtimeSinceStartup", new F(() => (float)clock.Elapsed.TotalSeconds));
        Reg(TM + "get_realtimeSinceStartupAsDouble", new D(() => clock.Elapsed.TotalSeconds));
        Reg(TM + "get_timeSinceLevelLoad", new F(() => (float)clock.Elapsed.TotalSeconds));
        Reg(TM + "get_deltaTime", new F(() => 0.02f));
        Reg(TM + "get_unscaledDeltaTime", new F(() => 0.02f));
        Reg(TM + "get_smoothDeltaTime", new F(() => 0.02f));
        Reg(TM + "get_fixedDeltaTime", new F(() => 0.02f));
        Reg(TM + "set_fixedDeltaTime", new VF(v => { }));
        Reg(TM + "get_timeScale", new F(() => timeScale));
        Reg(TM + "set_timeScale", new VF(v => timeScale = v));
        Reg(TM + "get_frameCount", new I(() => 0));
        Reg(TM + "get_renderedFrameCount", new I(() => 0));
        // last: its inert stubs fill only what nothing above registered
        FakeRender.Install(Reg, registered, Obj, Span, FakeObjects.RegisterObject, FakeObjects.RegisterObject, Ptr, projectDir);
    }

    static void Log(int level, string msg)
    {
        // LogType: Error 0, Assert 1, Warning 2, Log 3, Exception 4
        if (level == 0 || level == 1 || level == 4) Logged.Add((level == 4 ? "Exception: " : level == 1 ? "Assert: " : "Error: ") + msg);
        if (Verbose) Console.WriteLine("  [log " + level + "] " + msg);
    }
}

/// <summary>The engine's helpers for the other stand-in files.</summary>
static class FakeEngineAccess
{
    static readonly System.Collections.Generic.List<GCHandle> kept = new System.Collections.Generic.List<GCHandle>();
    public static IntPtr Ptr(object o) => FakeEngine.Ptr(o);
    public static void Keep(GCHandle h) => kept.Add(h);
}
