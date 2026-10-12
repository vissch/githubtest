// Phase: P0 — guards the Burst options on every [BurstCompile] method in the sim.
// Burst merges a METHOD attribute with the ASSEMBLY attribute only: the options on the enclosing class are
// not read. A bare [BurstCompile] on a method therefore runs Mono until the async compile lands and then
// switches mid-run, which is the cold-cache false desync docs/03-determinism-rules.md logged on 2026-09-20.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using Unity.Burst;

namespace TW.Tests
{
    public class BurstOptionsTests
    {
        // The sim assemblies the test asmdef references. Every one must be loaded, so nothing is skipped silently.
        static readonly string[] SimAssemblies =
        {
            "TW.Sim.Core", "TW.Sim.Terrain", "TW.Sim.Nav", "TW.Sim.Combat",
            "TW.Sim.Match", "TW.Sim.Units", "TW.Net", "TW.Data",
        };

        const BindingFlags All = BindingFlags.Public | BindingFlags.NonPublic
                               | BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly;

        // [S1] [N5] Every method-level [BurstCompile] in the sim carries the full options, so no hashed
        // arithmetic ever runs through Mono first and through Burst later in the same run.
        [Test]
        public void EveryBurstMethod_CompilesSynchronouslyAndStrict()
        {
            var found = new List<Assembly>();
            var missing = new List<string>();
            foreach (var name in SimAssemblies)
            {
                var asm = Array.Find(AppDomain.CurrentDomain.GetAssemblies(), a => a.GetName().Name == name);
                if (asm == null) missing.Add(name); else found.Add(asm);
            }
            Assert.IsEmpty(missing, "[S1] sim assemblies not loaded, so they went unchecked: " + string.Join(", ", missing));

            var offenders = new List<string>();
            int checked_ = 0;
            foreach (var asm in found)
            {
                foreach (var type in asm.GetTypes())
                {
                    foreach (var m in type.GetMethods(All))
                    {
                        // Only the method's own attribute: a job struct's type-level one is read by Burst and is already full.
                        var attr = (BurstCompileAttribute)Attribute.GetCustomAttribute(m, typeof(BurstCompileAttribute), false);
                        if (attr == null) continue;
                        checked_++;
                        if (!attr.CompileSynchronously || attr.FloatMode != FloatMode.Strict)
                            offenders.Add($"{type.FullName}.{m.Name} (CompileSynchronously={attr.CompileSynchronously}, FloatMode={attr.FloatMode})");
                    }
                }
            }

            Assert.Greater(checked_, 0, "[S1] no [BurstCompile] method was found at all, so the test proves nothing");
            Assert.IsEmpty(offenders,
                "[S1] [N5] these sim methods carry a [BurstCompile] without the full options, so they run Mono until "
                + "the async compile lands: " + string.Join("; ", offenders));
        }
    }
}
