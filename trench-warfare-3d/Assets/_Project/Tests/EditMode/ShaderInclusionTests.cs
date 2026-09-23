// Phase: tooling (perf pass, 2026-09-23) — every shader the game finds by name at runtime is in a player build.
// Shader.Find only finds a shader a build contains, and a build contains a shader only if a material it ships uses it
// or GraphicsSettings > Always Included Shaders lists it. Nothing checked this, and the first Windows player built
// (2026-09-23) was missing TW/Debris, TW/Sea, TW/Tank and TW/TankDisc (no debris, no sea, box tanks) and URP Unlit/Lit
// (no tracers, sparks, smoke or bodies): Shader.Find returned null, new Material(null) threw, and CombatFx and Ocean
// threw again every frame, 8,500 exceptions in a 22 s benchmark and 256 KB of garbage a frame. The editor finds every
// shader in the project, so no editor test, capture or Play session could see any of it.
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Tests
{
    public class ShaderInclusionTests
    {
        /// <summary>Fallbacks for the built-in pipeline, which this URP game never runs; not required in a build.</summary>
        static readonly HashSet<string> BuiltInFallbacks = new HashSet<string> { "Standard", "Unlit/Color" };

        [Test]
        public void EveryShaderTheGameFindsByNameIsInTheBuild()
        {
            var wanted = new SortedSet<string>();
            foreach (var file in Directory.GetFiles("Assets/_Project", "*.cs", SearchOption.AllDirectories))
            {
                string f = file.Replace('\\', '/');
                if (f.Contains("/Editor/") || f.Contains("/Tests/")) continue;   // editor tooling runs in the editor, where every shader is found
                foreach (Match m in Regex.Matches(File.ReadAllText(file), "Shader\\.Find\\(\"([^\"]+)\"\\)")) wanted.Add(m.Groups[1].Value);
            }
            Assert.That(wanted.Count, Is.GreaterThan(10), "the source scan found almost no Shader.Find calls: the scan is broken, not the build");

            var inBuild = new HashSet<string>();
            var gs = AssetDatabase.LoadAssetAtPath<GraphicsSettings>("ProjectSettings/GraphicsSettings.asset");
            var always = new SerializedObject(gs).FindProperty("m_AlwaysIncludedShaders");
            for (int i = 0; i < always.arraySize; i++)
                if (always.GetArrayElementAtIndex(i).objectReferenceValue is Shader s) inBuild.Add(s.name);
            // a material in a Resources folder ships with every build, and so does its shader
            foreach (var guid in AssetDatabase.FindAssets("t:Material", new[] { "Assets" }))
            {
                string p = AssetDatabase.GUIDToAssetPath(guid);
                if (!p.Contains("/Resources/")) continue;
                var mat = AssetDatabase.LoadAssetAtPath<Material>(p);
                if (mat != null && mat.shader != null) inBuild.Add(mat.shader.name);
            }

            var missing = new List<string>();
            foreach (var name in wanted) if (!inBuild.Contains(name) && !BuiltInFallbacks.Contains(name)) missing.Add(name);
            Assert.That(missing, Is.Empty,
                "found by name at runtime but not in a player build (Shader.Find returns null there): add each to GraphicsSettings " +
                "> Always Included Shaders, or keep it with a material under a Resources folder (Resources/ShaderKeep for URP's own)");
        }

        /// <summary>The URP shaders are kept by materials set up the way the code sets up its own, so the variants the code
        /// uses (instancing, the transparent surface keyword) are the ones that ship.</summary>
        [Test]
        public void TheUrpKeepersCarryTheVariantsTheCodeUses()
        {
            var unlitT = Resources.Load<Material>("ShaderKeep/KeepUnlitTransparent");
            var unlitO = Resources.Load<Material>("ShaderKeep/KeepUnlitOpaque");
            var litO = Resources.Load<Material>("ShaderKeep/KeepLitOpaque");
            Assert.That(unlitT != null && unlitO != null && litO != null, "a ShaderKeep material is missing");
            Assert.That(unlitT.enableInstancing && unlitO.enableInstancing && litO.enableInstancing, "the code draws these instanced");
            Assert.That(unlitT.IsKeywordEnabled("_SURFACE_TYPE_TRANSPARENT"), "CombatFx's glow, smoke and gas are transparent URP Unlit");
        }
    }
}
