// Phase: tooling (2026-09-25) — a static written during a match must be put back when the match ends, or be here
// with the reason it need not be.
//
// The project does not reload the domain when the editor leaves Play, so every static a match wrote survives into
// the next thing the editor runs, tests included. That is how CameraShake's look point failed BlastReactionTests in
// a live editor while the batch gate passed. By 2026-09-25 presentation and UI held about 85 mutable statics across
// 27 types and nothing listed them; this test is the list. A type that gains a mutable static fails here until it
// either registers a reset with SceneStatics.Register (run by SceneStatics.ResetSession when Play ends) or is added
// to Explained with the reason leaving it is safe.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public sealed class StaticLifecycleTests
    {
        /// <summary>Types allowed to keep mutable statics across a Play session, and why.</summary>
        static readonly Dictionary<string, string> Explained = new Dictionary<string, string>
        {
            ["Atmosphere"] = "weather, mood and the storm flash: the live Atmosphere rewrites them every frame; PinnedClock is a capture switch the caller restores",
            ["AudioLevels"] = "player settings, applied from settings.json by SettingsApplier",
            ["BattleHud"] = "legacy IMGUI HUD layout (MinimapRect, a warning latch); retired in the audit backlog",
            ["BattlefieldProps"] = "EditorCamera: the prop editor's camera, set and cleared by EnvPropEditor",
            ["BootstrapLoader"] = "Override: which scene Bootstrap loads, set by tools before they load it",
            ["CampaignSession"] = "the campaign mission in flight, carried across the scene load like MatchLaunch.Current; MatchLaunch.QuitToMenu clears it",
            ["DebrisRenderer"] = "Gore is a setting and ZoomShare the camera's share of every burst, set by CombatFx each frame; Biome and LavaLevel are set by each biome; Instance is the live renderer",
            ["EventPump"] = "ProfileSubscribers: a profiling switch",
            ["Flamethrower"] = "Active: the live instance, replaced when the next CombatFx starts",
            ["FrameBudget"] = "frame-stamped counters that roll over by themselves",
            ["GreyboxTerrainView"] = "code-made textures cached for the process (mudDetail, ripples)",
            ["HeavyWork"] = "frame-stamped claim that expires by itself",
            ["HudBootstrap"] = "Disabled: a test switch the tests set and restore",
            ["HudBridge"] = "PointerOverUi is cleared by SceneStatics.Reset; WheelClaimed is re-wired by the HUD; UseToolkitHud lives in PlayerPrefs",
            ["HudHotkeys"] = "LegacyOverlayActive: follows the HUD flag",
            ["InputFocus"] = "cleared by SceneStatics.Reset on every scene load",
            ["KeyMap"] = "Current: the player's bindings from settings.json",
            ["MatchLaunch"] = "Current/Running: the mission request carried across a scene load, by design",
            ["ProfileStore"] = "Current: the loaded profile.json, kept across scene loads like SettingsStore",
            ["RenderGround"] = "the drawn ground, replaced when the next terrain view builds; tests pass their maps explicitly",
            ["SceneMood"] = "Night: set by Atmosphere at the start of each scene",
            ["SceneTints"] = "the biome's tints and their epoch: set by Atmosphere at the start of each scene",
            ["SettingsStore"] = "Current: the loaded settings.json",
            ["ShellBoot"] = "Disabled: a test switch; the shell root outlives scene loads by design",
            ["ShellRouter"] = "Instance: the one shell router, which outlives scene loads by design",
            ["SimHost"] = "BombardmentOverride is cleared by SceneStatics.Reset; CanaryOverride and StressOverride are set and restored by tests and tools",
        };

        /// <summary>Types SceneStatics.ResetSession resets directly rather than through a registration.</summary>
        static readonly HashSet<string> ResetDirectly = new HashSet<string> { "SceneHooks" };

        static IEnumerable<Type> Holders()
        {
            var assemblies = AppDomain.CurrentDomain.GetAssemblies()
                .Where(a => { var n = a.GetName().Name; return n.StartsWith("TW.Presentation") || n == "TW.UI"; });
            foreach (var asm in assemblies)
                foreach (var t in asm.GetTypes())
                {
                    if (t.Name.Contains('<') || t.IsDefined(typeof(CompilerGeneratedAttribute), false)) continue;
                    bool mutable = t.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
                        .Any(f => !f.IsLiteral && !f.IsInitOnly);
                    if (mutable) yield return t;
                }
        }

        [Test]
        public void Every_Mutable_Static_Is_Reset_When_Play_Ends_Or_Explained()
        {
            var holders = Holders().ToList();
            var unexplained = new List<string>();
            foreach (var t in holders)
            {
                if (Explained.ContainsKey(t.Name) || ResetDirectly.Contains(t.Name)) continue;
                RuntimeHelpers.RunClassConstructor(t.TypeHandle);   // registration happens in the static constructor
                if (!SceneStatics.Registered.Contains(t.Name)) unexplained.Add(t.FullName);
            }
            Assert.That(unexplained, Is.Empty,
                "these types hold mutable statics that nothing puts back when Play ends. Register a reset with " +
                "SceneStatics.Register(nameof(Type), Reset) from a static constructor, or add the type to Explained in " +
                "this test with the reason it is safe: " + string.Join(", ", unexplained));

            var names = new HashSet<string>(holders.Select(t => t.Name));
            var stale = Explained.Keys.Where(k => !names.Contains(k)).ToList();
            Assert.That(stale, Is.Empty, "no longer hold a mutable static; remove them from Explained: " + string.Join(", ", stale));
        }

        [Test]
        public void The_Camera_Forgets_The_Match_When_The_Session_Ends()
        {
            SceneStatics.ResetSession();          // whatever an earlier test left, start from an untouched camera
            CameraShake.Add(Vector3.zero, 8f);   // under the middle of that picture: always felt
            Assert.That(CameraShake.Pending, Is.GreaterThan(0), "a burst in the middle of the picture queues a kick");
            SceneStatics.ResetSession();
            Assert.That(CameraShake.Pending, Is.EqualTo(0), "ResetSession clears the kicks still on their way");
            Assert.That(CameraShake.DistanceToLook(new Vector3(3f, 0f, 4f)), Is.EqualTo(5f).Within(1e-4f),
                "and the look point is back at the origin");
        }

        [Test]
        public void Scene_Hooks_Are_Empty_When_The_Session_Ends()
        {
            SceneHooks.CloseUp = 1f;
            SceneHooks.TanksDrawn = true;
            SceneHooks.Sparks = (at, n) => { };
            SceneHooks.SmokeSources.Add(Vector3.one);
            SceneStatics.ResetSession();
            Assert.That(SceneHooks.CloseUp, Is.EqualTo(0f));
            Assert.That(SceneHooks.TanksDrawn, Is.False);
            Assert.That(SceneHooks.Sparks, Is.Null);
            Assert.That(SceneHooks.SmokeSources, Is.Empty);
        }
    }
}
