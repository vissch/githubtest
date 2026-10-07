// Phase: tooling (2026-09-25; keyed by field 2026-10-07) — a static written during a match must be put back when the
// match ends, or be here with the reason it need not be.
//
// The project does not reload the domain when the editor leaves Play, so every static a match wrote survives into
// the next thing the editor runs, tests included. That is how CameraShake's look point failed BlastReactionTests in
// a live editor while the batch gate passed. By 2026-09-25 presentation and UI held about 85 mutable statics across
// 27 types and nothing listed them; this test is the list. A static that is added fails here until it either
// registers a reset with SceneStatics.Register (run by SceneStatics.ResetSession when Play ends) or is named here
// with the reason leaving it is safe.
//
// [TS11] The guard used to work per TYPE and by simple name: one registration, or one allow-list entry, covered
// every present and future static on that type; it knew only the session registry, not the per-scene one; and a
// readonly array was not "mutable", though only the reference is readonly, not the rows. Three real bugs walked
// through those holes (MetaServices.HomeFront/Map registered for the session but needing the scene load — P1a;
// Clips.Table rewritten by the bake; SceneMood.Night allowed on a prose claim). So: every key is "Type.Field", a
// readonly field holding a container counts, and a static holding a scene's own objects must be forgotten on the
// scene load, not only when Play ends.
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
        /// <summary>Statics allowed to live across a Play session, keyed "Type.Field", and why.</summary>
        static readonly Dictionary<string, string> Explained = new Dictionary<string, string>
        {
            ["Atmosphere.Current"] = "the mood the live Atmosphere writes every frame",
            ["Atmosphere.Profile"] = "the biome profile the live Atmosphere writes every frame",
            ["Atmosphere.RainNow"] = "written every frame by the live Atmosphere",
            ["Atmosphere.WindNow"] = "written every frame by the live Atmosphere",
            ["Atmosphere.StormFlash"] = "written every frame by the live Atmosphere",
            ["Atmosphere.StormLightFrom"] = "written every frame by the live Atmosphere",
            ["Atmosphere.PinnedClock"] = "a capture switch: the caller that pins the clock restores it",
            ["AllocProbe.recorder"] = "a cached profiler Recorder, re-made when Unity has dropped it",
            ["AllocProbe.busy"] = "a re-entry guard for one measurement at a time; the measurement clears it",
            ["AudioLevels.Ambience"] = "a player setting, applied from settings.json by SettingsApplier",
            ["AudioLevels.Sfx"] = "a player setting, applied from settings.json by SettingsApplier",
            ["AudioLevels.Music"] = "a player setting, applied from settings.json by SettingsApplier",
            ["BattleHud.MinimapRect"] = "legacy IMGUI HUD layout, laid out again every frame it draws; retired in the audit backlog",
            ["BattleHud.barWarned"] = "a warn-once latch for the legacy IMGUI HUD; retired in the audit backlog",
            ["BattlefieldProps.EditorCamera"] = "the prop editor's camera, set and cleared by EnvPropEditor, not by a match",
            ["BootstrapLoader.Override"] = "which scene Bootstrap loads, set by tools before they load it",
            ["CampaignSession.NodeId"] = "the campaign mission in flight, carried across the scene load; MatchLaunch.QuitToMenu clears it",
            ["CampaignSession.MissionIndex"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.Faction"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.Awarded"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.LastAward"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.AwardedFor"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.ResumeMap"] = "part of the mission in flight, carried across the scene load by design",
            ["EventPump.ProfileSubscribers"] = "a profiling switch, off unless a profiling run sets it",
            ["Flamethrower.Active"] = "the live instance, replaced when the next CombatFx starts",
            ["FrameBudget.frame"] = "the frame the counters below belong to: they roll over by themselves",
            ["HeavyWork.claimedFrame"] = "a frame-stamped claim that expires by itself",
            ["HudBootstrap.Disabled"] = "a test switch the tests set and restore",
            ["HudBridge.PointerOverUi"] = "set and cleared by its owner (HudController) and by ResetSession; a scene load must NOT clear it",
            ["HudBridge.WheelClaimed"] = "set and cleared by its owner (SelectionController) and by ResetSession; a scene load must NOT clear it",
            ["HudHotkeys.LegacyOverlayActive"] = "follows the HUD flag, set by the HUD it belongs to",
            ["InputFocus.Modal"] = "cleared by SceneStatics.Reset on every scene load",
            ["InputFocus.Listening"] = "cleared by SceneStatics.Reset on every scene load",
            ["InputFocus.escapeFrame"] = "a frame stamp, cleared by SceneStatics.Reset on every scene load",
            ["ProfileStore.current"] = "the backing field of Current, above",
            ["ProfileStore.Writable"] = "whether this process may write profile.json (the live editor/play rule)",
            ["PropHandle.All"] = "the prop handles in the scene, kept by their own OnEnable/OnDisable (editor stand-ins)",
            ["RenderGround.Map"] = "the drawn ground, replaced when the next terrain view builds; tests pass their maps explicitly",
            ["RenderGround.Grid"] = "the drawn ground's heights, replaced when the next terrain view builds",
            ["SceneStatics.session"] = "the reset registry itself: entries are added once, from static constructors",
            ["SceneStatics.perScene"] = "the per-scene reset registry itself: entries are added once, from static constructors",
            ["SettingsStore.Current"] = "the loaded settings.json",
            ["ShellBoot.Disabled"] = "a test switch the tests set and restore",
            ["ShellBoot.settingsApplied"] = "settings are applied once per process, not once per scene",
            ["ShellRouter.Instance"] = "the one shell router, which outlives scene loads by design",
            ["SimHost.BombardmentOverride"] = "cleared by SceneStatics.Reset on every scene load",
            ["SimHost.CanaryOverride"] = "set and restored by the tests and tools that use it",
            ["SimHost.StressOverride"] = "set and restored by the tests and tools that use it",
            ["UnitArt.cache"] = "portraits loaded from Resources; the assets outlive Play",
            ["PerfBench.Running"] = "the bench in flight; CaptureRig reads it after the bench",
            ["PerfBench.LastResultPath"] = "where the last bench wrote, read by CaptureRig after the bench",
            ["PerfBench.LastExitCode"] = "how the last bench ended, read by CaptureRig after the bench",
            ["Knobs.initialised"] = "the knobs are read once per process, from env and settings.json",
            ["Knobs.warned"] = "which unknown knob names have been warned about, once per process",
            ["Storm.Hold"] = "the lightning's hold on the time scale; SceneStatics.Reset puts the time scale back",
            ["SceneHooks.CloseUp"] = "cleared by SceneHooks.Reset (ResetSession); a scene load must NOT clear it",
            ["CombatFx.ShowOverlays"] = "a debug switch a tool sets and clears",
            ["DeathGags.pinned"] = "a capture switch: the caller that pins the gag restores it",
            ["ProvingGroundPanel.custom"] = "the wave the proving-ground panel is editing; the panel owns it",
            ["CameraShake.Strength"] = "a player setting",
            ["MainMenuScreen.shadeTex"] = "a code-made texture, owned by no scene: a Single LoadScene destroys the scene's objects, not an asset, and MainMenuScreen drops it when Play ends",
            ["MainMenuScreen.footTex"] = "the same code-made texture pair as shadeTex, above",
            ["CameraShake.viewDistance"] = "CameraShake registers its own reset; this is part of what that reset puts back",
            ["DebrisRenderer.Instance"] = "the live renderer, replaced when the next CombatFx builds one",
            ["DebrisRenderer.Gore"] = "a player setting",
            ["DebrisRenderer.ZoomShare"] = "the camera's share of every burst, set by CombatFx each frame",
            ["DebrisRenderer.Biome"] = "set by each biome when the scene starts",
            ["DebrisRenderer.LavaLevel"] = "set by each biome when the scene starts",
            ["DebrisRenderer.figure"] = "the figure source loaded from Resources; the asset outlives Play",
            ["DebrisRenderer.figureTried"] = "a load-once latch for the figure above",
            ["TankModel.grown"] = "mesh copies per (mesh, scale); a copy destroyed when Play ends reads as null and is rebuilt, so only stale keys remain",
            ["CampaignSession.MapNode"] = "part of the mission in flight, carried across the scene load by design",
            ["CampaignSession.HomeBuilding"] = "part of the mission in flight, carried across the scene load by design",
            ["KeyMap.Current"] = "the player's bindings from settings.json",
            ["MatchLaunch.Current"] = "the mission request carried across a scene load, by design",
            ["MatchLaunch.Running"] = "the running mission carried across a scene load, by design",
            ["ProfileStore.PersistOverride"] = "a test switch the tests set and restore (null = the live editor/play rule)",
            ["SceneTints.Now"] = "the biome's tints, set by Atmosphere at the start of each scene",
            ["SceneTints.Epoch"] = "the stamp on the tints above, which only grows",
            ["FrameBudget.draws"] = "a frame-stamped counter that rolls over by itself",
            ["FrameBudget.lastDraws"] = "last frame's count of the counter above",
            ["FrameBudget.indirect"] = "a frame-stamped counter that rolls over by itself",
            ["FrameBudget.lastIndirect"] = "last frame's count of the counter above",
            ["FrameBudget.verts"] = "a frame-stamped counter that rolls over by itself",
            ["FrameBudget.lastVerts"] = "last frame's count of the counter above",
            ["GreyboxTerrainView.Unmetered"] = "a capture switch a tool sets and clears",
            ["GreyboxTerrainView.mudDetail"] = "a code-made texture cached for the process",
            ["GreyboxTerrainView.ripples"] = "a code-made texture cached for the process",
            ["GameLogo.layout"] = "the logo layout read from Resources, re-read when Unity has destroyed a texture",
            ["GameLogo.textures"] = "the logo's textures, re-loaded when Unity has destroyed one",
            ["TankRenderer.Machines"] = "the machine models of the field being drawn, rebuilt by the next TankRenderer",
            ["TankRenderer.StandIns"] = "the stand-in models of the field being drawn, rebuilt by the next TankRenderer",
        };

        /// <summary>Readonly tables built once by their initializer: nothing writes a row, so Play cannot change
        /// them. Keyed "Type.Field". A table whose rows ARE written does not belong here — Clips.Table was the one
        /// that did, and it registers a reset instead ([TS11]).</summary>
        static readonly HashSet<string> ConstantTables = new HashSet<string>
        {
            "AimReadout.Cache", "ArmouryScreen.RequiredNames", "ArmouryScreen.Ranks",
            "CampaignDifficulty.Standard", "CampaignGraph.Nodes", "CampaignGraph.FrontLine",
            "DebriefScreen.Rows", "DebriefScreen.RequiredNames",
            "FactionBuildings.StageCosts", "FactionBuildings.TrackCosts", "FactionBuildings.GlobalCosts",
            "FactionBuildings.StageHeights", "FactionBuildings.IronBuildings", "FactionBuildings.BrassBuildings",
            "GymCatalogue.Bands", "HomeFrontScreen.RequiredNames", "HomeFrontScreen.FactionNames",
            "HomeFrontScreen.StageNames", "HomeFrontStages.StageFractions", "HudText.SupportActions",
            "HudView.RequiredNames", "HudView.IgnorePickingNames", "HudView.SupportAbilities", "HudView.SpeedOfIndex",
            "IntText.ints", "IntText.secs", "IntText.clocks", "Knobs.Separators",
            "MainMenuScreen.RequiredNames", "MatchClock.Speeds", "MissionSelectScreen.RequiredNames",
            "PauseMenuScreen.RequiredNames", "ProvingGround.StatusNames",
            "ProvingGroundLaunchScreen.RequiredNames", "ProvingGroundLaunchScreen.GroundNames",
            "ProvingGroundLaunchScreen.Grounds", "ProvingGroundLaunchScreen.BombardmentNames",
            "ProvingGroundLaunchScreen.Bombardments", "ProvingGroundLaunchScreen.Silvers",
            "ProvingGroundLaunchScreen.AiBlurbs", "ProvingGroundPanel.RequiredNames", "ProvingGroundPanel.Counts",
            "ProvingGroundPanel.Everys", "ProvingGroundPanel.Pages",
            "SelectionController.Digits",
            "SettingsScreen.RequiredNames", "SettingsScreen.Tabs", "SettingsScreen.FullscreenNames",
            "SkinSpec.All", "SkinSpec.PortraitNames", "SkinSpec.Fonts",
            "StagingScreen.RequiredNames", "StrategicMapScreen.RequiredNames", "StrategicMapScreen.StateChips",
            "StrategicMapScreen.MissionChips", "TankRenderer.Exhausts", "TestPanel.SlotNames", "UnitArt.StateNames",
            "UnitArt.Faces", "UnitArt.Cutouts", "UnitLook.InfantryNames", "UnitLook.InfantryPortraits",
            "UnitLook.InfantryTips", "UnitStatus.Words", "VATRenderer.FigureNames", "SceneHooks.SmokeSources",
            "PerfBench.HudParts", "PerfBench.SelectionParts", "DebrisRenderer.Capacity",
            "DebrisRenderer.CastsShadow", "FlipbookFx.Sheets", "TankModel.JoinedLegs",
            "TankRenderer.WalkerSizeFactor", "ProvingGround.Ideas", "ProvingGround.AiNames",
            "ProvingGround.AiPresets", "AssetScaleReport.Grounds", "AssetScaleTable.rules", "BattlefieldKit.EnvSets",
            "ProceduralSoldier.Pivot", "ObjectiveTracker.pct", "DeathMarks.timesCache",
            "TrenchOrderCluster.countCache", "TrenchOrderCluster.menCache",
        };

        /// <summary>Types SceneStatics.ResetSession resets directly rather than through a registration.</summary>
        static readonly HashSet<string> ResetDirectly = new HashSet<string> { "SceneHooks" };

        // [TS11] a readonly ARRAY is mutable where it counts: the reference cannot move but every row can be written.
        static bool IsContainer(Type t) =>
            typeof(System.Collections.ICollection).IsAssignableFrom(t)
            || t.GetInterfaces().Any(i => i.IsGenericType && i.GetGenericTypeDefinition() == typeof(ICollection<>));

        /// <summary>A static holding things the ENDING scene owned (a view behind its interface, a component, a mesh)
        /// must be forgotten on the scene load, not only when Play ends: the new scene's first frame would otherwise
        /// be handed a destroyed object that reads as non-null. That was P1a.</summary>
        static bool HoldsSceneObjects(Type ft) =>
            typeof(UnityEngine.Object).IsAssignableFrom(ft) || ft.IsInterface;

        /// <summary>A property's backing field under the name the property is known by.</summary>
        static string Field(FieldInfo f)
        {
            string n = f.Name;
            int b = n.IndexOf('>');
            return n.Length > 0 && n[0] == '<' && b > 1 ? n.Substring(1, b - 1) : n;
        }

        static IEnumerable<FieldInfo> MutableStatics(Type t) =>
            t.GetFields(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
                .Where(f => !f.IsLiteral && (!f.IsInitOnly || IsContainer(f.FieldType)));

        static IEnumerable<Type> Holders()
        {
            // TW.Editor is left out on purpose: editor tools are meant to keep their state across Play.
            var assemblies = AppDomain.CurrentDomain.GetAssemblies()
                .Where(a => { var n = a.GetName().Name; return n.StartsWith("TW.Presentation") || n == "TW.UI" || n == "TW.Perf"; });
            foreach (var asm in assemblies)
                foreach (var t in asm.GetTypes())
                {
                    if (t.Name.Contains('<') || t.IsDefined(typeof(CompilerGeneratedAttribute), false)) continue;
                    if (MutableStatics(t).Any()) yield return t;
                }
        }

        [Test]
        public void Every_Mutable_Static_Is_Reset_When_Play_Ends_Or_Explained()
        {
            var unexplained = new List<string>();
            var notPerScene = new List<string>();
            var present = new HashSet<string>();
            foreach (var t in Holders())
            {
                var fields = MutableStatics(t).ToList();
                bool anyChecked = false;
                foreach (var f in fields)
                {
                    string key = t.Name + "." + Field(f);
                    present.Add(key);
                    if (!Explained.ContainsKey(key) && !ConstantTables.Contains(key)) anyChecked = true;
                }
                if (!anyChecked) continue;
                RuntimeHelpers.RunClassConstructor(t.TypeHandle);   // registration happens in the static constructor
                bool session = SceneStatics.Registered.Contains(t.Name) || ResetDirectly.Contains(t.Name);
                bool perScene = SceneStatics.PerSceneRegistered.Contains(t.Name);
                foreach (var f in fields)
                {
                    string key = t.Name + "." + Field(f);
                    if (Explained.ContainsKey(key) || ConstantTables.Contains(key)) continue;
                    if (!session) unexplained.Add(key);
                    else if (HoldsSceneObjects(f.FieldType) && !perScene) notPerScene.Add(key);
                }
            }
            var stale = Explained.Keys.Concat(ConstantTables).Where(k => !present.Contains(k)).ToList();
            var problems = new List<string>();
            if (unexplained.Count > 0)
                problems.Add("nothing puts these statics back when Play ends. Register a reset with " +
                    "SceneStatics.Register(nameof(Type), Reset) from a static constructor, or name the field in " +
                    "Explained (or ConstantTables) here with the reason it is safe: " + string.Join(", ", unexplained));
            if (notPerScene.Count > 0)
                problems.Add("these statics hold objects the scene owned, so forgetting them only when Play ends " +
                    "hands the next scene a destroyed object that reads as non-null (P1a). Add " +
                    "SceneStatics.RegisterPerScene: " + string.Join(", ", notPerScene));
            if (stale.Count > 0)
                problems.Add("no longer a mutable static; remove them from Explained/ConstantTables: " +
                    string.Join(", ", stale));
            Assert.That(problems, Is.Empty, string.Join(" || ", problems));
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

        /// <summary>
        /// Every public static of SceneHooks, not a sample of four: set each to something that is not its default
        /// (a delegate gets a compiled do-nothing lambda of its own signature), end the session, and require each
        /// one back at its default. A hook added without a line in SceneHooks.Reset fails here.
        /// </summary>
        [Test]
        public void Scene_Hooks_Are_Empty_When_The_Session_Ends()
        {
            var fields = typeof(SceneHooks)
                .GetFields(BindingFlags.Static | BindingFlags.Public).Where(f => !f.IsLiteral).ToList();
            Assert.That(fields.Count, Is.GreaterThan(15), "every hook is a public static field; this found almost none");
            foreach (var f in fields) Dirty(f);
            foreach (var f in fields)
                Assert.That(IsDefault(f), Is.False, "the sweep failed to dirty SceneHooks." + f.Name);

            SceneStatics.ResetSession();

            var left = fields.Where(f => !IsDefault(f)).Select(f => f.Name).ToList();
            Assert.That(left, Is.Empty, "SceneHooks.Reset leaves these set, so the next scene inherits the last " +
                "one's services: " + string.Join(", ", left));
        }

        static void Dirty(FieldInfo f)
        {
            var ft = f.FieldType;
            if (typeof(Delegate).IsAssignableFrom(ft))
            {
                // a do-nothing delegate of this exact signature, whatever that signature is
                var sig = ft.GetMethod("Invoke");
                var ps = sig.GetParameters()
                    .Select(p => System.Linq.Expressions.Expression.Parameter(p.ParameterType, p.Name)).ToArray();
                System.Linq.Expressions.Expression body = sig.ReturnType == typeof(void)
                    ? (System.Linq.Expressions.Expression)System.Linq.Expressions.Expression.Empty()
                    : System.Linq.Expressions.Expression.Default(sig.ReturnType);
                f.SetValue(null, System.Linq.Expressions.Expression.Lambda(ft, body, ps).Compile());
                return;
            }
            if (f.IsInitOnly)
            {
                var list = f.GetValue(null) as System.Collections.IList;
                Assert.That(list, Is.Not.Null, "readonly hook " + f.Name + ": teach this sweep how to dirty it");
                list.Add(Activator.CreateInstance(ft.GetGenericArguments()[0]));
                return;
            }
            if (ft == typeof(float)) { f.SetValue(null, 0.75f); return; }
            if (ft == typeof(bool)) { f.SetValue(null, true); return; }
            if (ft == typeof(int)) { f.SetValue(null, 7); return; }
            Assert.Fail("teach this sweep how to dirty a " + ft.Name + " (SceneHooks." + f.Name + ")");
        }

        static bool IsDefault(FieldInfo f)
        {
            if (f.IsInitOnly) return ((System.Collections.ICollection)f.GetValue(null)).Count == 0;
            var v = f.GetValue(null);
            if (!f.FieldType.IsValueType) return v == null;      // a delegate's default is null, and it has no ctor
            return v.Equals(Activator.CreateInstance(f.FieldType));
        }
    }
}
