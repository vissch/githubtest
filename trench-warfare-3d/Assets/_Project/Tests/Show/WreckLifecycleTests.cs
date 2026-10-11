// Phase: review 2 (2026-10-11) — a flyer's wreck coming down. Two pieces of code move the same hull: Smoulder drops it as
// a stone and clamps it to the floor, and the death gag's drop adds its own fall frame by frame. With both owning the
// fall the hull ends metres under the ground, which no still shows and no other test reads, so the rig here is the
// renderer itself: a View made by reflection, stepped through Smoulder and GagsFrame as LateUpdate does.
using System;
using System.Collections;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Terrain;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WreckLifecycleTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>Unity calls OnDestroy only on a component that was running, which an edit-mode test's never is: call
        /// the very method a destroyed renderer would get.</summary>
        static void Gone(MonoBehaviour mb)
        {
            var m = mb.GetType().GetMethod("OnDestroy", Any);
            Assert.NotNull(m, mb.GetType().Name + ".OnDestroy");
            m.Invoke(mb, null);
        }

        static void Set(object v, string field, object value)
        {
            var f = v.GetType().GetField(field, Any);
            Assert.NotNull(f, $"View has no field {field}");
            f.SetValue(v, value);
        }

        static void Heave(object v, float value)
        {
            var f = v.GetType().GetField("Heave", Any);
            object spring = f.GetValue(v);
            spring.GetType().GetField("Value", Any).SetValue(spring, value);
            f.SetValue(v, spring);
        }

        static float HeaveOf(object v)
        {
            object spring = v.GetType().GetField("Heave", Any).GetValue(v);
            return (float)spring.GetType().GetField("Value", Any).GetValue(spring);
        }

        /// <summary>[N7.1] a flyer killed in the air, stepped until both the fall and the gag have finished, rests at the
        /// floor and not under it.</summary>
        [Test]
        public void AFlyersWreckRestsOnTheGroundNotUnderIt()
        {
            var go = new GameObject("flyer wreck rig");
            try
            {
                var r = go.AddComponent<TankRenderer>();   // no Host: Ground is 0 everywhere
                var type = r.GetType();
                var viewType = type.GetNestedType("View", BindingFlags.NonPublic);
                Assert.NotNull(viewType, "TankRenderer.View");
                var model = TankModel.Load("Hopper", VehicleArchetype.Hopper, "Hull", 1f);
                Assert.NotNull(model, "the Hopper did not load from Resources/Vehicles/Hopper");
                int parts = model.Lods[0].Parts.Count;

                object v = Activator.CreateInstance(viewType, true);
                Set(v, "Model", model);
                Set(v, "World", new Matrix4x4[parts]);
                Set(v, "Off", new bool[parts]);
                Set(v, "Lift", TankRenderer.FlyerLift);
                Set(v, "Hover", true);
                Set(v, "Dead", true);
                Set(v, "Linked", true);        // a wreck prop in the sim: nothing sinks it out of sight
                Heave(v, TankRenderer.FlyerLift);

                float now = 100f;
                Set(v, "DiedAt", now);
                // the gag's drop as DeathGag leaves it for a hover machine (TankRenderer.Deaths): a hop, then down to the ground
                Set(v, "Dropping", true);
                Set(v, "DropLanded", false);
                Set(v, "Dropped", 0f);
                Set(v, "DropFrom", TankRenderer.FlyerLift);
                Set(v, "DropUp", VehicleGags.HopSpeed(1f));
                Set(v, "DropAt", now);
                Set(v, "DropTo", 0f);

                var wrecks = (IList)type.GetField("wrecks", Any).GetValue(r);
                wrecks.Add(v);

                var smoulder = type.GetMethod("Smoulder", Any);
                var gags = type.GetMethod("GagsFrame", Any);
                Assert.NotNull(smoulder, "TankRenderer.Smoulder"); Assert.NotNull(gags, "TankRenderer.GagsFrame");
                const float dt = 1f / 60f;
                for (int i = 0; i < 600; i++)
                {
                    now += dt;
                    smoulder.Invoke(r, new object[] { v, dt, now });
                    gags.Invoke(r, new object[] { dt, now });
                }

                float h = HeaveOf(v);
                Assert.IsTrue(h >= 0f && h <= 0.5f, $"[N7.1] the flyer's wreck rests {h:0.00} m, under the ground at 0.00 m");
                Assert.IsFalse((bool)viewType.GetField("Dropping", Any).GetValue(v), "[N7.1] the drop has finished");
            }
            finally
            {
                UnityEngine.Object.DestroyImmediate(go);
            }
        }

        /// <summary>A stand-in for Presentation/Terrain's BattlefieldProps: the renderer asks for a recomposition by name
        /// (SendMessage on the component whose type is called BattlefieldProps), so a counter of the same name is enough.</summary>
        [ExecuteAlways]   // SendMessage refuses a component that is not running, and an edit-mode test is not Play
        sealed class BattlefieldProps : MonoBehaviour
        {
            public int Recomposed;
            public void Recompose() => Recomposed++;
        }

        /// <summary>A Maw hull put into the renderer's private model field: TankRenderer.Start loads it
        /// (Presentation/Camera/TankRenderer.cs:303) and an edit-mode test never runs Start, so AdoptHusks would find no
        /// model and adopt nothing. VehicleSize.Tank is 1.7f.</summary>
        static bool GiveAMawModel(TankRenderer r)
        {
            var model = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", 1.7f);
            if (model == null) return false;
            Set(r, "maw", model);
            return true;
        }

        /// <summary>A wreck prop on the greybox map: AddProp refuses a trench, a link, a bunker or a blocked cell
        /// (Sim/Terrain/MapData.cs:176-181), so try a few spots. Returns its index, or -1.</summary>
        static int AddAWreck(TW.Sim.Match.MatchSim match)
        {
            for (float x = 4f; x < 60f; x += 7f)
                for (float z = 4f; z < 60f; z += 7f)
                {
                    int i = match.Map.AddProp(new PropDef { Pos = new Unity.Mathematics.float3(x, 0f, z), Kind = PropKind.Wreck, Scale = 1f });
                    if (i >= 0) return i;
                }
            return -1;
        }

        /// <summary>[N7.2] adopting the map's own wrecks as husks asks the composer to compose again, so its grey stand-ins
        /// go when the composer next runs (the ask is a flag; BattlefieldProps.Update claims HeavyWork and composes over
        /// several frames) instead of being drawn over the husks until the prop's first change.</summary>
        [Test]
        public void AdoptingHusksAsksForARecompose()
        {
            var props = new GameObject("props stand-in").AddComponent<BattlefieldProps>();
            var go = new GameObject("husk rig");
            TW.Sim.Match.MatchSim match = null;
            try
            {
                var r = go.AddComponent<TankRenderer>();
                match = TW.Sim.Match.MatchSim.CreateGreybox(SimConfig.Default);
                Assert.IsTrue(GiveAMawModel(r), "the premise: the Maw hull loaded (without it nothing can be adopted)");
                Assert.GreaterOrEqual(AddAWreck(match), 0, "the premise: a wreck prop went onto the map");
                var adopt = r.GetType().GetMethod("AdoptHusks", Any);
                Assert.NotNull(adopt, "TankRenderer.AdoptHusks");
                adopt.Invoke(r, new object[] { match, 100f });
                var wrecks = (System.Collections.ICollection)r.GetType().GetField("wrecks", Any).GetValue(r);
                Assert.AreEqual(1, wrecks.Count, "the premise: the map's wreck was adopted as a husk");
                Assert.AreEqual(1, props.Recomposed, "[N7.2] the composer was not asked to compose again, so its grey stand-ins stay beside the husks");
            }
            finally
            {
                match?.Dispose();
                UnityEngine.Object.DestroyImmediate(go);
                UnityEngine.Object.DestroyImmediate(props.gameObject);
            }
        }

        /// <summary>[N7.c] A match whose map holds no wreck adopts no husk, so it must not ask for a recomposition: the
        /// ask costs a whole-map composition, and every match start went through here.</summary>
        [Test]
        public void AMatchWithNoHuskAsksForNoRecompose()
        {
            var props = new GameObject("props stand-in").AddComponent<BattlefieldProps>();
            var go = new GameObject("husk rig");
            TW.Sim.Match.MatchSim match = null;
            try
            {
                var r = go.AddComponent<TankRenderer>();
                match = TW.Sim.Match.MatchSim.CreateGreybox(SimConfig.Default);
                GiveAMawModel(r);
                var adopt = r.GetType().GetMethod("AdoptHusks", Any);
                Assert.NotNull(adopt, "TankRenderer.AdoptHusks");
                adopt.Invoke(r, new object[] { match, 100f });
                var wrecks = (System.Collections.ICollection)r.GetType().GetField("wrecks", Any).GetValue(r);
                Assert.AreEqual(0, wrecks.Count, "the premise: the greybox map holds no wreck prop, so no husk was adopted");
                Assert.AreEqual(0, props.Recomposed, "[N7.c] a match with no husk still paid for a whole-map composition");
            }
            finally
            {
                match?.Dispose();
                UnityEngine.Object.DestroyImmediate(go);
                UnityEngine.Object.DestroyImmediate(props.gameObject);
            }
        }

        /// <summary>[N7.6] the cut carcass meshes are HideAndDontSave: the renderer going away destroys them.</summary>
        [Test]
        public void TheRendererDestroysItsCarcassMeshes()
        {
            var go = new GameObject("carcass rig");
            var hull = new Mesh { name = "hull" };
            var mesh = new Mesh { name = "hull carcass", hideFlags = HideFlags.HideAndDontSave };
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var field = r.GetType().GetField("carcasses", Any);
                Assert.NotNull(field, "TankRenderer.carcasses");
                var carcasses = (IDictionary)field.GetValue(r);
                carcasses[hull] = new Carcass { Mesh = mesh };
                Gone(r);   // outside Play Unity does not call OnDestroy itself; this is the same method it would call
                Assert.IsTrue(mesh == null, "[N7.6] the carcass mesh outlived the renderer");
                Assert.IsTrue(hull != null, "[N7.6] the live model's own hull was destroyed with it");
            }
            finally
            {
                if (go != null) UnityEngine.Object.DestroyImmediate(go);
                if (mesh != null) UnityEngine.Object.DestroyImmediate(mesh);
                if (hull != null) UnityEngine.Object.DestroyImmediate(hull);
            }
        }

        /// <summary>[N7.6] the two gag mark materials are HideAndDontSave: CombatFx going away destroys them.</summary>
        [Test]
        public void CombatFxDestroysItsGagMarkMaterials()
        {
            var shader = Shader.Find("TW/GroundMark (URP)") ?? Shader.Find("Unlit/Color") ?? Shader.Find("Sprites/Default");
            Assert.NotNull(shader, "no shader to make a stand-in material from");
            var go = new GameObject("gag mark rig");
            var mats = new[] { new Material(shader) { hideFlags = HideFlags.HideAndDontSave }, new Material(shader) { hideFlags = HideFlags.HideAndDontSave } };
            try
            {
                var fx = go.AddComponent<CombatFx>();
                var field = fx.GetType().GetField("gagMarkMats", Any);
                Assert.NotNull(field, "CombatFx.gagMarkMats");
                var held = (Material[])field.GetValue(fx);
                held[0] = mats[0]; held[1] = mats[1];
                Gone(fx);
                Assert.IsTrue(mats[0] == null && mats[1] == null, "[N7.6] a gag mark material outlived CombatFx");
            }
            finally
            {
                if (go != null) UnityEngine.Object.DestroyImmediate(go);
                foreach (var m in mats) if (m != null) UnityEngine.Object.DestroyImmediate(m);
            }
        }
    }
}
