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
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WreckLifecycleTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

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
    }
}
