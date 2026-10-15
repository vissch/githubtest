// Phase: review 3 (2026-10-15) — a walker's legs coming off and going back on. The renderer threw a leg when the sim's
// LegsLost bit went up but did nothing when it came down, so a mended leg stayed hidden for the rest of the match.
// The rig is the renderer itself: a View made by reflection, driven through the one method that reads the sim's mask.
using System;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class WalkerLegLossTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static void Set(object v, string field, object value)
        {
            var f = v.GetType().GetField(field, Any);
            Assert.NotNull(f, "View has no field " + field);
            f.SetValue(v, value);
        }

        /// <summary>[K7] the sim's LegsLost bit going down puts the leg back: the mask is the truth, not the moment it
        /// changed. The finding, not a run, says what is right here ("a cleared LegsLost bit is not handled").</summary>
        [Test]
        public void A_Mended_Leg_Comes_Back_When_The_Sim_Clears_Its_Bit()
        {
            var go = new GameObject("walker leg rig");
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var type = r.GetType();
                var viewType = type.GetNestedType("View", BindingFlags.NonPublic);
                Assert.NotNull(viewType, "TankRenderer.View");
                var model = TankModel.Load("Redoubt", VehicleArchetype.Redoubt, "Body", VehicleSize.Walker);
                Assert.NotNull(model, "the Redoubt did not load from Resources/Vehicles/Redoubt");
                int parts = model.Lods[0].Parts.Count;

                object v = Activator.CreateInstance(viewType, true);
                Set(v, "Model", model);
                Set(v, "World", new Matrix4x4[parts]);
                Set(v, "Off", new bool[parts]);

                int leg0 = -1;
                var list = model.Lods[0].Parts;
                for (int i = 0; i < list.Count; i++)
                    if (list[i].Leg == 0 && (list[i].Role == TankPartRole.Leg || list[i].Role == TankPartRole.Thigh)) { leg0 = i; break; }
                Assert.GreaterOrEqual(leg0, 0, "the Redoubt has a part for leg 0");

                var changed = type.GetMethod("LegsChangedTo", Any);
                Assert.NotNull(changed, "TankRenderer.LegsChangedTo");

                changed.Invoke(r, new object[] { v, (byte)0b0001 });
                var off = (bool[])viewType.GetField("Off", Any).GetValue(v);
                Assert.IsTrue(off[leg0], "[K7] leg 0 is thrown clear when the sim loses it");

                changed.Invoke(r, new object[] { v, (byte)0 });
                Assert.IsFalse(off[leg0], "[K7] a mended leg is drawn again when the sim clears its bit");
            }
            finally { UnityEngine.Object.DestroyImmediate(go); }
        }
    }
}
