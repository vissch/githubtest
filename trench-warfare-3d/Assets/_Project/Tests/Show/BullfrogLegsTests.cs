// Phase: the battle (2026-10-07) — the Bullfrog's legs move in the battle (TankRenderer.HopLegs.cs)
// What would go wrong silently: the battle's copy of the leg weights drifts from the Playground's, or the battle model
// is re-split so the weights no longer fit its hull. Either way WeldedLegRig returns no rig, the renderer says so once
// in the log, and the toad goes back to hopping as one stiff piece, which is what the owner sent back on 2026-10-07.
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim;

namespace TW.Tests
{
    public class BullfrogLegsTests
    {

        static WeldedLegRig Rig(out Mesh[] copies, out TankModel model)
        {
            model = TankModel.Load("Bullfrog", VehicleArchetype.Bullfrog, "Hull", TankRenderer.ScaleOf(VehicleArchetype.Bullfrog));
            Assert.NotNull(model, "no battle model");
            var json = Resources.Load<TextAsset>("Vehicles/Bullfrog/Bullfrog_legs");
            Assert.NotNull(json, "Resources/Vehicles/Bullfrog/Bullfrog_legs.json is missing");
            copies = new Mesh[2];
            for (int lod = 0; lod < 2; lod++)
            {
                Assert.NotNull(model.Lods[lod], $"no LOD{lod}");
                TankModel.Part hull = null;
                foreach (var p in model.Lods[lod].Parts) if (p.Parent < 0) { hull = p; break; }
                Assert.NotNull(hull, $"LOD{lod} has no root part");
                Assert.IsTrue(hull.Mesh.isReadable, $"LOD{lod}'s hull cannot be read, so it cannot be skinned");
                copies[lod] = Object.Instantiate(hull.Mesh);
            }
            return WeldedLegRig.Parse(json.text, copies, "test", TankRenderer.HopLegsLod, TankRenderer.ScaleOf(VehicleArchetype.Bullfrog), TankRenderer.HopLegsWithin);
        }

        [Test]
        public void TheLegWeightsFitTheBattleModelAtBothDistances()
        {
            var rig = Rig(out var copies, out _);
            try { Assert.NotNull(rig, "the weights no longer fit the battle model's hull: run Tools/legrig.py and copy the file again (docs/22)"); }
            finally { foreach (var m in copies) Object.DestroyImmediate(m); }
        }

        [Test]
        public void MidHopTheLegsMoveAndTheBodyDoesNot()
        {
            var rig = Rig(out var copies, out _);
            try
            {
                Assert.NotNull(rig);
                var bones = new HopLegs.Bones(rig);
                for (int s = 0; s < 2; s++)
                {
                    Assert.GreaterOrEqual(bones.Thigh[s], 0, "no thigh bone"); Assert.GreaterOrEqual(bones.Shin[s], 0, "no shin bone");
                    Assert.GreaterOrEqual(bones.Arm[s], 0, "no arm bone");
                }
                var rest = copies[0].vertices;
                // a fifth of the way through the flight: the hind legs are out behind, the forelegs tucked
                HopLegs.Drives(HopLegs.Crouch + HopLegs.Flight * 0.2f, out float extend, out float trail, out float open, out float tuck, out float reach, out _);
                Assert.Greater(extend, 0.5f, "the hind legs are not unfolded a fifth of the way through the flight");
                HopLegs.Pose(rig, bones, extend, extend, trail, open, tuck, reach, 0f, 0f, 0f);
                rig.Solve(); rig.Apply(0);
                var posed = copies[0].vertices;
                float legMoved = 0f, bodyMoved = 0f; int legs = 0, body = 0;
                for (int i = 0; i < rest.Length; i++)
                {
                    float d = (posed[i] - rest[i]).magnitude, share = rig.LegShare(0, i);
                    if (share > 0.9f) { legMoved = Mathf.Max(legMoved, d); legs++; }
                    else if (share < 0.001f) { bodyMoved = Mathf.Max(bodyMoved, d); body++; }
                }
                Assert.Greater(legs, 100, "hardly any vertex follows a leg"); Assert.Greater(body, 100, "hardly any vertex is the body's");
                Assert.Greater(legMoved, 0.5f, "mid-hop no leg vertex has moved half a metre: the legs are not posed");
                Assert.Less(bodyMoved, 0.002f, "the body moved with the legs");   // (a vertex counted as the body may still carry a thousandth of a leg)
                // and back at rest the hull is the sculpt again
                HopLegs.Pose(rig, bones, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f);
                rig.Solve(); rig.Apply(0);
                var back = copies[0].vertices; float off = 0f;
                for (int i = 0; i < rest.Length; i++) off = Mathf.Max(off, (back[i] - rest[i]).magnitude);
                Assert.Less(off, 1e-4f, "sitting, the legs are not as sculpted");
            }
            finally { foreach (var m in copies) Object.DestroyImmediate(m); }
        }

        [Test]
        public void TheBattlesCopyOfTheWeightsIsThePlaygrounds()
        {
            string battle = Path.Combine(Application.dataPath, "_Project/Resources/Vehicles/Bullfrog/Bullfrog_legs.json");
            string playground = Path.Combine(Application.dataPath, "_Project/Playground/Art/Tanks/Bullfrog/Bullfrog_legs.json");
            Assert.IsTrue(File.Exists(playground), "the Playground's legs file is gone");
            Assert.AreEqual(File.ReadAllText(playground), File.ReadAllText(battle), "the battle's legs file is not the Playground's: copy it again after Tools/legrig.py");
        }

        [Test]
        public void KnockedOut_TheToadGoesDownWhileItsSlotLives_EasedNotInOneFrame()
        {
            // review B1: the sim keeps a knocked-out machine's slot for 12 to 24 s, and for that time the toad sat at full
            // height, breathing: the slump was keyed on the wreck, and a view is only made a wreck as it leaves the renderer
            const float dt = 1f / 60f;
            int active = (int)TW.Sim.Units.VehicleState.Active, knockedOut = (int)TW.Sim.Units.VehicleState.KnockedOut, cookingOff = (int)TW.Sim.Units.VehicleState.CookingOff;
            Assert.IsFalse(TankRenderer.HopDown(active, false), "a live toad is up");
            Assert.IsTrue(TankRenderer.HopDown(knockedOut, false), "knocked out, its slot still alive: it goes down");
            Assert.IsTrue(TankRenderer.HopDown(cookingOff, false), "cooking off: down");
            Assert.IsTrue(TankRenderer.HopDown(active, true), "a wreck: down");
            // alive, it holds its live shape
            float up = 1f;
            for (int f = 0; f < 120; f++) up = TankRenderer.HopSquashStep(up, 1f, active, false, dt);
            Assert.AreEqual(1f, up, 1e-5f, "a live toad sitting stays at its height");
            // knocked out, it sinks toward DeadSquash (0.62 of its height): begun on the first frame, lower every frame
            // after, nearly down a second on, down at three; so the wreck that follows starts from the slump, not a jump
            float s = TankRenderer.HopSquashStep(1f, 1f, knockedOut, false, dt), last = s;
            Assert.That(s, Is.InRange(0.95f, 0.999f), "one frame after the knock-out it has begun to sink, and has not snapped flat");
            for (int f = 1; f < 60; f++) { s = TankRenderer.HopSquashStep(s, 1f, knockedOut, false, dt); Assert.LessOrEqual(s, last, $"frame {f}: it only sinks"); last = s; }
            Assert.That(s, Is.InRange(TankRenderer.DeadSquash, TankRenderer.DeadSquash + 0.02f), "a second on it is nearly down");
            for (int f = 60; f < 180; f++) s = TankRenderer.HopSquashStep(s, 1f, knockedOut, false, dt);
            Assert.AreEqual(TankRenderer.DeadSquash, s, 1e-3f, "three seconds on it lies at the wreck's height");
        }
    }
}
