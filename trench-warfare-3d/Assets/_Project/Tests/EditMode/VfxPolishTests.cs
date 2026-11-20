// Phase: VFX pass (2026-10-01) — the rules behind the effects' polish, where they are plain numbers a test can hold:
// a called strike's marker is a rim that lasts as long as its payload and thins away (CombatFx.Markers.cs); a burst
// that digs no crater still scorches the ground by its shape (GreyboxTerrainView.LightScorchRadius); a scorch has a
// charred core (ScorchTilePainter.Burn); the star shell paints a pool smaller than the field (NightLights); and the
// shaders keep the lines the pictures were fixed with (the stacked lights' roll-off, the round flake, the torn flame).
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;
using TW.Sim.Match;

namespace TW.Tests
{
    public class VfxPolishTests
    {
        [Test]
        public void AMarkerLastsAsLongAsItsPayload_AndThinsAway()
        {
            const float tick = 0.05f;
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var he));
            float life = CombatFx.MarkerLife((int)OffMapAbilityId.HeBarrage, tick);
            Assert.AreEqual(Mathf.Clamp((he.WarmupTicks + he.SpreadTicks) * tick + 0.5f, 3f, 20f), life, 1e-4f, "to the last shell and half a second");
            Assert.That(CombatFx.MarkerLife((int)OffMapAbilityId.StrafeRun, tick), Is.InRange(3f, 20f));
            Assert.AreEqual(CombatFx.MarkerSeconds, CombatFx.MarkerLife(-1, tick), "an ability with no stats keeps the old ten seconds");
            Assert.AreEqual(CombatFx.MarkerRimWidth, CombatFx.MarkerRim(8f), 1e-5f, "full width while there is time left");
            Assert.Less(CombatFx.MarkerRim(1f), CombatFx.MarkerRimWidth, "thinner in its last seconds");
            Assert.AreEqual(0f, CombatFx.MarkerRim(0f), "and gone at the end, not blinked out");
            Assert.LessOrEqual(CombatFx.MarkerRimWidth, 1f, "a rim, not a plate over the field (VFX round 1)");
        }

        [Test]
        public void ABurstWithNoCraterStillScorches_ByItsShape()
        {
            Assert.AreEqual(0f, GreyboxTerrainView.LightScorchRadius((int)TW.Sim.Combat.BlastShape.Shell, 6f), "a shell's scorch is its crater's");
            Assert.AreEqual(0f, GreyboxTerrainView.LightScorchRadius((int)TW.Sim.Combat.BlastShape.Mine, 4f));
            Assert.Greater(GreyboxTerrainView.LightScorchRadius((int)TW.Sim.Combat.BlastShape.Beam, 3f), 1.5f, "the beam chars the line it walks");
            Assert.That(GreyboxTerrainView.LightScorchRadius((int)TW.Sim.Combat.BlastShape.Strafe, 2f), Is.InRange(0.5f, 1.5f), "a strafe's rounds pock it");
            Assert.Greater(GreyboxTerrainView.LightScorchRadius((int)TW.Sim.Combat.BlastShape.Incendiary, 5f), 2f, "an incendiary blackens what it burnt");
            Assert.GreaterOrEqual(GreyboxTerrainView.MaxScorchMarks, 96, "room for them beside the craters");
        }

        [Test]
        public void AScorchHasACharredCore_AndNothingAtItsRim()
        {
            Assert.AreEqual(ScorchTilePainter.BurnCore, ScorchTilePainter.Burn(0f), 1e-5f);
            Assert.AreEqual(ScorchTilePainter.BurnCore, ScorchTilePainter.Burn(0.4f), 1e-5f, "charred out to near half its radius");
            Assert.Less(ScorchTilePainter.Burn(0.8f), ScorchTilePainter.BurnCore);
            Assert.AreEqual(0f, ScorchTilePainter.Burn(1f), 1e-5f);
            Assert.Greater(ScorchTilePainter.BurnCore, 0.6f, "a lone shell hole reads at the play zoom (round 3: a faint smudge at .45)");
        }

        [Test]
        public void TheStarShellPaintsAPool_SmallerThanTheField()
        {
            Assert.Greater(NightLights.FlarePoolReach, 40f, "a wide pool");
            Assert.Less(NightLights.FlarePoolReach, 70f, "and has an edge inside the standard view (round 4: the whole picture lit evenly)");
            Assert.Less(NightLights.FlarePoolHeight, NightLights.FlarePoolReach * 0.4f, "the pool hangs low under the flare, or none of it reaches the mud (round 5)");
            Assert.Greater(NightLights.FlarePoolGain, 0.5f);
        }

        [Test]
        public void TheShadersKeepTheirFixes()
        {
            string dir = Path.Combine(Application.dataPath, "_Project", "Shaders");
            string lights = File.ReadAllText(Path.Combine(dir, "TWLocalLights.hlsl"));
            StringAssert.Contains("if (most > 1.18) sum *= (1.18 + min((most - 1.18) * 0.25, 0.4)) / most;", lights, "stacked fires roll off above one lamp's worth");
            StringAssert.Contains("min(peak, 6.0)", lights, "a fire's glint on water is no wider than a lantern's");
            string poolsHlsl = File.ReadAllText(Path.Combine(dir, "TWLightPools.hlsl"));
            StringAssert.Contains("glints = TWGlintRoll(glints);", poolsHlsl, "a fire's glint on wet mud keeps its orange and never clips to yellow");
            StringAssert.Contains("return TWGlintRoll(sum);", poolsHlsl);
            string rain = File.ReadAllText(Path.Combine(dir, "Rain_URP.shader"));
            StringAssert.Contains("saturate((1.0 - length(q)) * 2.5)", rain, "a flake is round, not the quad it is drawn on");
            string flame = File.ReadAllText(Path.Combine(dir, "Flame_URP.shader"));
            StringAssert.Contains("half lick =", flame, "the flame is torn into tongues");
            StringAssert.Contains("half gutter =", flame, "and each gutters at its own height");
        }
    }
}
