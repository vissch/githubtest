// Phase: P0 (implemented)
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;

namespace TW.Tests
{
    public class SimHashTests
    {
        [Test]
        public void EqualArrays_HashEqual_DifferentArrays_HashDiffer()
        {
            using var a = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var b = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var c = new NativeArray<float>(new[] { 1f, 2f, 3.0001f }, Allocator.Temp);
            Assert.AreEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(b, SimHash.Offset));
            Assert.AreNotEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(c, SimHash.Offset));
        }

        [Test]
        public void SimRandom_IsStableForSameInputs()
        {
            var r1 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            var r2 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            Assert.AreEqual(r1.NextUInt(), r2.NextUInt());
            var r3 = SimRandom.For(42, 11, SimRandom.SystemId.Test, 3);
            Assert.AreNotEqual(SimRandom.For(42, 10, SimRandom.SystemId.Test, 3).NextUInt(), r3.NextUInt());
        }

        [Test]
        public void SimMath_TrigMatchesReferenceWithinTolerance()
        {
            for (int i = -720; i <= 720; i++)
            {
                float x = i * 0.5f * Unity.Mathematics.math.PI / 180f * 4f;
                Assert.AreEqual(System.Math.Sin(x), SimMath.Sin(x), 2e-5, $"sin({x})");
                Assert.AreEqual(System.Math.Cos(x), SimMath.Cos(x), 2e-5, $"cos({x})");
            }
            Assert.AreEqual(System.Math.Atan2(1, 2), SimMath.Atan2(1f, 2f), 1e-4);
            Assert.AreEqual(System.Math.Atan2(-3, -0.5), SimMath.Atan2(-3f, -0.5f), 1e-4);
        }

        /// <summary>The systems whose state SimWorld.Hash() folds, in the order it folds them, as FormatVersion 15 has
        /// them in the greybox match. Two SIM lanes that each add a system merge without a conflict into a chain
        /// neither of them ran, and a determinism rerun then compares the merged build with itself and passes (the
        /// overhaul and units-meta, 2026-09-27). This fails instead: a change to the chain is a format change, so it
        /// bumps ReplayRecorder.FormatVersion and pins the new chain here in the same commit.</summary>
        // The systems have not changed since v9; v10-v15 changed what the tables hold and what TankGunnerySystem hashes, not
        // the order of the chain. Named for what it is, not for a version it no longer matches.
        const string SystemChain =
            "40 CombatCatalogueSystem;50 TerrainHashSystem;110 TrenchOrdersSystem;130 OffMapAbilitySystem;" +
            "400 FlowFieldManager;600 TargetAcquisitionSystem;650 AuraSystem;700 DirectFireSystem;" +
            "705 TankGunnerySystem;715 AmbientBombardmentSystem;720 BlastSystem;723 BeamSystem;" +
            "725 BurningSystem;730 VehicleModulesSystem;735 SupportSystem;800 SuppressionSystem;" +
            "805 HeroSystem;820 TrenchGarrisonSystem;900 GasSmokeSystem;1000 DeformationSystem;" +
            "1105 LeapSystem;1110 MovementSystem;1115 BreakerSystem;1120 VehicleKinematicsSystem;" +
            "1130 MineSystem;1200 SectorControlSystem;";

        [Test]
        public void TheHashChainIsTheOneItsFormatVersionNames()
        {
            using var match = TW.Sim.Match.MatchSim.CreateGreybox(SimConfig.Default);
            var chain = new System.Text.StringBuilder();
            foreach (var s in match.World.Systems) chain.Append(s.Order).Append(' ').Append(s.GetType().Name).Append(';');
            Assert.AreEqual(15, ReplayRecorder.FormatVersion, "a new FormatVersion pins its own chain here");
            Assert.AreEqual(SystemChain, chain.ToString(), "the hash chain changed: bump ReplayRecorder.FormatVersion and pin this chain: " + chain);
        }
    }
}
