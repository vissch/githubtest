// Phase: B6 (2026-10-14, [sapper-button]) — the battle bar's lay-mine and lay-tripwire buttons send the very command
// the sim already consumes, and they are not there without a sapper.
// SapperOrders is pure over a SimWorld and a SapperSystem, so the rig here is SapperTests' own (MatchSim.CreatePlaytest,
// Spawn, Step): the commands the HUD builds are stepped through the real MatchSim and the sim's own events are what the
// test reads. Expected values are the sim's constants and the open point SapperTests uses; nothing is recomputed the
// way the code computes it.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;
using TW.UI;

namespace TW.Tests
{
    public class SapperOrderTests
    {
        static readonly float3 Open = new float3(150f, 0f, 240f);   // Tests/Sim/SapperTests.cs:21

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly List<SimEvent> Log = new List<SimEvent>();
            public SimWorld W => M.World;
            public SapperSystem Sapper => M.Sapper;
            public Rig()
            {
                var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }
            public int Man(byte archetype, float3 at, byte team = 0)
            {
                var e = W.Units.Roster[archetype];
                return W.Spawn(team, archetype, at, e.Hp, e.Speed, false);
            }
            public void Step(params SimCommand[] cmds)
            {
                using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                M.Step(arr);
                var ev = W.Events.Events;
                for (int k = 0; k < ev.Length; k++) Log.Add(ev[k]);
            }
            public int Count(SimEventType type, int a = int.MinValue, int b = int.MinValue)
            {
                int n = 0;
                foreach (var e in Log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
                return n;
            }
            public SimEvent First(SimEventType type)
            {
                foreach (var e in Log) if (e.Type == type) return e;
                Assert.Fail("no " + type + " event"); return default;
            }
            public void Dispose() => M.Dispose();
        }

        static UnitHandle H(SimWorld w, int slot) => new UnitHandle(slot, w.Generation[slot]);

        [Test]
        public void WithoutASapperSelectedThereAreNoButtons()
        {
            using var r = new Rig();
            int rifle = r.Man(InfantryArchetype.Rifle, Open + new float3(0f, 0f, -20f));
            r.Step();
            Assert.IsFalse(SapperOrders.Any(r.W, r.Sapper, new List<UnitHandle>()), "nothing selected: no row");
            Assert.IsFalse(SapperOrders.Any(r.W, r.Sapper, new[] { H(r.W, rifle) }), "a rifleman carries no charges: no row");
            var into = new List<SimCommand>();
            Assert.AreEqual(0, SapperOrders.Commands(r.W, r.Sapper, new[] { H(r.W, rifle) }, UnitAbilityId.LayMine, new Vector3(Open.x, 0f, Open.z), into));
            Assert.AreEqual(0, into.Count);
        }

        [Test]
        public void TheButtonSendsTheCommandTheSimConsumes()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -20f));
            r.Step();
            var sel = new[] { H(r.W, s) };
            Assert.IsTrue(SapperOrders.Any(r.W, r.Sapper, sel), "a sapper is selected: the row is there");
            Assert.AreEqual(r.Sapper.ChargesOf(r.W, s), SapperOrders.Charges(r.W, r.Sapper, sel), "his charges are what the row counts");

            var into = new List<SimCommand>();
            Assert.AreEqual(1, SapperOrders.Commands(r.W, r.Sapper, sel, UnitAbilityId.LayMine, new Vector3(Open.x, 1.3f, Open.z), into), "one command for the one sapper");
            Assert.AreEqual(1, into.Count);
            Assert.AreEqual(CommandType.UnitAbility, into[0].Type);
            Assert.AreEqual(s, into[0].A); Assert.AreEqual((int)UnitAbilityId.LayMine, into[0].B & 0xFF);
            Assert.AreEqual(0, into[0].B >> 8, "a mine takes no args");
            Assert.AreEqual(0f, into[0].Pos.y, "the ground plane, whatever the click's height");

            r.Step(into[0]);
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected), "the sim takes the HUD's command unchanged");
            Assert.AreEqual(1, r.Count(SimEventType.SapperOrdered, s, (int)UnitAbilityId.LayMine));
            Assert.AreEqual(SapperSystem.Walking, r.Sapper.Phase[s]);

            into.Clear();
            Assert.AreEqual(0, SapperOrders.Commands(r.W, r.Sapper, sel, UnitAbilityId.LayMine, new Vector3(Open.x, 0f, Open.z), into), "an errand in hand: no second order");
        }

        [Test]
        public void TheTripwireButtonSendsItAcrossTheFrontAtTheDefaultLength()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -10f));
            r.Step();
            var into = new List<SimCommand>();
            Assert.AreEqual(1, SapperOrders.Commands(r.W, r.Sapper, new[] { H(r.W, s) }, UnitAbilityId.LayTripwire, new Vector3(Open.x, 0f, Open.z), into));
            Assert.AreEqual((int)UnitAbilityId.LayTripwire, into[0].B & 0xFF);
            AbilityArgs.Unpack(into[0].B >> 8, out int heading, out _, out int length);
            Assert.AreEqual(SapperOrders.WireHeading, heading, "across the front, as the Proving Ground lays it");
            Assert.AreEqual(SapperSystem.DefaultTripwireMetres, length);

            r.Step(into[0]);
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected));
            Assert.AreEqual(1, r.Count(SimEventType.SapperOrdered, s, (int)UnitAbilityId.LayTripwire));
            var ordered = r.First(SimEventType.SapperOrdered);
            Assert.AreEqual(SapperSystem.DefaultTripwireMetres, ordered.Dir.x, 0.01f, "dir = heading x length: the default along +X");
        }

        [Test]
        public void WithNoChargeLeftTheRowStaysAndTheButtonsAreDead()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -20f));
            r.Step();
            var sel = new[] { H(r.W, s) };
            // spend both charges without walking him anywhere: the system's own counter is what the row reads
            r.Sapper.Charges[s] = 0;
            Assert.IsTrue(SapperOrders.Any(r.W, r.Sapper, sel), "he is still a sapper: the row stays, so the player can see why");
            Assert.AreEqual(0, SapperOrders.Charges(r.W, r.Sapper, sel));
            var into = new List<SimCommand>();
            Assert.AreEqual(0, SapperOrders.Commands(r.W, r.Sapper, sel, UnitAbilityId.LayMine, new Vector3(Open.x, 0f, Open.z), into), "nothing left to lay: no command");

            var refs = BuildBare();
            HudView.BindSapperOrders(refs, any: true, charges: 0, over: false, armed: UnitAbilityId.None);
            Assert.IsTrue(refs.UnitOrders.ClassListContains("is-visible"), "the row is shown");
            Assert.IsFalse(refs.LayMine.enabledSelf); Assert.IsFalse(refs.LayTripwire.enabledSelf);
            Assert.AreEqual(HudText.NoCharges, refs.OrderNote.text);

            HudView.BindSapperOrders(refs, any: false, charges: 0, over: false, armed: UnitAbilityId.None);
            Assert.IsFalse(refs.UnitOrders.ClassListContains("is-visible"), "no sapper selected: no row");
        }

        /// <summary>A HudRefs with no UXML and no panel, as HudBindTests builds one.</summary>
        static HudRefs BuildBare()
        {
            var na = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(na, 0);
            var roster = na.ToArray(); na.Dispose();
            var root = new VisualElement { name = HudView.RootName };
            foreach (var n in HudView.RequiredNames)
            {
                if (n == HudView.RootName) continue;
                VisualElement e = n.StartsWith("order-lay") ? new Button { name = n }
                    : n.EndsWith("-value") || n.StartsWith("tooltip") || n == "hint" || n == "banner-text" ? new Label { name = n }
                    : n.StartsWith("speed-") && n != "speed-value" && n != "speed-bar" ? (VisualElement)new Button { name = n }
                    : new VisualElement { name = n };
                root.Add(e);
            }
            root.Q("unit-orders").Add(new Label { name = "order-note" });
            return HudView.Build(root, null, roster, new[] { 150, 120 });
        }
    }
}
