// Riders prototype: the seats RiderSeats reads off each walker's shell, and each seat's way up. Asserts what matters
// on screen: a man sits ON the deck (not in the flanks or the air), two men never overlap, every seat has a way up
// that leaves the body, a man is never seated under a traversing barrel when the gun rule is on, and a bigger crab
// seats at least as many men.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class RiderSeatTests
    {
        // measured 2026-09-25 at VehicleSize.Walker with ClearOfGuns: Pincer 3 (its two turrets sweep most of the deck at
        // head height; 15 without the rule), Kettle 2 (a dome round a mortar), Censer 15, Pavise 16, Banner 0 (the whole
        // machine is 2.2 m high), Redoubt 14.
        static readonly (string name, byte archetype, int atLeast)[] Walkers =
        {
            ("Pincer", VehicleArchetype.Pincer, 2), ("Kettle", VehicleArchetype.Kettle, 1),
            ("Censer", VehicleArchetype.Censer, 8), ("Pavise", VehicleArchetype.Pavise, 8),
            ("Banner", VehicleArchetype.Banner, 0), ("Redoubt", VehicleArchetype.Redoubt, 8),
        };

        bool rule;
        [SetUp] public void Keep() => rule = RiderSeats.ClearOfGuns;
        [TearDown] public void Restore() => RiderSeats.ClearOfGuns = rule;

        static TankModel Load(string name, byte archetype, float scale, string root = "Body")
        {
            var m = TankModel.Load(name, archetype, root, scale);
            Assert.NotNull(m, $"{name} did not load");
            return m;
        }

        // measured 2026-09-27 at VehicleSize.Tank: the Maw a squad of 8 on its rear deck (19 over two levels of its roof
        // read as a heap: critic t2), the Tusk none either way (its only flat deck is the rim beside the turret)
        [Test]
        public void The_Tanks_Seat_A_Squad_On_One_Level_Each_With_A_Way_Up_Over_The_Tracks()
        {
            RiderSeats.ClearOfGuns = true;
            var maw = RiderSeats.For(Load("Maw", VehicleArchetype.Maw, VehicleSize.Tank, "Hull"));
            Assert.AreEqual(RiderSeats.CapFor(VehicleArchetype.Maw), maw.Seats.Count, "Maw seats: a full squad, no more");
            float lo = float.MaxValue, hi = float.MinValue;
            foreach (var s in maw.Seats) { lo = Mathf.Min(lo, s.Local.y); hi = Mathf.Max(hi, s.Local.y); }
            Assert.LessOrEqual(hi - lo, RiderSeats.TankTier * 2f + 0.01f, "the Maw's men on one level");
            Assert.AreEqual(0, maw.NoWay, "a Maw seat with no way up");
            foreach (var s in maw.Seats)
                Assert.Greater(new Vector2(s.Foot.x, s.Foot.z).magnitude, new Vector2(s.Edge.x, s.Edge.z).magnitude - 0.01f, "a climber's foot inside the hull's edge");
            RiderSeats.ClearOfGuns = false;
            Assert.AreEqual(0, RiderSeats.For(Load("Tusk", VehicleArchetype.Tusk, VehicleSize.Tank, "Hull")).Seats.Count, "Tusk seats without the gun rule");
        }

        [Test]
        public void A_Barrel_Turned_Onto_A_Seat_Makes_Its_Man_Duck_And_Turned_Away_Does_Not()
        {
            RiderSeats.ClearOfGuns = false;   // men under the guns: the case ducking is for
            var s = RiderSeats.For(Load("Pincer", VehicleArchetype.Pincer, VehicleSize.Walker));
            Assert.AreEqual(2, s.Guns.Count, "the Pincer's two turrets");
            int under = 0;
            for (int g = 0; g < s.Guns.Count; g++)
            {
                var gun = s.Guns[g];
                Assert.Greater(gun.Reach, 0.5f, $"gun {g} reach");
                foreach (var seat in s.Seats)
                {
                    float a = Mathf.Atan2(seat.Local.x - gun.Pivot.x, seat.Local.z - gun.Pivot.y);
                    float onto = gun.Yaw0 + (a - gun.Bearing0);
                    if (!s.UnderBarrel(seat.Local, g, onto)) continue;   // out of its reach, or below the barrel
                    under++;
                    Assert.IsFalse(s.UnderBarrel(seat.Local, g, onto + Mathf.PI), $"gun {g} turned away still ducks a man");
                }
            }
            Assert.Greater(under, 0, "no seat on the Pincer is ever under a barrel: the ducking would never fire");
        }

        [Test]
        public void Every_Walker_Has_Seats_On_Its_Deck_And_No_Two_Men_Overlap()
        {
            RiderSeats.ClearOfGuns = true;
            foreach (var (name, archetype, atLeast) in Walkers)
            {
                var m = Load(name, archetype, VehicleSize.Walker);
                var s = RiderSeats.For(m);
                Assert.GreaterOrEqual(s.Seats.Count, atLeast, $"{name}: seats");
                var body = m.Lods[0].Parts[s.BodyPart].Mesh.bounds;
                for (int i = 0; i < s.Seats.Count; i++)
                {
                    var a = s.Seats[i];
                    Assert.GreaterOrEqual(a.Local.y, s.DeckFloor - 1e-3f, $"{name} seat {i} is down the flank");
                    // a seat may be on something carried on the body (a drum, a shield rim)
                    Assert.LessOrEqual(a.Local.y, body.max.y + 2.5f, $"{name} seat {i} floats above the shell");
                    Assert.GreaterOrEqual(a.Flat, RiderSeats.MinFlat, $"{name} seat {i} is on a slope");
                    for (int j = 0; j < i; j++)
                    {
                        var b = s.Seats[j];
                        float d = new Vector2(a.Local.x - b.Local.x, a.Local.z - b.Local.z).magnitude;
                        Assert.GreaterOrEqual(d, RiderSeats.Spacing - 1e-3f, $"{name} seats {i} and {j} overlap ({d:0.00} m)");
                    }
                }
            }
        }

        [Test]
        public void Every_Seat_Has_A_Way_Up_That_Leaves_The_Body_And_Does_Not_Point_Forward()
        {
            RiderSeats.ClearOfGuns = false;   // the most seats, so the most ways up to check
            foreach (var (name, archetype, _) in Walkers)
            {
                var s = RiderSeats.For(Load(name, archetype, VehicleSize.Walker));
                for (int i = 0; i < s.Seats.Count; i++)
                {
                    var a = s.Seats[i];
                    Assert.AreEqual(1f, a.Out.magnitude, 1e-3f, $"{name} seat {i}: the way out is not a unit direction");
                    Assert.LessOrEqual(a.Out.z, 0.5f, $"{name} seat {i} climbs off over the claws");
                    var toEdge = new Vector2(a.Edge.x - a.Local.x, a.Edge.z - a.Local.z);
                    var toFoot = new Vector2(a.Foot.x - a.Local.x, a.Foot.z - a.Local.z);
                    Assert.Greater(toFoot.magnitude, toEdge.magnitude + 0.3f, $"{name} seat {i}: the foot is not beyond the edge");
                    Assert.GreaterOrEqual(a.Edge.y, s.DeckFloor - 0.45f, $"{name} seat {i}: the edge is below the deck");
                }
            }
        }

        [Test]
        public void The_Gun_Rule_Only_Ever_Takes_Seats_Away()
        {
            RiderSeats.ClearOfGuns = false;
            var all = RiderSeats.For(Load("Pincer", VehicleArchetype.Pincer, VehicleSize.Walker));
            RiderSeats.ClearOfGuns = true;
            var clear = RiderSeats.For(Load("Pincer", VehicleArchetype.Pincer, VehicleSize.Walker));
            Assert.Greater(all.Seats.Count, clear.Seats.Count, "the Pincer's guns sweep its deck: the rule must remove seats");
            Assert.Greater(clear.UnderGun, 0);
        }

        [Test]
        public void Seats_Are_Handed_Out_Spread_Over_The_Deck()
        {
            // the first four of a big deck must not be a clump: each at least two man-widths from the others
            RiderSeats.ClearOfGuns = true;
            var s = RiderSeats.For(Load("Pavise", VehicleArchetype.Pavise, VehicleSize.Walker));
            Assert.GreaterOrEqual(s.Seats.Count, 4);
            for (int i = 1; i < 4; i++)
                for (int j = 0; j < i; j++)
                    Assert.Greater(new Vector2(s.Seats[i].Local.x - s.Seats[j].Local.x, s.Seats[i].Local.z - s.Seats[j].Local.z).magnitude, RiderSeats.Spacing * 1.9f, $"seats {i} and {j} are side by side");
        }

        [Test]
        public void A_Bigger_Crab_Seats_At_Least_As_Many_Men()
        {
            RiderSeats.ClearOfGuns = false;
            var small = RiderSeats.For(Load("Pincer", VehicleArchetype.Pincer, VehicleSize.Walker));
            var big = RiderSeats.For(Load("Pincer", VehicleArchetype.Pincer, VehicleSize.Walker * 1.5f));
            Assert.GreaterOrEqual(big.Seats.Count, small.Seats.Count);
            Assert.Greater(big.DeckTop, small.DeckTop * 1.4f, "the bigger model's deck is not higher: the size did not take");
        }
    }
}
