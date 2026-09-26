// Phase: B2 (implemented) — part of BattlefieldComposer: the imported landmarks placed sparingly, and the horizon
// beyond the map's edge.
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
    {
        /// <summary>
        /// The imported landmarks, sparingly (owner, 2026-09-22: "some can be used sparingly like the turrets"): one MG nest
        /// on the enemy-facing parapet of each fire trench, two field guns and an observation stand in each side's rear
        /// corners, one well behind a line, a crashed biplane just beyond the far edge of no man's land, and duds left in
        /// a few dry shell holes. Decoration only, like the site blueprints: none of it is cover or an obstacle, so the
        /// big pieces keep to ground the men do not cross (the parapet between ladders, the rear corners off the road,
        /// outside the map) and every one is dropped rather than moved when the ground under it is no longer fit.
        /// </summary>
        void Landmarks(MapData map, BattlefieldSurface surface)
        {
            float W = map.SizeMeters.x, L = map.SizeMeters.y;
            var taken = new List<Vector3>();
            foreach (var site in sites) taken.Add(site.Position);
            foreach (var h in hamlets) taken.Add(h.Centre);
            bool Free(Vector3 p, float spacing) { foreach (var t in taken) if ((new Vector2(t.x - p.x, t.z - p.z)).sqrMagnitude < spacing * spacing) return false; return true; }
            void Put(BattlefieldKit.Module module, Vector3 p, Quaternion rotation, float size, float sink)
                => emit(module, Matrix4x4.TRS(new Vector3(p.x, Ground(map, surface, p.x, p.z) - sink, p.z), rotation, Vector3.one * size));
            // half the drawn footprint of a kind: its look's size (Module.Size) spaces what stands round it
            Vector3 Half(BattlefieldKit.Module module) => Vector3.Scale(module.Mesh.bounds.extents, module.Size);

            // MG nests: on the lip of the parapet that faces the enemy, the gun out over no man's land, never by a ladder
            var ladders = new List<Vector3>();
            foreach (var edge in surface.Edges) if (edge.Link) ladders.Add(edge.Center);
            var candidates = new List<BattlefieldSurface.Edge>(surface.Edges);
            candidates.Sort((a, b) => { int order = Rand(a.Key, 850).CompareTo(Rand(b.Key, 850)); return order != 0 ? order : a.Key.CompareTo(b.Key); });
            var nests = new int[map.Trenches.Length];
            var nest = kit.mgNest.Size;
            foreach (var edge in candidates)
            {
                var trench = map.Trenches[edge.Trench];
                if (edge.Link || trench.Kind != 0 || nests[edge.Trench] > 0) continue;
                var facing = new Vector3(Mathf.Sin(trench.FacingYaw), 0f, Mathf.Cos(trench.FacingYaw));
                if (Vector3.Dot(edge.DressOutward, facing) < .92f || edge.DressCenter.x < 10f || edge.DressCenter.x > W - 10f) continue;
                bool byLadder = false;
                foreach (var ladder in ladders) if ((ladder - edge.Center).sqrMagnitude < 36f) { byLadder = true; break; }
                if (byLadder) continue;
                var at = edge.DressCenter + edge.DressOutward * (.66f + 1.29f * nest.z);   // its back on the lip, however deep its look draws it
                var rotation = Quaternion.LookRotation(edge.DressOutward) * Quaternion.Euler(0f, (Rand(edge.Key, 851) - .5f) * 10f, 0f);
                if (!Free(at, Mathf.Max(9f, 3.5f * nest.z)) || !Room(map, surface, at, rotation, .95f * nest.x, 1.3f * nest.z, false, 1.0f * nest.z)) continue;   // it hangs over the parapet's fall: a metre of fall for each time its own size it is drawn
                nests[edge.Trench]++; taken.Add(at);
                Put(kit.mgNest, at, rotation, 1f, .06f);
                Put(kit.sandbag, at + rotation * Vector3.Scale(new Vector3(-1.05f, 0f, 1.25f), nest), rotation * Quaternion.Euler(0f, 70f + Rand(edge.Key, 852) * 30f, 0f), 1f, .04f);
                Put(kit.sandbag, at + rotation * Vector3.Scale(new Vector3(1.1f, 0f, 1.1f), nest), rotation * Quaternion.Euler(0f, -80f - Rand(edge.Key, 853) * 30f, 0f), .95f, .04f);
            }

            // each side's rear, the owner's way (hand placement, 2026-09-22): a field gun on either flank pulled back to the rear
            // edge (its trail may run off the map) and laid toward the enemy, its shells and a sack beside it and one limber
            // behind; and four observation stands, two along the rear edge, half off the map, and two out past the far edge
            // where the fog lies (the side the standard view looks toward). None stands out in the open between the lines.
            // Behind one side, the well of a farm that is no longer there.
            var gun = Half(kit.fieldGun); var stand = Half(kit.armouredStand);
            for (int side = 0; side < 2; side++)
            {
                float ahead = side == 0 ? 1f : -1f, rear = side == 0 ? 0f : L;
                var toward = Quaternion.LookRotation(new Vector3(0f, 0f, ahead));
                for (int flank = 0; flank < 2; flank++)
                    for (int attempt = 0; attempt < 10; attempt++)
                    {
                        int key = side * 64 + flank * 16 + attempt;
                        var at = new Vector3(W * (flank == 0 ? .06f : .84f) + Rand(key, 860) * W * .10f, 0f, rear + ahead * (2.5f + Rand(key, 861) * 6.5f));
                        var rotation = toward * Quaternion.Euler(0f, (Rand(key, 862) - .5f) * 24f, 0f);
                        if (!Free(at, Mathf.Max(7f, gun.z)) || !Room(map, surface, at, rotation, gun.x, gun.z, true)) continue;
                        taken.Add(at);
                        float outer = flank == 0 ? 1f : -1f;
                        Put(kit.fieldGun, at, rotation, 1f, .05f);
                        Put(kit.shellStack, at + rotation * new Vector3(outer * (gun.x + .9f), 0f, -.35f * gun.z), rotation * Quaternion.Euler(0f, 90f + (Rand(key, 863) - .5f) * 30f, 0f), .9f, .04f);
                        Put(kit.sandbag, at + rotation * new Vector3(-outer * (gun.x + .5f), 0f, .3f * gun.z), rotation * Quaternion.Euler(0f, Rand(key, 864) * 180f, 0f), 1f, .04f);
                        if (flank == side) Put(kit.limber, at + rotation * new Vector3(-outer * (gun.x * .5f + .4f), 0f, -(gun.z + 1.8f)), Quaternion.Euler(0f, Rand(key, 865) * 360f, (Rand(key, 866) - .5f) * 10f), .95f, .08f);
                        break;
                    }
                for (int attempt = 0, stands = 0; attempt < 24 && stands < 4; attempt++)
                {
                    int key = side * 64 + 40 + attempt;
                    var at = stands < 2
                        ? new Vector3(W * (.22f + Rand(key, 867) * .72f), 0f, rear + ahead * (-3f + Rand(key, 868) * 7f))
                        : new Vector3(-3f - Rand(key, 867) * 7f, 0f, rear + ahead * (4f + Rand(key, 868) * L * .28f));
                    var rotation = toward * Quaternion.Euler(0f, 45f + (Rand(key, 869) - .5f) * 20f, 0f);
                    if (!Free(at, 14f) || !Room(map, surface, at, rotation, stand.x, stand.z, true)) continue;
                    taken.Add(at); Put(kit.armouredStand, at, rotation, 1f, .05f); stands++;
                }
                if (side == (int)(Rand(seed, 870) * 2f))
                    for (int attempt = 0; attempt < 6; attempt++)
                    {
                        int key = side * 64 + 50 + attempt;
                        var at = new Vector3(W * (.62f + Rand(key, 871) * .18f), 0f, rear + ahead * (4.5f + Rand(key, 872) * 4f));
                        var rotation = Quaternion.Euler(0f, Rand(key, 873) * 360f, 0f);
                        if (!Free(at, 7f) || !Room(map, surface, at, rotation, 1.1f, 1.1f)) continue;
                        taken.Add(at); Put(kit.well, at, rotation, 1f, .08f); break;
                    }
            }

            // a biplane that came down nose-first just beyond the far edge of no man's land (the side the standard view
            // looks toward), where it is seen behind the fighting and never stood in
            {
                float x = -6.5f - Rand(seed, 874) * 5f, z = L * .5f + (Rand(seed, 875) - .5f) * L * .22f;
                var rotation = Quaternion.Euler(0f, 90f + (Rand(seed, 876) - .5f) * 70f, 0f) * Quaternion.Euler(20f + Rand(seed, 877) * 12f, 0f, (Rand(seed, 878) - .5f) * 30f);
                emit(kit.biplane, Matrix4x4.TRS(new Vector3(x, GreyboxTerrainView.SkirtHeight(map, x, z) - .55f, z), rotation, Vector3.one));
            }

            // duds: a shell that did not go off, nose down in the bottom of a dry hole, or in the shallows at the edge of
            // a flooded one (the night look floods most of them), its fins out of the water
            for (int i = 0; i < surface.Hollows.Count; i++)
            {
                var h = surface.Hollows[i];
                bool flooded = h.Level > -100f;
                if (Rand(i, 880) > (flooded ? .14f : .30f) || h.Radius < 1.2f) continue;
                float angle = Rand(i, 881) * Mathf.PI * 2f, off = h.Radius * (flooded ? .72f + .14f * Rand(i, 882) : .3f * Rand(i, 882));
                float x = h.Center.x + Mathf.Cos(angle) * off, z = h.Center.y + Mathf.Sin(angle) * off;
                if (!Clear(map, x, z) || surface.At(x, z).Hollow < 0) continue;
                emit(kit.dudShell, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .22f, z), Quaternion.Euler((Rand(i, 883) - .5f) * 30f, Rand(i, 884) * 360f, (Rand(i, 885) - .5f) * 30f), Vector3.one * (.6f + Rand(i, 886) * .2f)));
            }
        }

        void Horizon(MapData map)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            // clumps, as on the field: a big tree, a couple of medium ones close by, small stuff round them
            for (int c = 0; c < 110; c++)
            {
                float cx = -260f + Rand(c, 1) * (w + 520f), cz = -200f + Rand(c, 2) * (l + 400f);
                int members = 3 + (int)(Rand(c, 3) * 6f);
                for (int i = 0; i < members; i++)
                {
                    int key = c * 16 + i;
                    float reach = i == 0 ? 0f : i < 3 ? 3f + Rand(key, 4) * 4f : 5f + Rand(key, 4) * 9f, angle = Rand(key, 5) * Mathf.PI * 2f;
                    float x = cx + Mathf.Cos(angle) * reach, z = cz + Mathf.Sin(angle) * reach;
                    float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
                    if (outside < 5f) continue;
                    float level = GreyboxTerrainView.SkirtHeight(map, x, z) - 0.05f;
                    if (Shore.UnderWater(map, x, z, level)) continue;   // no wood grows out of the sea
                    float s = i == 0 ? 1.3f + Rand(key, 6) * .5f : i < 3 ? .85f + Rand(key, 6) * .3f : .5f + Rand(key, 6) * .3f;
                    var m = Matrix4x4.TRS(new Vector3(x, level, z), Quaternion.Euler(0f, Rand(key, 7) * 360f, 0f), new Vector3(s, s, s));
                    if (i == 0) emit(Rand(key, 8) < .4f ? kit.fork : kit.trunk, m);
                    else if (i < 3) emit(Rand(key, 8) < .5f ? kit.trunk : kit.snag, m);
                    else emit(Rand(key, 8) < .45f ? kit.stump : Rand(key, 8) < .75f ? kit.bush : kit.snag, m);
                }
            }
            for (int i = 0; i < 9; i++)
            {
                bool farSide = i < 6;   // the standard view looks along -X, so most of them stand there
                float x = farSide ? -45f - Rand(i, 6) * 120f : w + 45f + Rand(i, 6) * 90f, z = Rand(i, 7) * l;
                float s = 1.2f + Rand(i, 8) * 1.0f;
                var turn = Quaternion.Euler(0f, 80f + Rand(i, 9) * 40f, 0f);
                float stands = GreyboxTerrainView.SkirtHeight(map, x, z);
                if (Shore.UnderWater(map, x, z, stands)) continue;   // and no village stands in it
                var at = new Vector3(x, stands - 0.25f, z);
                emit(kit.ruin, Matrix4x4.TRS(at, turn, new Vector3(s, s, s)));
                // what else is left of the place: wall stubs either side, a slab and a low wall in the rubble
                (Vector3 local, BattlefieldKit.Module module, float size)[] rubble =
                {
                    (new Vector3(-10.5f, 0f, 1.5f), kit.wallStub, 1.1f), (new Vector3(7.8f, 0f, -1.8f), kit.wallStub, .9f),
                    (new Vector3(2.4f, 0f, 3.2f), kit.rebarSlab, 1f), (new Vector3(-4.2f, 0f, -3.4f), kit.barricade, 1.1f),
                };
                for (int r = 0; r < rubble.Length; r++)
                {
                    if (Rand(i * 8 + r, 10) < .25f) continue;
                    emit(rubble[r].module, Matrix4x4.TRS(at + turn * (rubble[r].local * s) + new Vector3(0f, .1f, 0f), turn * Quaternion.Euler(0f, (Rand(i * 8 + r, 11) - .5f) * 50f, 0f), Vector3.one * rubble[r].size * s));
                }
            }
        }
    }
}
