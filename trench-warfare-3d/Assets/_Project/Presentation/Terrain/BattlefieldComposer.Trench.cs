// Phase: B2 (implemented) — part of BattlefieldComposer: the trench lining. TrenchKit dresses every trench edge the
// surface found with walls, sandbag courses, duckboards, ladders and the small kit of a manned line.
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
    {
        /// <summary>Which trench cells already have a floor; a cleared field, not a fresh set per recomposition (review call B4).</summary>
        readonly HashSet<int> floors = new HashSet<int>();

        void TrenchKit(MapData map, BattlefieldSurface surface)
        {
            floors.Clear();
            // Hand-made (BattlefieldKit.Handmade, the default): each piece picks its own variant, and every turn and lean is
            // wider, with now and then a length that was badly built. Every number is a hash of the edge's key alone, as
            // before, so a piece stands where it stood after every recomposition and PropDestruction keeps its damage.
            bool rough = kit.Handmade;
            foreach (var edge in surface.Edges)
            {
                var center = edge.Center; var outward = edge.Outward; var rotation = edge.Rotation;
                var inside = center - outward * .95f;
                float floor = map.Height.Sample(inside.x, inside.z);
                int variant = Mathf.Min(2, (int)(Rand(edge.Key, 802) * 3f));
                int floorVariant = rough ? Mathf.Min(2, (int)(Rand(edge.Key, 830) * 3f)) : variant;
                if (floors.Add(edge.Cell))
                {
                    var lay = rotation * Quaternion.Euler(0f, edge.Link ? 0f : (Rand(edge.Key, 803) - .5f) * 7f, 0f);
                    float sunk = 0f;
                    if (rough)
                    {
                        // laid on mud by men who could not see it: turned, tipped, some sunk, one in fourteen with an end up
                        bool tipped = Rand(edge.Key, 838) < .07f;
                        lay = rotation * Quaternion.Euler(tipped ? 4f + Rand(edge.Key, 839) * 3f : (Rand(edge.Key, 836) - .5f) * 3f,
                            (Rand(edge.Key, 803) - .5f) * (edge.Link ? 4f : 12f), (Rand(edge.Key, 837) - .5f) * 4f);
                        sunk = Rand(edge.Key, 840) * .05f;
                        if (Rand(edge.Key, 845) < .5f) lay *= Quaternion.Euler(0f, 180f, 0f);   // laid the other way round
                    }
                    emit(kit.TrenchFloors[floorVariant], Matrix4x4.TRS(new Vector3(inside.x, floor + .035f - sunk, inside.z), lay, Vector3.one));
                }
                if (edge.Link)
                {
                    emit(kit.ladder, Matrix4x4.TRS(new Vector3(center.x, floor, center.z) - outward * .35f, rotation, Vector3.one));
                    if (Rand(edge.Key, 815) < .35f)
                    {
                        // a board by the way out says where it leads
                        var post = center + outward * 1.1f + (rotation * Vector3.right) * (Rand(edge.Key, 816) < .5f ? 1.0f : -1.0f);
                        emit(kit.signBoard, Matrix4x4.TRS(new Vector3(post.x, surface.VisualHeight(post.x, post.z) - .05f, post.z), rotation * Quaternion.Euler(0f, 180f + (Rand(edge.Key, 817) - .5f) * 30f, 0f), Vector3.one));
                    }
                    continue;
                }
                center = edge.DressCenter; outward = edge.DressOutward; rotation = Quaternion.LookRotation(outward);
                var wall = center - outward * .08f;
                var lip = center + outward * (.60f + (Rand(edge.Key, 804) - .5f) * .12f);
                float upper = surface.VisualHeight(lip.x, lip.z);
                float height = Mathf.Clamp((upper - floor) / 2f, .55f, 1.3f);
                var lean = Quaternion.Euler(-4f - Rand(edge.Key, 73) * 3f, 0f, 0f);
                if (rough)
                {
                    // the earth pushes a revetment in: most lean a little, one in sixteen bellies out badly; none is plumb,
                    // and a short one leaves raw earth above it
                    bool bellied = Rand(edge.Key, 834) < .06f;
                    lean = Quaternion.Euler(bellied ? -11f - Rand(edge.Key, 73) * 4f : -2.5f - Rand(edge.Key, 73) * 6.5f, (Rand(edge.Key, 833) - .5f) * 4f, (Rand(edge.Key, 832) - .5f) * 5f);
                    height = Mathf.Clamp(height * (.9f + Rand(edge.Key, 835) * .16f), .55f, 1.3f);
                }
                emit(kit.TrenchWalls[variant], Matrix4x4.TRS(new Vector3(wall.x, floor, wall.z), rotation * lean, new Vector3(edge.DressLength / 2f, height, 1f)));
                // the trench is lived in (small kit, close camera only): rifles stood against the wall, tins on a nail, a
                // bucket on the boards, and the telephone wire stapled along the revetment in sagging lengths
                var along = rotation * Vector3.right;
                float depth = Mathf.Max(.8f, upper - floor), life = Rand(edge.Key, 811);
                if (life < .10f) emit(kit.leanRifle, Matrix4x4.TRS(new Vector3(wall.x, floor + .04f, wall.z) - outward * .30f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation * Quaternion.Euler(13f, 0f, 0f), Vector3.one));
                else if (life < .16f) emit(kit.hangingTins, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .72f, wall.z) - outward * .16f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation, Vector3.one));
                else if (life < .21f) emit(kit.bucket, Matrix4x4.TRS(new Vector3(wall.x, floor + .05f, wall.z) - outward * .45f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), Quaternion.Euler(0f, Rand(edge.Key, 813) * 360f, 0f), Vector3.one));
                if (Rand(edge.Key, 814) < .62f) emit(kit.phoneWire, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .84f, wall.z) - outward * .13f, Quaternion.LookRotation(outward) , new Vector3(edge.DressLength / 2f, 1f, 1f)));
                float frontage = Mathf.PerlinNoise(center.x * .09f + 19f, center.z * .09f + 7f);
                // hand-made: one length in twenty was blown out and never rebuilt
                if (frontage > .26f && !(rough && Rand(edge.Key, 844) < .05f))
                {
                    int course = frontage > .62f ? 0 : frontage > .43f ? 1 : 2;
                    // hand-made: a third of the lengths were built higher or lower than their neighbours
                    if (rough && Rand(edge.Key, 831) < .34f) course = Mathf.Clamp(course + (Rand(edge.Key, 841) < .5f ? -1 : 1), 0, 2);
                    if (Rand(edge.Key, 808) < .12f)
                        // a length held up with gabions instead: two wicker baskets of earth on the lip
                        for (int g = -1; g <= 1; g += 2)
                        {
                            var basket = lip + along * (g * edge.DressLength * .26f) + outward * .08f;
                            emit(kit.gabion, Matrix4x4.TRS(new Vector3(basket.x, surface.VisualHeight(basket.x, basket.z) - .12f, basket.z),
                                Quaternion.Euler((Rand(edge.Key + g, 809) - .5f) * 6f, Rand(edge.Key + g, 810) * 360f, 0f), Vector3.one * (.76f + Rand(edge.Key + g, 818) * .12f)));
                        }
                    else if (rough)
                    {
                        // slid along the lip, so lengths overlap or leave a gap, and turned more than a line would allow
                        var at = lip + along * ((Rand(edge.Key, 843) - .5f) * .5f);
                        emit(kit.TrenchBags[course], Matrix4x4.TRS(new Vector3(at.x, surface.VisualHeight(at.x, at.z) + .01f, at.z), rotation * Quaternion.Euler((Rand(edge.Key, 805) - .5f) * 4.5f, Rand(edge.Key, 75) * 20f - 10f, (Rand(edge.Key, 806) - .5f) * 3.5f), new Vector3(edge.DressLength / 2f, .86f + Rand(edge.Key, 807) * .20f, 1f + Rand(edge.Key, 842) * .1f)));
                    }
                    else emit(kit.TrenchBags[course], Matrix4x4.TRS(new Vector3(lip.x, upper + .025f, lip.z), rotation * Quaternion.Euler((Rand(edge.Key, 805) - .5f) * 4f, Rand(edge.Key, 75) * 6f - 3f, (Rand(edge.Key, 806) - .5f) * 3f), new Vector3(edge.DressLength / 2f, .88f + Rand(edge.Key, 807) * .20f, 1.05f)));
                }
            }
        }
    }
}
