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
        void TrenchKit(MapData map, BattlefieldSurface surface)
        {
            var floors = new HashSet<int>();
            foreach (var edge in surface.Edges)
            {
                var center = edge.Center; var outward = edge.Outward; var rotation = edge.Rotation;
                var inside = center - outward * .95f;
                float floor = map.Height.Sample(inside.x, inside.z);
                int variant = Mathf.Min(2, (int)(Rand(edge.Key, 802) * 3f));
                if (floors.Add(edge.Cell)) emit(kit.TrenchFloors[variant], Matrix4x4.TRS(new Vector3(inside.x, floor + .035f, inside.z), rotation * Quaternion.Euler(0f, edge.Link ? 0f : (Rand(edge.Key, 803) - .5f) * 7f, 0f), Vector3.one));
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
                emit(kit.TrenchWalls[variant], Matrix4x4.TRS(new Vector3(wall.x, floor, wall.z), rotation * Quaternion.Euler(-4f - Rand(edge.Key, 73) * 3f, 0f, 0f), new Vector3(edge.DressLength / 2f, height, 1f)));
                // the trench is lived in (small kit, close camera only): rifles stood against the wall, tins on a nail, a
                // bucket on the boards, and the telephone wire stapled along the revetment in sagging lengths
                var along = rotation * Vector3.right;
                float depth = Mathf.Max(.8f, upper - floor), life = Rand(edge.Key, 811);
                if (life < .10f) emit(kit.leanRifle, Matrix4x4.TRS(new Vector3(wall.x, floor + .04f, wall.z) - outward * .30f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation * Quaternion.Euler(13f, 0f, 0f), Vector3.one));
                else if (life < .16f) emit(kit.hangingTins, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .72f, wall.z) - outward * .16f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), rotation, Vector3.one));
                else if (life < .21f) emit(kit.bucket, Matrix4x4.TRS(new Vector3(wall.x, floor + .05f, wall.z) - outward * .45f + along * ((Rand(edge.Key, 812) - .5f) * edge.DressLength * .5f), Quaternion.Euler(0f, Rand(edge.Key, 813) * 360f, 0f), Vector3.one));
                if (Rand(edge.Key, 814) < .62f) emit(kit.phoneWire, Matrix4x4.TRS(new Vector3(wall.x, floor + depth * .84f, wall.z) - outward * .13f, Quaternion.LookRotation(outward) , new Vector3(edge.DressLength / 2f, 1f, 1f)));
                float frontage = Mathf.PerlinNoise(center.x * .09f + 19f, center.z * .09f + 7f);
                if (frontage > .26f)
                {
                    int course = frontage > .62f ? 0 : frontage > .43f ? 1 : 2;
                    if (Rand(edge.Key, 808) < .12f)
                        // a length held up with gabions instead: two wicker baskets of earth on the lip
                        for (int g = -1; g <= 1; g += 2)
                        {
                            var basket = lip + along * (g * edge.DressLength * .26f) + outward * .08f;
                            emit(kit.gabion, Matrix4x4.TRS(new Vector3(basket.x, surface.VisualHeight(basket.x, basket.z) - .12f, basket.z),
                                Quaternion.Euler((Rand(edge.Key + g, 809) - .5f) * 6f, Rand(edge.Key + g, 810) * 360f, 0f), Vector3.one * (.76f + Rand(edge.Key + g, 818) * .12f)));
                        }
                    else emit(kit.TrenchBags[course], Matrix4x4.TRS(new Vector3(lip.x, upper + .025f, lip.z), rotation * Quaternion.Euler((Rand(edge.Key, 805) - .5f) * 4f, Rand(edge.Key, 75) * 6f - 3f, (Rand(edge.Key, 806) - .5f) * 3f), new Vector3(edge.DressLength / 2f, .88f + Rand(edge.Key, 807) * .20f, 1.05f)));
                }
            }
        }
    }
}
