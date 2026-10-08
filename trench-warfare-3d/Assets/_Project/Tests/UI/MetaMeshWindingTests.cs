// Phase: B6 / docs/21 phase 6 (implemented) — [V1] the campaign meta meshes (map pins, the front-line ribbon, the
// sea plate, the Home Front plate) wind their triangles so the authored normal is the front face, and so the
// cameras that draw them (MetaCamera.Apply, Map and Orbit) look at that front, not the Cull Front outline pass.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Meta;

namespace TW.Tests
{
    public class MetaMeshWindingTests
    {
        static IEnumerable<Mesh> BuiltMeshes()
        {
            yield return ContinentMesh.Sea();
            yield return MetaMeshes.Cylinder(StrategicMapView.PinRadius, 0.25f, StrategicMapView.PinHeight, 10, "MapPin");
            yield return MetaMeshes.Cylinder(0.32f, 0.24f, 1.7f, 10, "HomeFrontChimney");
            yield return MetaMeshes.Ring(StrategicMapView.PinRadius * 1.8f, StrategicMapView.PinRadius * 2.1f, 32, "MapRing");
            yield return MetaMeshes.Box(new Vector3(HomeFrontDiorama.BlockSize + 6f, 0.6f, HomeFrontDiorama.BlockSize + 6f), new Vector3(0f, -0.3f, 0f), "HomeFrontPlate");
            var points = new List<Vector3> { new Vector3(0f, 0f, 0f), new Vector3(10f, 0f, 4f), new Vector3(14f, 0f, 12f) };
            yield return MetaMeshes.Ribbon(points, StrategicMapView.RibbonWidth, 0.18f, 0f, "FrontLine");
        }

        /// <summary>[V1] every triangle's winding (cross(b-a,c-a)) agrees with the normal the builder authored for it:
        /// before the fix the Box, Cylinder side and Ribbon quads had their winding backwards (cross gave the opposite
        /// sign to the declared normal), so the lit pass culled the front and the Cull Front outline pass drew instead.</summary>
        [Test]
        public void Every_Triangle_Of_Every_Meta_Mesh_Faces_Out()
        {
            foreach (var mesh in BuiltMeshes())
            {
                try
                {
                    var v = mesh.vertices; var n = mesh.normals; var t = mesh.triangles;
                    for (int i = 0; i + 2 < t.Length; i += 3)
                    {
                        int ia = t[i], ib = t[i + 1], ic = t[i + 2];
                        Vector3 cross = Vector3.Cross(v[ib] - v[ia], v[ic] - v[ia]);
                        Vector3 avgN = n[ia] + n[ib] + n[ic];
                        Assert.That(Vector3.Dot(cross, avgN), Is.GreaterThan(0f),
                            "[V1] " + mesh.name + " triangle " + (i / 3) + " winds away from its authored normal");
                    }
                }
                finally { Object.DestroyImmediate(mesh); }
            }
        }

        /// <summary>[V1] the Map and Orbit cameras (MetaCamera.Apply: Pitch/Yaw -> Euler rotation, looking along
        /// rot*forward) actually look at the front of an up-facing top triangle (plate, pins, ribbon), not its back.</summary>
        [Test]
        public void The_Meta_Cameras_See_The_Front_Of_What_They_Draw()
        {
            float[] pitches = { MetaCamera.PitchMin, MetaCamera.OrbitPitch, MetaCamera.MapPitch, MetaCamera.PitchMax };
            float[] yaws = { 0f, 90f, 180f, 270f };
            int checkedTriangles = 0;
            foreach (var mesh in BuiltMeshes())
            {
                try
                {
                    var v = mesh.vertices; var n = mesh.normals; var t = mesh.triangles;
                    for (int i = 0; i + 2 < t.Length; i += 3)
                    {
                        int ia = t[i], ib = t[i + 1], ic = t[i + 2];
                        Vector3 avgN = (n[ia] + n[ib] + n[ic]).normalized;
                        if (avgN.y <= 0.5f) continue;
                        checkedTriangles++;
                        foreach (var pitch in pitches)
                            foreach (var yaw in yaws)
                            {
                                var rot = Quaternion.Euler(pitch, yaw, 0f);
                                Vector3 forward = rot * Vector3.forward;
                                Assert.That(Vector3.Dot(forward, avgN), Is.LessThan(0f),
                                    "[V1] " + mesh.name + " pitch " + pitch + " yaw " + yaw + ": the camera looks at the back of an up-facing triangle");
                            }
                    }
                }
                finally { Object.DestroyImmediate(mesh); }
            }
            Assert.That(checkedTriangles, Is.GreaterThan(0), "at least one up-facing triangle across the built meshes");
        }
    }
}
