// Phase: B3 (implemented; placeholder for the C2 rigs)
// A box soldier and its vertex animation texture, built in code so the B3 renderer can be driven before any rig
// exists. The output has exactly the layout VATBaker will produce from real clips: U = vertex index, V = frame, rows
// stacked in AnimRow order, RGBAHalf object-space positions and normals, plus a row table (start frame, length).
// Vertex colour: rgb = albedo, a = 1 where the team colour multiplies it (uniform, pack), 0 elsewhere.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Units
{
    public sealed class VatAsset
    {
        public Mesh Mesh;
        public Texture2D Positions, Normals;
        public Vector2[] RowTable;   // x = first frame, y = frame count, indexed by AnimRow
        public int TotalFrames;
    }

    public static class ProceduralSoldier
    {
        public const int FramesPerRow = 16;
        public const float HipHeight = 0.9f, Height = 1.78f;

        enum Part { Torso, Head, ArmL, ArmR, Rifle, ThighL, ShinL, ThighR, ShinR, Count }

        struct Box { public Part Part; public Vector3 Center, Size; public Color Color; }

        public struct SoldierPose
        {
            public Vector3 RootPos, RootEuler, Torso, Head, ArmL, ArmR, Rifle;
            public float ThighL, ShinL, ThighR, ShinR;
            public float ForeL, ForeR;   // elbow bend towards the rifle; only rigs with forearms use it (VATBaker)
        }

        // joint pivots in the parent's space; legs hang off the root, everything else off the torso
        static readonly Vector3[] Pivot =
        {
            new Vector3(0f, 0f, 0f), new Vector3(0f, 0.58f, 0f), new Vector3(-0.24f, 0.52f, 0f), new Vector3(0.24f, 0.52f, 0f),
            new Vector3(0.12f, 0.36f, 0.12f), new Vector3(-0.1f, 0f, 0f), new Vector3(0f, -0.45f, 0f), new Vector3(0.1f, 0f, 0f),
            new Vector3(0f, -0.45f, 0f),
        };

        static Box[] Boxes()
        {
            var cloth = new Color(1f, 1f, 1f, 1f);
            var steel = new Color(0.30f, 0.33f, 0.27f, 0f);   // not team tinted: the helmet is what reads from above
            var skin = new Color(0.78f, 0.60f, 0.47f, 0f);
            var wood = new Color(0.27f, 0.18f, 0.11f, 0f);
            var boot = new Color(0.13f, 0.11f, 0.10f, 0f);
            var webbing = new Color(0.80f, 0.80f, 0.74f, 1f);
            return new[]
            {
                new Box { Part = Part.Torso, Center = new Vector3(0f, 0.30f, 0f), Size = new Vector3(0.40f, 0.58f, 0.23f), Color = cloth },
                new Box { Part = Part.Torso, Center = new Vector3(0f, 0.20f, -0.15f), Size = new Vector3(0.30f, 0.32f, 0.12f), Color = webbing },   // pack
                new Box { Part = Part.Head, Center = new Vector3(0f, 0.11f, 0.01f), Size = new Vector3(0.19f, 0.22f, 0.21f), Color = skin },
                new Box { Part = Part.Head, Center = new Vector3(0f, 0.24f, 0f), Size = new Vector3(0.34f, 0.07f, 0.36f), Color = steel },
                new Box { Part = Part.ArmL, Center = new Vector3(0f, -0.26f, 0f), Size = new Vector3(0.11f, 0.56f, 0.11f), Color = cloth },
                new Box { Part = Part.ArmR, Center = new Vector3(0f, -0.26f, 0f), Size = new Vector3(0.11f, 0.56f, 0.11f), Color = cloth },
                new Box { Part = Part.Rifle, Center = new Vector3(0f, 0f, 0.22f), Size = new Vector3(0.05f, 0.08f, 1.15f), Color = wood },
                new Box { Part = Part.ThighL, Center = new Vector3(0f, -0.22f, 0f), Size = new Vector3(0.15f, 0.47f, 0.16f), Color = cloth },
                new Box { Part = Part.ShinL, Center = new Vector3(0f, -0.22f, 0.01f), Size = new Vector3(0.13f, 0.46f, 0.15f), Color = boot },
                new Box { Part = Part.ThighR, Center = new Vector3(0f, -0.22f, 0f), Size = new Vector3(0.15f, 0.47f, 0.16f), Color = cloth },
                new Box { Part = Part.ShinR, Center = new Vector3(0f, -0.22f, 0.01f), Size = new Vector3(0.13f, 0.46f, 0.15f), Color = boot },
            };
        }

        public static VatAsset Build()
        {
            var boxes = Boxes();
            int vertexCount = boxes.Length * 24;
            var restPos = new Vector3[vertexCount];
            var restNrm = new Vector3[vertexCount];
            var part = new Part[vertexCount];
            var colors = new Color[vertexCount];
            var tris = new int[boxes.Length * 36];
            Vector3[] axes = { Vector3.right, Vector3.left, Vector3.up, Vector3.down, Vector3.forward, Vector3.back };
            int v = 0, ti = 0;
            foreach (var b in boxes)
                foreach (var n in axes)
                {
                    Vector3 u = Mathf.Abs(n.y) > 0.5f ? Vector3.right : Vector3.up;
                    Vector3 w = Vector3.Cross(n, u);
                    Vector3 h = b.Size * 0.5f;
                    for (int k = 0; k < 4; k++)
                    {
                        float su = (k == 0 || k == 3) ? -1f : 1f, sw = k < 2 ? -1f : 1f;
                        restPos[v + k] = b.Center + Vector3.Scale(n + u * su + w * sw, h);
                        restNrm[v + k] = n;
                        part[v + k] = b.Part;
                        colors[v + k] = b.Color;
                    }
                    tris[ti++] = v; tris[ti++] = v + 1; tris[ti++] = v + 2; tris[ti++] = v; tris[ti++] = v + 2; tris[ti++] = v + 3;
                    v += 4;
                }

            int rows = (int)AnimRow.Count, total = rows * FramesPerRow;
            var posPixels = new Color[vertexCount * total];
            var nrmPixels = new Color[vertexCount * total];
            var table = new Vector2[rows];
            var m = new Matrix4x4[(int)Part.Count];
            for (int r = 0; r < rows; r++)
            {
                table[r] = new Vector2(r * FramesPerRow, FramesPerRow);
                for (int f = 0; f < FramesPerRow; f++)
                {
                    Solve(Sample((AnimRow)r, f / (float)FramesPerRow), m);
                    int line = (r * FramesPerRow + f) * vertexCount;
                    for (int i = 0; i < vertexCount; i++)
                    {
                        Vector3 p = m[(int)part[i]].MultiplyPoint3x4(restPos[i]);
                        Vector3 n = m[(int)part[i]].MultiplyVector(restNrm[i]);
                        posPixels[line + i] = new Color(p.x, p.y, p.z, 1f);
                        nrmPixels[line + i] = new Color(n.x, n.y, n.z, 0f);
                    }
                }
            }

            // the rest pose lives in the mesh too, so it still draws as a soldier under a non-VAT material; the winding
            // is checked against the normal because the face basis above flips handedness per axis
            for (int t = 0; t < tris.Length; t += 3)
            {
                Vector3 a = restPos[tris[t]], b = restPos[tris[t + 1]], c = restPos[tris[t + 2]];
                if (Vector3.Dot(Vector3.Cross(b - a, c - a), restNrm[tris[t]]) < 0f) { int s = tris[t + 1]; tris[t + 1] = tris[t + 2]; tris[t + 2] = s; }
            }
            Solve(Sample(AnimRow.Idle, 0f), m);
            var meshPos = new Vector3[vertexCount];
            for (int i = 0; i < vertexCount; i++) meshPos[i] = m[(int)part[i]].MultiplyPoint3x4(restPos[i]);
            var mesh = new Mesh { name = "ProceduralSoldier", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(meshPos);
            mesh.SetNormals(restNrm);
            mesh.SetColors(colors);
            mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(new Vector3(0f, 0.9f, 0f), new Vector3(2.4f, 2.4f, 2.4f));

            return new VatAsset
            {
                Mesh = mesh, RowTable = table, TotalFrames = total,
                Positions = Atlas("SoldierVatPositions", vertexCount, total, posPixels),
                Normals = Atlas("SoldierVatNormals", vertexCount, total, nrmPixels),
            };
        }

        static Texture2D Atlas(string name, int width, int height, Color[] pixels)
        {
            var t = new Texture2D(width, height, TextureFormat.RGBAHalf, false, true)
            { name = name, filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            t.SetPixels(pixels);
            t.Apply(false, true);
            return t;
        }

        static void Solve(in SoldierPose p, Matrix4x4[] m)
        {
            var root = Matrix4x4.TRS(p.RootPos, Quaternion.Euler(p.RootEuler), Vector3.one);
            var torso = root * Matrix4x4.Rotate(Quaternion.Euler(p.Torso));
            m[(int)Part.Torso] = torso;
            m[(int)Part.Head] = torso * Joint(Part.Head, p.Head);
            m[(int)Part.ArmL] = torso * Joint(Part.ArmL, p.ArmL);
            m[(int)Part.ArmR] = torso * Joint(Part.ArmR, p.ArmR);
            m[(int)Part.Rifle] = torso * Joint(Part.Rifle, p.Rifle);
            m[(int)Part.ThighL] = root * Joint(Part.ThighL, new Vector3(p.ThighL, 0f, 0f));
            m[(int)Part.ShinL] = m[(int)Part.ThighL] * Joint(Part.ShinL, new Vector3(p.ShinL, 0f, 0f));
            m[(int)Part.ThighR] = root * Joint(Part.ThighR, new Vector3(p.ThighR, 0f, 0f));
            m[(int)Part.ShinR] = m[(int)Part.ThighR] * Joint(Part.ShinR, new Vector3(p.ShinR, 0f, 0f));
        }

        static Matrix4x4 Joint(Part part, Vector3 euler) => Matrix4x4.TRS(Pivot[(int)part], Quaternion.Euler(euler), Vector3.one);

        // Positive X pitches forward: a torso leans towards +Z, a hanging limb swings back. Forward swing is negative.
        public static SoldierPose Sample(AnimRow row, float t)
        {
            float a = t * Mathf.PI * 2f, s = Mathf.Sin(a), c = Mathf.Cos(a);
            float kick = Mathf.Exp(-t * 9f);   // recoil at the start of a fire loop
            var p = new SoldierPose
            {
                RootPos = new Vector3(0f, HipHeight, 0f), ArmL = new Vector3(-62f, 0f, -28f), ArmR = new Vector3(-48f, 0f, 12f),
                Rifle = new Vector3(-18f, -22f, 0f), ForeL = 55f, ForeR = 45f,
            };
            switch (row)
            {
                case AnimRow.Idle:
                    p.Torso.x = 2f + s; p.RootPos.y -= 0.005f * (1f + s);
                    break;
                case AnimRow.Walk:
                    Stride(ref p, s, c, 28f, 0.025f); p.Torso.x = 6f;
                    break;
                case AnimRow.Sprint:
                    Stride(ref p, s, c, 50f, 0.06f); p.Torso.x = 20f; p.Head.x = -12f;
                    break;
                case AnimRow.CrouchWalk:
                    Stride(ref p, s, c, 20f, 0.02f);
                    p.RootPos.y = 0.62f; p.ThighL -= 48f; p.ThighR -= 48f; p.ShinL += 80f; p.ShinR += 80f; p.Torso.x = 28f; p.Head.x = -20f;
                    break;
                case AnimRow.ProneCrawl:
                    Prone(ref p);
                    p.ThighL = 12f * s; p.ThighR = -12f * s; p.ShinL = 25f + 25f * s; p.ShinR = 25f - 25f * s;
                    p.ArmL.x += 18f * s; p.ArmR.x -= 18f * s; p.RootEuler.z = 5f * s;
                    break;
                case AnimRow.FireStanding:
                case AnimRow.FireFireStep:
                    p.ThighL = -16f; p.ThighR = 14f; p.ShinL = 8f; p.ShinR = 6f;
                    p.RootPos.y -= 0.03f;
                    p.Torso = new Vector3((row == AnimRow.FireFireStep ? 12f : 4f) - 4f * kick, 18f, 0f);
                    p.Head = new Vector3(4f, -14f, 0f);
                    p.ArmL = new Vector3(-84f, 0f, -34f); p.ArmR = new Vector3(-70f, 0f, 22f);
                    p.Rifle = new Vector3(-3f - 6f * kick, -18f, 0f);
                    break;
                case AnimRow.FireProne:
                    Prone(ref p); p.RootPos.z -= 0.03f * kick; p.Rifle.x -= 4f * kick;
                    break;
                case AnimRow.Throw:
                    p.Torso = new Vector3(-10f + 30f * t, -30f + 60f * t, 0f);
                    p.ArmR = new Vector3(-200f + 170f * t, 0f, 10f);
                    p.ThighL = -20f; p.ThighR = 16f;
                    break;
                case AnimRow.Vault:
                    float up = Mathf.Sin(t * Mathf.PI);
                    p.RootPos.y += 0.35f * up; p.Torso.x = 30f * up;
                    p.ThighL = -70f * up; p.ShinL = 80f * up; p.ThighR = 20f * up; p.ShinR = 30f * up;
                    break;
                case AnimRow.Flinch0:
                case AnimRow.Flinch1:
                case AnimRow.Flinch2:
                    float duck = Mathf.Sin(t * Mathf.PI);
                    p.RootPos.y -= 0.18f * duck; p.Torso.x = 34f * duck; p.Head.x = 20f * duck;
                    p.Torso.y = (row - AnimRow.Flinch1) * 25f * duck;
                    p.ThighL = p.ThighR = -30f * duck; p.ShinL = p.ShinR = 55f * duck;
                    break;
                case AnimRow.Death0:
                case AnimRow.Death1:
                case AnimRow.Death2:
                case AnimRow.Death3:
                    float fall = Mathf.SmoothStep(0f, 1f, Mathf.Min(1f, t * 1.6f));
                    bool back = row == AnimRow.Death1 || row == AnimRow.Death3;
                    p.RootEuler = new Vector3((back ? -88f : 86f) * fall, (row - AnimRow.Death0) * 35f * fall, 0f);
                    p.RootPos.y = Mathf.Lerp(HipHeight, 0.14f, fall);
                    p.ArmL = new Vector3(-62f - 60f * fall, 0f, -28f - 40f * fall); p.ArmR = new Vector3(-48f + 30f * fall, 0f, 12f + 50f * fall);
                    p.ThighL = -14f * fall; p.ShinL = 30f * fall; p.Rifle = new Vector3(-18f + 60f * fall, -22f - 40f * fall, 0f);
                    break;
                case AnimRow.PinnedLoop:
                    Prone(ref p);
                    p.Head.x = -8f; p.ArmL = new Vector3(-150f, 0f, 35f); p.ArmR = new Vector3(-150f, 0f, -35f);
                    p.RootEuler.z = 1.5f * Mathf.Sin(a * 3f); p.ShinL = 10f; p.ShinR = 16f;
                    break;
            }
            return p;
        }

        static void Stride(ref SoldierPose p, float s, float c, float swing, float bob)
        {
            p.ThighL = -swing * s; p.ThighR = swing * s;
            p.ShinL = swing * 0.9f * Mathf.Max(0f, c); p.ShinR = swing * 0.9f * Mathf.Max(0f, -c);
            p.RootPos.y -= bob * (1f - Mathf.Abs(s));
            p.ArmL.x += swing * 0.12f * s; p.ArmR.x -= swing * 0.12f * s;
        }

        static void Prone(ref SoldierPose p)
        {
            p.RootPos = new Vector3(0f, 0.17f, -0.35f); p.RootEuler = new Vector3(84f, 0f, 0f);
            p.Head = new Vector3(-62f, 0f, 0f);
            p.ArmL = new Vector3(-150f, 0f, -22f); p.ArmR = new Vector3(-138f, 0f, 14f);
            p.Rifle = new Vector3(-86f, -8f, 0f);
            p.ThighL = 4f; p.ThighR = -4f; p.ShinL = 6f; p.ShinR = 6f;
        }
    }
}
