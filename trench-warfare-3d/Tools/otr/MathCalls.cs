// Tools/otr.py's stand-in for the maths Unity implements natively: Quaternion (Euler in Unity's order, Z then X then Y;
// AngleAxis, LookRotation, FromToRotation, Slerp, Lerp, Inverse, back to Euler in 0..2 pi), Matrix4x4 (TRS, Inverse,
// Inverse3DAffine, Transpose, determinant, IsIdentity, ValidTRS, rotation and lossy scale, LookAt, Ortho,
// Perspective, Frustum; column-major like Unity) and Vector3 (Slerp, RotateTowards, OrthoNormalize). Standard
// formulas in double precision rounded to float: they agree with Unity to float rounding, not bit for bit.
using System;
using System.Runtime.InteropServices;

static unsafe class FakeMath
{
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPP(float* a, float* b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPP(float* a, float* b, float* c);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPPP(float* a, float* b, float* c, float* d);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPFP(float* a, float* b, float t, float* r);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VFPP(float angle, float* axis, float* r);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void VPPFFP(float* a, float* b, float x, float y, float* r);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void V6FP(float a, float b, float c, float d, float e, float f, float* r);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate void V4FP(float a, float b, float c, float d, float* r);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] delegate float FP(float* a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BP(float* a);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BPP(float* a, float* b);
    [UnmanagedFunctionPointer(CallingConvention.Cdecl)] [return: MarshalAs(UnmanagedType.U1)] delegate bool BPPF(float* a, float* b, float t);

    // ---- quaternions (x, y, z, w) ----
    static void Q(float* r, double x, double y, double z, double w) { r[0] = (float)x; r[1] = (float)y; r[2] = (float)z; r[3] = (float)w; }
    static void Mul(double[] a, double[] b, double[] r)
    {
        double x = a[3] * b[0] + a[0] * b[3] + a[1] * b[2] - a[2] * b[1];
        double y = a[3] * b[1] + a[1] * b[3] + a[2] * b[0] - a[0] * b[2];
        double z = a[3] * b[2] + a[2] * b[3] + a[0] * b[1] - a[1] * b[0];
        double w = a[3] * b[3] - a[0] * b[0] - a[1] * b[1] - a[2] * b[2];
        r[0] = x; r[1] = y; r[2] = z; r[3] = w;
    }
    static void FromEuler(float* e, float* r)
    {
        double hx = e[0] * 0.5, hy = e[1] * 0.5, hz = e[2] * 0.5;
        var qx = new[] { Math.Sin(hx), 0, 0, Math.Cos(hx) };
        var qy = new[] { 0, Math.Sin(hy), 0, Math.Cos(hy) };
        var qz = new[] { 0, 0, Math.Sin(hz), Math.Cos(hz) };
        var t = new double[4]; var o = new double[4];
        Mul(qy, qx, t); Mul(t, qz, o);   // applied to a vector: z first, then x, then y
        Q(r, o[0], o[1], o[2], o[3]);
    }
    // rotation matrix rows from a quaternion
    static double[,] RotM(float* q)
    {
        double x = q[0], y = q[1], z = q[2], w = q[3];
        double n = x * x + y * y + z * z + w * w, s = n > 0 ? 2 / n : 0;
        double xx = x * x * s, yy = y * y * s, zz = z * z * s, xy = x * y * s, xz = x * z * s, yz = y * z * s, wx = w * x * s, wy = w * y * s, wz = w * z * s;
        return new double[,] { { 1 - yy - zz, xy - wz, xz + wy }, { xy + wz, 1 - xx - zz, yz - wx }, { xz - wy, yz + wx, 1 - xx - yy } };
    }
    static void FromRot(double[,] m, float* r)
    {
        double tr = m[0, 0] + m[1, 1] + m[2, 2];
        if (tr > 0) { double s = Math.Sqrt(tr + 1) * 2; Q(r, (m[2, 1] - m[1, 2]) / s, (m[0, 2] - m[2, 0]) / s, (m[1, 0] - m[0, 1]) / s, 0.25 * s); }
        else if (m[0, 0] > m[1, 1] && m[0, 0] > m[2, 2]) { double s = Math.Sqrt(1 + m[0, 0] - m[1, 1] - m[2, 2]) * 2; Q(r, 0.25 * s, (m[0, 1] + m[1, 0]) / s, (m[0, 2] + m[2, 0]) / s, (m[2, 1] - m[1, 2]) / s); }
        else if (m[1, 1] > m[2, 2]) { double s = Math.Sqrt(1 + m[1, 1] - m[0, 0] - m[2, 2]) * 2; Q(r, (m[0, 1] + m[1, 0]) / s, 0.25 * s, (m[1, 2] + m[2, 1]) / s, (m[0, 2] - m[2, 0]) / s); }
        else { double s = Math.Sqrt(1 + m[2, 2] - m[0, 0] - m[1, 1]) * 2; Q(r, (m[0, 2] + m[2, 0]) / s, (m[1, 2] + m[2, 1]) / s, 0.25 * s, (m[1, 0] - m[0, 1]) / s); }
    }
    static double Wrap(double a) { a %= 2 * Math.PI; return a < 0 ? a + 2 * Math.PI : a; }
    static void ToEuler(float* q, float* r)
    {
        var m = RotM(q);
        double sx = -m[1, 2];
        double x = Math.Asin(Math.Max(-1, Math.Min(1, sx))), y, z;
        if (Math.Abs(sx) < 0.999999) { y = Math.Atan2(m[0, 2], m[2, 2]); z = Math.Atan2(m[1, 0], m[1, 1]); }
        else { y = Math.Atan2(-m[2, 0], m[0, 0]); z = 0; }
        r[0] = (float)Wrap(x); r[1] = (float)Wrap(y); r[2] = (float)Wrap(z);
    }
    static double[] N(double x, double y, double z) { double l = Math.Sqrt(x * x + y * y + z * z); return l > 1e-12 ? new[] { x / l, y / l, z / l } : new double[] { 0, 0, 0 }; }
    static double[] Cross(double[] a, double[] b) => new[] { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
    static void LookRotation(float* f, float* u, float* r)
    {
        var z = N(f[0], f[1], f[2]);
        if (z[0] == 0 && z[1] == 0 && z[2] == 0) { Q(r, 0, 0, 0, 1); return; }
        var x = Cross(new double[] { u[0], u[1], u[2] }, z);
        double lx = Math.Sqrt(x[0] * x[0] + x[1] * x[1] + x[2] * x[2]);
        if (lx < 1e-9) { FromTo(new double[] { 0, 0, 1 }, z, r); return; }
        x = N(x[0], x[1], x[2]);
        var y = Cross(z, x);
        FromRot(new double[,] { { x[0], y[0], z[0] }, { x[1], y[1], z[1] }, { x[2], y[2], z[2] } }, r);
    }
    static void FromTo(double[] a, double[] b, float* r)
    {
        a = N(a[0], a[1], a[2]); b = N(b[0], b[1], b[2]);
        double d = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
        if (d > 0.999999) { Q(r, 0, 0, 0, 1); return; }
        if (d < -0.999999)
        {
            var axis = Cross(new double[] { 1, 0, 0 }, a);
            if (axis[0] * axis[0] + axis[1] * axis[1] + axis[2] * axis[2] < 1e-12) axis = Cross(new double[] { 0, 1, 0 }, a);
            axis = N(axis[0], axis[1], axis[2]); Q(r, axis[0], axis[1], axis[2], 0); return;
        }
        var c = Cross(a, b); double w = 1 + d, l = Math.Sqrt(c[0] * c[0] + c[1] * c[1] + c[2] * c[2] + w * w);
        Q(r, c[0] / l, c[1] / l, c[2] / l, w / l);
    }
    static void Slerp(float* a, float* b, float t, float* r, bool clamp)
    {
        if (clamp) t = Math.Max(0, Math.Min(1, t));
        double ax = a[0], ay = a[1], az = a[2], aw = a[3], bx = b[0], by = b[1], bz = b[2], bw = b[3];
        double d = ax * bx + ay * by + az * bz + aw * bw;
        if (d < 0) { d = -d; bx = -bx; by = -by; bz = -bz; bw = -bw; }
        double s0, s1;
        if (d > 0.9995) { s0 = 1 - t; s1 = t; }
        else { double th = Math.Acos(d), sn = Math.Sin(th); s0 = Math.Sin((1 - t) * th) / sn; s1 = Math.Sin(t * th) / sn; }
        double x = s0 * ax + s1 * bx, y = s0 * ay + s1 * by, z = s0 * az + s1 * bz, w = s0 * aw + s1 * bw, l = Math.Sqrt(x * x + y * y + z * z + w * w);
        Q(r, x / l, y / l, z / l, w / l);
    }
    static void Lerp(float* a, float* b, float t, float* r, bool clamp)
    {
        if (clamp) t = Math.Max(0, Math.Min(1, t));
        double sign = a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3] < 0 ? -1 : 1;
        double x = a[0] + (sign * b[0] - a[0]) * t, y = a[1] + (sign * b[1] - a[1]) * t, z = a[2] + (sign * b[2] - a[2]) * t, w = a[3] + (sign * b[3] - a[3]) * t;
        double l = Math.Sqrt(x * x + y * y + z * z + w * w);
        Q(r, x / l, y / l, z / l, w / l);
    }

    // ---- matrices: float[16] column-major, element (row r, col c) at c * 4 + r ----
    static double G(float* m, int r, int c) => m[c * 4 + r];
    static void S(float* m, int r, int c, double v) => m[c * 4 + r] = (float)v;
    static double Det3(double a, double b, double c, double d, double e, double f, double g, double h, double i) => a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g);
    static double Det(float* m)
    {
        double det = 0;
        for (int c = 0; c < 4; c++)
        {
            int c0 = c == 0 ? 1 : 0, c1 = c <= 1 ? 2 : 1, c2 = c <= 2 ? 3 : 2;
            double minor = Det3(G(m, 1, c0), G(m, 1, c1), G(m, 1, c2), G(m, 2, c0), G(m, 2, c1), G(m, 2, c2), G(m, 3, c0), G(m, 3, c1), G(m, 3, c2));
            det += ((c & 1) == 0 ? 1 : -1) * G(m, 0, c) * minor;
        }
        return det;
    }
    static bool Inverse(float* m, float* r)
    {
        var a = new double[4, 8];
        for (int i = 0; i < 4; i++) { for (int j = 0; j < 4; j++) a[i, j] = G(m, i, j); a[i, 4 + i] = 1; }
        for (int c = 0; c < 4; c++)
        {
            int p = c; for (int i = c + 1; i < 4; i++) if (Math.Abs(a[i, c]) > Math.Abs(a[p, c])) p = i;
            if (Math.Abs(a[p, c]) < 1e-20) { for (int k = 0; k < 16; k++) r[k] = 0; return false; }
            if (p != c) for (int j = 0; j < 8; j++) { double t = a[c, j]; a[c, j] = a[p, j]; a[p, j] = t; }
            double piv = a[c, c]; for (int j = 0; j < 8; j++) a[c, j] /= piv;
            for (int i = 0; i < 4; i++) if (i != c) { double f = a[i, c]; if (f != 0) for (int j = 0; j < 8; j++) a[i, j] -= f * a[c, j]; }
        }
        for (int i = 0; i < 4; i++) for (int j = 0; j < 4; j++) S(r, i, j, a[i, 4 + j]);
        return true;
    }
    static void TRS(float* p, float* q, float* s, float* r)
    {
        var m = RotM(q);
        for (int i = 0; i < 3; i++) { for (int j = 0; j < 3; j++) S(r, i, j, m[i, j] * s[j]); S(r, i, 3, p[i]); S(r, 3, i, 0); }
        S(r, 3, 3, 1);
    }

    public static void Install(Action<string, Delegate> reg)
    {
        const string QT = "UnityEngine.Quaternion::";
        reg(QT + "Internal_FromEulerRad_Injected", new VPP((e, r) => FromEuler(e, r)));
        reg(QT + "Internal_ToEulerRad_Injected", new VPP((q, r) => ToEuler(q, r)));
        reg(QT + "Inverse_Injected", new VPP((q, r) =>
        {
            double n = q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3]; if (n <= 0) n = 1;
            Q(r, -q[0] / n, -q[1] / n, -q[2] / n, q[3] / n);
        }));
        reg(QT + "AngleAxis_Injected", new VFPP((angle, axis, r) =>
        {
            var a = N(axis[0], axis[1], axis[2]); double h = angle * Math.PI / 360.0, sn = Math.Sin(h);
            Q(r, a[0] * sn, a[1] * sn, a[2] * sn, Math.Cos(h));
        }));
        reg(QT + "LookRotation_Injected", new VPPP((f, u, r) => LookRotation(f, u, r)));
        reg(QT + "FromToRotation_Injected", new VPPP((a, b, r) => FromTo(new double[] { a[0], a[1], a[2] }, new double[] { b[0], b[1], b[2] }, r)));
        reg(QT + "Slerp_Injected", new VPPFP((a, b, t, r) => Slerp(a, b, t, r, true)));
        reg(QT + "SlerpUnclamped_Injected", new VPPFP((a, b, t, r) => Slerp(a, b, t, r, false)));
        reg(QT + "Lerp_Injected", new VPPFP((a, b, t, r) => Lerp(a, b, t, r, true)));
        reg(QT + "LerpUnclamped_Injected", new VPPFP((a, b, t, r) => Lerp(a, b, t, r, false)));
        reg(QT + "Internal_ToAxisAngleRad_Injected", new VPPP((q, axis, angle) =>
        {
            double w = Math.Max(-1, Math.Min(1, q[3])), s = Math.Sqrt(1 - w * w);
            *angle = (float)(2 * Math.Acos(w));
            if (s < 1e-6) { axis[0] = 1; axis[1] = 0; axis[2] = 0; } else { axis[0] = (float)(q[0] / s); axis[1] = (float)(q[1] / s); axis[2] = (float)(q[2] / s); }
        }));

        const string MX = "UnityEngine.Matrix4x4::";
        reg(MX + "TRS_Injected", new VPPPP((p, q, s, r) => TRS(p, q, s, r)));
        reg(MX + "Inverse_Injected", new VPP((m, r) => Inverse(m, r)));
        reg(MX + "Inverse3DAffine_Injected", new BPP((m, r) => Inverse(m, r)));
        reg(MX + "Transpose_Injected", new VPP((m, r) => { for (int i = 0; i < 4; i++) for (int j = 0; j < 4; j++) S(r, i, j, G(m, j, i)); }));
        reg(MX + "GetDeterminant", new FP(m => (float)Det(m)));
        reg(MX + "IsIdentity", new BP(m => { for (int i = 0; i < 4; i++) for (int j = 0; j < 4; j++) if (G(m, i, j) != (i == j ? 1 : 0)) return false; return true; }));
        reg(MX + "ValidTRS", new BP(m => G(m, 3, 0) == 0 && G(m, 3, 1) == 0 && G(m, 3, 2) == 0 && G(m, 3, 3) == 1 && Math.Abs(Det(m)) > 1e-12));
        reg(MX + "GetLossyScale_Injected", new VPP((m, r) =>
        {
            double sgn = Det(m) < 0 ? -1 : 1;
            for (int c = 0; c < 3; c++) r[c] = (float)(Math.Sqrt(G(m, 0, c) * G(m, 0, c) + G(m, 1, c) * G(m, 1, c) + G(m, 2, c) * G(m, 2, c)) * (c == 0 ? sgn : 1));
        }));
        reg(MX + "GetRotation_Injected", new VPP((m, r) =>
        {
            float* f = stackalloc float[3]; float* u = stackalloc float[3];
            for (int i = 0; i < 3; i++) { f[i] = (float)G(m, i, 2); u[i] = (float)G(m, i, 1); }
            LookRotation(f, u, r);
        }));
        reg(MX + "LookAt_Injected", new VPPPP((from, to, up, r) =>
        {
            var z = N(to[0] - from[0], to[1] - from[1], to[2] - from[2]);
            var x = N(Cross(new double[] { up[0], up[1], up[2] }, z)[0], Cross(new double[] { up[0], up[1], up[2] }, z)[1], Cross(new double[] { up[0], up[1], up[2] }, z)[2]);
            var y = Cross(z, x);
            for (int i = 0; i < 3; i++) { S(r, i, 0, x[i]); S(r, i, 1, y[i]); S(r, i, 2, z[i]); S(r, i, 3, from[i]); S(r, 3, i, 0); }
            S(r, 3, 3, 1);
        }));
        reg(MX + "Ortho_Injected", new V6FP((l, rt, b, t, n, f, r) =>
        {
            for (int k = 0; k < 16; k++) r[k] = 0;
            S(r, 0, 0, 2.0 / (rt - l)); S(r, 1, 1, 2.0 / (t - b)); S(r, 2, 2, -2.0 / (f - n));
            S(r, 0, 3, -(rt + l) / (double)(rt - l)); S(r, 1, 3, -(t + b) / (double)(t - b)); S(r, 2, 3, -(f + n) / (double)(f - n)); S(r, 3, 3, 1);
        }));
        reg(MX + "Perspective_Injected", new V4FP((fov, aspect, n, f, r) =>
        {
            for (int k = 0; k < 16; k++) r[k] = 0;
            double cot = 1.0 / Math.Tan(fov * Math.PI / 360.0);
            S(r, 0, 0, cot / aspect); S(r, 1, 1, cot); S(r, 2, 2, (f + n) / (double)(n - f)); S(r, 2, 3, 2.0 * f * n / (n - f)); S(r, 3, 2, -1);
        }));
        reg(MX + "Frustum_Injected", new V6FP((l, rt, b, t, n, f, r) =>
        {
            for (int k = 0; k < 16; k++) r[k] = 0;
            S(r, 0, 0, 2.0 * n / (rt - l)); S(r, 1, 1, 2.0 * n / (t - b)); S(r, 0, 2, (rt + l) / (double)(rt - l)); S(r, 1, 2, (t + b) / (double)(t - b));
            S(r, 2, 2, -(f + n) / (double)(f - n)); S(r, 2, 3, -2.0 * f * n / (f - n)); S(r, 3, 2, -1);
        }));
        reg(MX + "CompareApproximately_Injected", new BPPF((a, b, th) => { for (int k = 0; k < 16; k++) if (Math.Abs(a[k] - b[k]) > th) return false; return true; }));

        const string V3 = "UnityEngine.Vector3::";
        reg(V3 + "Slerp_Injected", new VPPFP((a, b, t, r) => SlerpV(a, b, Math.Max(0, Math.Min(1, t)), r)));
        reg(V3 + "SlerpUnclamped_Injected", new VPPFP((a, b, t, r) => SlerpV(a, b, t, r)));
        reg(V3 + "RotateTowards_Injected", new VPPFFP((cur, tgt, maxRad, maxMag, r) =>
        {
            double lc = Math.Sqrt(cur[0] * cur[0] + cur[1] * cur[1] + cur[2] * cur[2]), lt = Math.Sqrt(tgt[0] * tgt[0] + tgt[1] * tgt[1] + tgt[2] * tgt[2]);
            if (lc < 1e-9 || lt < 1e-9) { for (int i = 0; i < 3; i++) r[i] = cur[i] + (float)Math.Max(-maxMag, Math.Min(maxMag, tgt[i] - cur[i])); return; }
            double d = (cur[0] * tgt[0] + cur[1] * tgt[1] + cur[2] * tgt[2]) / (lc * lt), ang = Math.Acos(Math.Max(-1, Math.Min(1, d)));
            double t = ang < 1e-9 ? 1 : Math.Min(1, maxRad / ang);
            float* dir = stackalloc float[3]; float* a = stackalloc float[3]; float* b = stackalloc float[3];
            for (int i = 0; i < 3; i++) { a[i] = (float)(cur[i] / lc); b[i] = (float)(tgt[i] / lt); }
            SlerpV(a, b, (float)t, dir);
            double len = lc + Math.Max(-maxMag, Math.Min(maxMag, lt - lc));
            double ld = Math.Sqrt(dir[0] * dir[0] + dir[1] * dir[1] + dir[2] * dir[2]);
            for (int i = 0; i < 3; i++) r[i] = (float)(dir[i] / ld * len);
        }));
        reg(V3 + "OrthoNormalize2", new VPP((a, b) => Ortho(a, b, null)));
        reg(V3 + "OrthoNormalize3", new VPPP((a, b, c) => Ortho(a, b, c)));
    }

    static void SlerpV(float* a, float* b, float t, float* r)
    {
        double la = Math.Sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]), lb = Math.Sqrt(b[0] * b[0] + b[1] * b[1] + b[2] * b[2]);
        if (la < 1e-9 || lb < 1e-9) { for (int i = 0; i < 3; i++) r[i] = a[i] + (b[i] - a[i]) * t; return; }
        double d = Math.Max(-1, Math.Min(1, (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]) / (la * lb))), th = Math.Acos(d) * t;
        var ua = new[] { a[0] / la, a[1] / la, a[2] / la };
        var rel = N(b[0] / lb - ua[0] * d, b[1] / lb - ua[1] * d, b[2] / lb - ua[2] * d);
        double len = la + (lb - la) * t;
        for (int i = 0; i < 3; i++) r[i] = (float)((ua[i] * Math.Cos(th) + rel[i] * Math.Sin(th)) * len);
    }

    static void Ortho(float* a, float* b, float* c)
    {
        var x = N(a[0], a[1], a[2]); for (int i = 0; i < 3; i++) a[i] = (float)x[i];
        double d = b[0] * x[0] + b[1] * x[1] + b[2] * x[2];
        var y = N(b[0] - d * x[0], b[1] - d * x[1], b[2] - d * x[2]); for (int i = 0; i < 3; i++) b[i] = (float)y[i];
        if (c == null) return;
        double d1 = c[0] * x[0] + c[1] * x[1] + c[2] * x[2], d2 = c[0] * y[0] + c[1] * y[1] + c[2] * y[2];
        var z = N(c[0] - d1 * x[0] - d2 * y[0], c[1] - d1 * x[1] - d2 * y[1], c[2] - d1 * x[2] - d2 * y[2]); for (int i = 0; i < 3; i++) c[i] = (float)z[i];
    }
}
