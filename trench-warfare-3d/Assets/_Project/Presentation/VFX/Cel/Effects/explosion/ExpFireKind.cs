// Phase: reference match. Merged procedural fire billows, one immutable-record pool.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Three merged, warped billows with nonconcentric heat fields; extra.x selects body or vertical heart.
    /// Kept for builds by Resources/ShaderKeep/KeepCelExpFire.mat. The new Close records replace old Lobe records.
    /// No per-frame CPU work. Transparency is ordered per pool, not sorted per instance.</summary>
    public sealed class ExpFireKind : CelKind
    {
        public const string KindName = "ExpFire";
        public const float Body = 0f, Heart = 1f, SiteBody = 2f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFire");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpFire");
        public override int Capacity => 256;
        public override int Order => 14;
    }
    /// <summary>The same procedural fire drawn below the secondary smoke bank.</summary>
    public sealed class ExpSiteFireKind : CelKind
    {
        public const string KindName = "ExpSiteFire";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFire");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpSiteFire");
        public override int Capacity => 96;
        public override int Order => 9;
        public override void Configure(Material material) { material.renderQueue = 3009; }
    }

    /// <summary>One authored connected orange surround, composited after pressure and before Body/Heart.</summary>
    public sealed class ExpBodyUnderlayKind : CelKind
    {
        public const string KindName = "ExpBodyUnderlay";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFire");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpBodyUnderlay");
        public override int Capacity => 64;
        public override int Order => 13;
        public override void Configure(Material material) { material.renderQueue = 3013; }
    }
}

namespace TW.Presentation.VFX.Cel
{
    /// <summary>The continuous hot impact stays in front of the surrounding gray wrap.</summary>
    public sealed class ExpHeartKind : CelKind
    {
        public const string KindName = "ExpHeart";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFire");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpHeart");
        public override int Capacity => 256;
        public override int Order => 17;
        public override void Configure(Material material) { material.renderQueue = 3017; }
    }
}

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Four authored warm body pockets, above orange billows and below the impact Heart.</summary>
    public sealed class ExpWarmBodyKind : CelKind
    {
        public const string KindName = "ExpWarmBody";
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ExpFire");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel ExpWarmBody");
        public override int Capacity => 64;
        public override int Order => 15;
        public override void Configure(Material material) { material.renderQueue = 3015; }
    }
}
