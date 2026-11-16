// Phase: CelFx orbital shadow lab prototype (2026-11-14). Selected record, low upward receivers.
#ifndef CEL_ORBITAL_SHADOW_SAMPLING_INCLUDED
#define CEL_ORBITAL_SHADOW_SAMPLING_INCLUDED
TEXTURECUBE(_OrbitalShadowDepthCube); SAMPLER(sampler_OrbitalShadowDepthCube);
float _OrbitalShadowValid;
float4 _OrbitalShadowSourceRange;
float4 _OrbitalShadowStrike; // born, life, bias metres, strength
float4 _OrbitalShadowCentre; // matches the selected diffuse record's elevated centre

half CelOrbitalShadowVisibility(float3 positionWS, half3 normalWS, float3 elevatedCentre, float born)
{
    if (_OrbitalShadowValid < 0.5 || abs(born - _OrbitalShadowStrike.x) > 0.0001
        || dot(elevatedCentre - _OrbitalShadowCentre.xyz, elevatedCentre - _OrbitalShadowCentre.xyz) > 0.0001)
        return 1.0;
    float3 fromLight = positionWS - _OrbitalShadowSourceRange.xyz;
    float distanceToLight = length(fromLight);
    if (distanceToLight <= 0.05 || distanceToLight >= _OrbitalShadowSourceRange.w) return 1.0;
    float3 direction = fromLight / distanceToLight;
    float casterDistance = SAMPLE_TEXTURECUBE_LOD(_OrbitalShadowDepthCube,
        sampler_OrbitalShadowDepthCube, direction, 0).r * _OrbitalShadowSourceRange.w;
    float receiverBias = _OrbitalShadowStrike.z * (1.0 + 2.0 * (1.0 - saturate(dot(normalWS, -direction))));
    half visibility = step(distanceToLight - receiverBias, casterDistance);
    // Preserve existing figure illumination. This prototype casts from objects onto nearby terrain;
    // it does not claim figure self-shadowing, transparent receivers, or terrain self-shadowing.
    float height = max(0.0, positionWS.y - (elevatedCentre.y - 1.1));
    float receiver = smoothstep(0.55, 0.85, normalWS.y) * (1.0 - smoothstep(0.4, 1.8, height));
    return lerp(1.0, visibility, receiver * saturate(_OrbitalShadowStrike.w));
}
#endif
