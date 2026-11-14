// Diffuse orbital illumination, isolated from the warm fire pools and their wet-ground reflections.
#ifndef CEL_ORBITAL_LIGHTING_INCLUDED
#define CEL_ORBITAL_LIGHTING_INCLUDED

#define ORBITAL_LIGHT_CAPACITY 8
float4 _OrbitalLightPositions[ORBITAL_LIGHT_CAPACITY]; // xyz elevated source, w reach
float4 _OrbitalLightColours[ORBITAL_LIGHT_CAPACITY];
float4 _OrbitalLightAxes[ORBITAL_LIGHT_CAPACITY];
float4 _OrbitalLightTiming[ORBITAL_LIGHT_CAPACITY]; // born, life, attack, release
float _OrbitalLightCount;
float _CelNow; // CelFxRenderer.Submit sets this before cameras render, including pinned stills.

half3 CelOrbitalDiffuseLight(float3 positionWS, half3 normalWS)
{
    half3 light = 0;
    int count = min((int)_OrbitalLightCount, ORBITAL_LIGHT_CAPACITY);
    [loop] for (int j = 0; j < count; j++)
    {
        float4 timing = _OrbitalLightTiming[j];
        float age = _CelNow - timing.x;
        if (age <= 0.0 || age >= timing.y) continue;
        float envelope = smoothstep(0.0, timing.z, age)
            * (1.0 - smoothstep(timing.y - timing.w, timing.y, age));
        float3 right = _OrbitalLightAxes[j].xyz;
        float sweepK = saturate((age - 0.2) / 3.16);
        float angle = lerp(0.45, 3.22, pow(sweepK, 0.88));
        float terminal = lerp(3.18, 3.06, smoothstep(3.20, 3.36, age));
        float correction = lerp(0.55, 0.75, smoothstep(3.30, 3.50, age));
        angle = lerp(angle, terminal, smoothstep(2.92, 3.17, age) * correction);
        float3 sweep = right * (0.45 - sin(angle) * 3.0)
            + cross(right, float3(0, 1, 0)) * (cos(angle) * 2.24);
        float3 centre = _OrbitalLightPositions[j].xyz;
        float3 delta = centre + sweep - positionWS;
        // A tall shaft illuminates the upper head as well as vertical chest surfaces.
        delta.y = max(delta.y, 0.65);
        float distanceToLight = max(length(delta), 0.0001);
        float groundFacing = smoothstep(0.55, 0.85, normalWS.y);
        float forwardDistance = max(0.0, dot(delta, cross(right, float3(0, 1, 0))));
        float attenuationDistance = sqrt(dot(delta, delta) + (1.18 * 1.18 - 1.0)
            * forwardDistance * forwardDistance * groundFacing);
        float reach = _OrbitalLightPositions[j].w;
        float falloff = 1.0 - smoothstep(0.10, 1.0, attenuationDistance / reach);
        float facing = smoothstep(-0.10, 0.28, dot(normalWS, delta / distanceToLight));
        // A tall emitter reaches upward-facing parts of nearby curved props as well as the board below.
        float height = positionWS.y - (centre.y - 1.1);
        float upperSurface = smoothstep(1.4, 2.0, height) * smoothstep(0.20, 0.65, normalWS.y);
        float nearShaft = 1.0 - smoothstep(reach * 0.57, reach * 0.624, length(delta.xz));
        float3 upperDelta = delta;
        upperDelta.y = max(upperDelta.y, 4.5);
        float upperFacing = smoothstep(-0.10, 0.28, dot(normalWS, normalize(upperDelta)));
        facing = max(facing, upperSurface * nearShaft * upperFacing);
        light += _OrbitalLightColours[j].rgb * (falloff * facing * envelope);

        // The vertical shaft also fills upward-facing terrain with a broad blue spill.
        // Keeping this lobe on ground normals preserves the dummy's changing light direction.
        float groundReach = reach * (11.5 / 9.3);
        float groundFalloff = 1.0 - smoothstep(0.10, 1.0, length(centre - positionWS) / groundReach);
        float bounceHeight = 1.0 - smoothstep(0.4, 1.8, max(0.0, positionWS.y - (centre.y - 1.1)));
        light += _OrbitalLightColours[j].rgb * half3(0.32143, 0.16667, 0.5)
            * (groundFalloff * groundFacing * bounceHeight * envelope);
    }
    return light;
}

#endif
