#!/usr/bin/env bash
# One capture round: the same shot list every time, so rounds compare like with like.  usage: round.sh TAG
# The critic loop's fixed shot list (docs/22): the same stills every round, so rounds compare like with like.
set -u
T="${1:-r0}"; HERE="$(dirname "${BASH_SOURCE[0]}")"; P="bash $HERE/pg.sh"
OUT="${PG_OUT:-$(cd "$HERE/../.." && (pwd -W 2>/dev/null || pwd))/Captures/playground}"
$P do "panel 0; labels 0; biome NightMud; timescale 1; lod -1; cookdelay 7; ground grid; team -1"
# ---- each LOD's colour fitted to LOD0's on the render, before anything is shot (kept for the session)
$P do "vehicle"; sleep 2; $P do "lodfit $OUT/${T}_fit_vehicle.json"; sleep 2
$P do "unit; clip Rifle Idle"; sleep 2; $P do "lodfit $OUT/${T}_fit_unit.json"; sleep 2
# ---- the vehicle, three LODs side by side, one scripted destruction
$P do "vehicle.compare"; sleep 3
$P shot ${T}_v1_intact
$P do "seq"; sleep 2.9
$P shot ${T}_v2_hits
sleep 2.6
$P shot ${T}_v3_burning
sleep 6.0
$P shot ${T}_v4_cookoff
sleep 12
$P shot ${T}_v5_aftermath
# ---- one vehicle: close, standard view, far
$P do "vehicle; cam close"; $P do "cam 0 3.4 0 0 12 15 42"; sleep 2
$P shot ${T}_v6_close
$P do "cam 0 1.5 0 20 25 75 25"; sleep 1.5
$P shot ${T}_v7_standard
$P do "cam 0 1.5 0 20 25 240 25"; sleep 1.5
$P shot ${T}_v8_far
# ---- the figure, four LODs side by side
$P do "unit.compare; clip Rifle Idle"; sleep 2.5
$P shot ${T}_u1_idle
$P do "clip Walk With Rifle"; sleep 1.3
$P shot ${T}_u2_walk
$P do "clip Rifle Crouch Walk"; sleep 1.1
$P shot ${T}_u3_crouchwalk
$P do "clip Firing Rifle"; sleep 1.0
$P shot ${T}_u4_fire
$P do "face 105"; sleep 0.8
$P shot ${T}_u5_fire_side
# ---- one figure close, then set alight
$P do "unit; clip Walk With Rifle"; sleep 1.5
$P shot ${T}_u6_close
$P do "ignite"; sleep 2.5
$P shot ${T}_u7_burning
sleep 5.5
$P shot ${T}_u8_charred
# ---- a file of figures from 8 m to 300 m at the standard lens, and the vehicle among men
$P do "unit.squad; clip Walk With Rifle; cam 0 0 16.2 0 12 28.8 25"; sleep 2
$P shot ${T}_u9_squad
$P do "mixed; clip Rifle Idle"; sleep 2
$P shot ${T}_m1_mixed
$P do "cam -2 1.5 -3 200 25 75 25"; sleep 1
$P shot ${T}_m2_mixed_standard
$P do "ground mud; team split"; sleep 0.5
$P shot ${T}_m3_mud_standard
$P do "sidehue $OUT/${T}_m3_sidehue.json"; sleep 1
$P do "ground grid; team -1"
# ---- a building from the game's kit, shelled until it comes down
$P do "set Ruins"; sleep 2
$P shot ${T}_b1_intact
$P do "shell; shell; shell"; sleep 3
$P shot ${T}_b2_shelled3
$P do "shell; shell; shell; shell; shell"; sleep 6
$P shot ${T}_b3_shelled8
$P do "cam 0 3 0 20 25 75 25"; sleep 1
$P shot ${T}_b4_standard
$P do "cutsdebug 1; set Ruins; shell; shell; shell"; sleep 3
$P do "cam 0 3 0 20 25 30 42"; sleep 1
$P shot ${T}_b5_cutfaces
$P do "cutsdebug 0"
# ---- LOD pops: each boundary, both sides of it, at the switch distance
$P do "vehicle; lod -1"; sleep 1.5
$P do "lodpop $OUT/${T}_pop_vehicle.json"; sleep 2
$P do "unit; lod -1; clip Rifle Idle"; sleep 1.5
$P do "lodpop $OUT/${T}_pop_unit.json"; sleep 2
echo "ROUND $T DONE"
