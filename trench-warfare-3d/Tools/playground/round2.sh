#!/usr/bin/env bash
# The second batch's capture round (docs/22): every machine in the vehicle bay, the same stills every time, and each
# machine's LOD pops, so rounds compare like with like.  usage: round2.sh TAG   (after round.sh TAG, or on its own)
# Vehicles are addressed by index in the library's (alphabetical) order: Brute 0, Croaker 1, Hopper 2, Mercy 3.
set -u
T="${1:-r0}"; HERE="$(dirname "${BASH_SOURCE[0]}")"; P="bash $HERE/pg.sh"
OUT="${PG_OUT:-$(cd "$HERE/../.." && (pwd -W 2>/dev/null || pwd))/Captures/playground}"
$P do "panel 0; labels 0; biome NightMud; timescale 1; lod -1; cookdelay 7; ground grid; team -1; size 1.7"
for spec in "1:croaker" "2:hopper" "3:mercy"; do
  k=${spec%%:*}; n=${spec#*:}
  $P do "model v $k"; sleep 2
  # the three LODs side by side, standing (a flyer hovers), then one scripted destruction
  $P do "vehicle.compare"; sleep 3
  $P do "cam 0 5 0 20 18 80 35"; sleep 1
  $P shot ${T}_${n}_1_compare
  $P do "seq"; sleep 2.9
  $P shot ${T}_${n}_2_hits
  sleep 2.6
  $P shot ${T}_${n}_3_burning
  sleep 6.0
  $P shot ${T}_${n}_4_cookoff
  sleep 12
  $P shot ${T}_${n}_5_after
  # one, close, moving: the walker on the spot, the flyer circling, the ambulance standing
  $P do "vehicle"; sleep 2.5
  case $n in
    croaker) $P do "walk 2.5 1; cam 0 4 0 150 6 26 35"; sleep 2; $P shot ${T}_${n}_6_walk_a; sleep 0.35; $P shot ${T}_${n}_7_walk_b
             $P do "cam 0 4 0 60 6 26 35"; sleep 0.6; $P shot ${T}_${n}_8_walk_side; $P do "walk 0" ;;
    hopper)  $P do "cam 0 14 0 150 10 30 35"; sleep 1; $P shot ${T}_${n}_6_hover
             $P do "fly 8; cam follow 200 14 45 35"; sleep 3; $P shot ${T}_${n}_7_circling; $P do "fly 0" ;;
    mercy)   $P do "cam 0 1.5 0 20 20 22 35"; sleep 1; $P shot ${T}_${n}_6_close
             $P do "cam 0 1.5 0 200 20 22 35"; sleep 1; $P shot ${T}_${n}_7_rear ;;
  esac
  # the far view the battle's lens sees
  # (a flyer's focus is up where it flies: the battle's camera would follow the unit, not the ground under it)
  if [ "$n" = hopper ]; then $P do "cam 0 12 0 20 25 78 25"; else $P do "cam 0 2 0 20 25 78 25"; fi
  sleep 1.2; $P shot ${T}_${n}_9_standard
  # LOD pops (a flyer on the ground for it: the meter frames the rig's middle)
  [ "$n" = hopper ] && $P do "fly 0 0" && sleep 1.5
  $P do "lod -1"; sleep 1
  $P do "lodpop $OUT/${T}_pop_${n}.json"; sleep 3
  [ "$n" = hopper ] && $P do "fly 0 14"
done
$P do "model v 0"
echo "ROUND2 $T DONE"
