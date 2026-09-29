#!/usr/bin/env bash
# The second batch's capture round (docs/22): every machine in the vehicle bay, the same stills every time, and each
# machine's LOD pops, so rounds compare like with like.  usage: round2.sh TAG   (after round.sh TAG, or on its own)
# Vehicles are addressed by name ("model v Croaker"): by index, adding the Bullfrog (2026-09-28) shifted every machine after it.
set -u
T="${1:-r0}"; HERE="$(dirname "${BASH_SOURCE[0]}")"; P="bash $HERE/pg.sh"
OUT="${PG_OUT:-$(cd "$HERE/../.." && (pwd -W 2>/dev/null || pwd))/Captures/playground}"
$P do "panel 0; labels 0; biome NightMud; timescale 1; lod -1; cookdelay 7; ground grid; team -1; size 1.7"
for spec in "Bullfrog:bullfrog" "Croaker:croaker" "Hopper:hopper" "Mercy:mercy" "Skimmer:skimmer"; do
  k=${spec%%:*}; n=${spec#*:}
  $P do "model v $k"; sleep 2
  # the three LODs side by side, standing (a flyer hovers), then one scripted destruction
  $P do "vehicle.compare"; sleep 3
  $P do "cam 0 5 0 20 18 80 35"; sleep 1
  $P shot ${T}_${n}_1_compare
  # (2.0 s: after the first two hits, before the HE; at 2.9 s a machine already knocked out was shot as "hits", r37, and
  # at 2.3 s the HE's flash, due at 2.6 s, was caught lighting the machine cream, loop 3)
  $P do "seq"; sleep 2.0
  $P shot ${T}_${n}_2_hits
  sleep 3.5
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
    hopper)  $P do "team 0; cam 0 14 0 150 10 42 35"; sleep 1; $P shot ${T}_${n}_6_hover
             $P do "fly 8; cam follow 200 14 45 35"; sleep 3; $P shot ${T}_${n}_7_circling; $P do "fly 0; team -1" ;;
    skimmer) $P do "cam 0 3 0 150 12 34 35"; sleep 1; $P shot ${T}_${n}_6_close
             $P do "fly 6; cam follow 200 14 50 35"; sleep 3; $P shot ${T}_${n}_7_skimming; $P do "fly 0" ;;
    bullfrog) # a burst at a point on the ground ahead and to its left, slowed to a fifth so the still catches it mid-burst
             # (the rounds start once the barrels are at 70 % speed, 0.35 s into the burst: 1.75 s of real time at a fifth;
             # after 1.3 s the close still caught no round at all, critic g4). The burst lasts 11 s at a fifth.
             $P do "target near; cam 0 2.4 0 150 12 18 38"; sleep 2.5; $P do "timescale 0.2; fire"; sleep 2.6; $P shot ${T}_${n}_6_fire_close
             # and three more frames, each about 0.2 game-seconds on: their brightest pixels together are a short exposure
             # that shows the stream (tracers, cases, the flashes' rhythm), which one frame catches only by chance (g6)
             for x in 2 3 4; do $P shot ${T}_${n}_6_fire_close_$x >/dev/null; done
             python -c "import sys; from PIL import Image, ImageChops; f=sys.argv[1]; im=Image.open(f+'.png').convert('RGB')
for x in (2,3,4): im=ImageChops.lighter(im, Image.open(f+'_%d.png' % x).convert('RGB'))
im.save(f+'_exposure.png')" "$OUT/${T}_${n}_6_fire_close"
             $P do "cam follow 250 38 42 45"; sleep 0.6; $P shot ${T}_${n}_7_fire_wide; sleep 0.3; $P shot ${T}_${n}_7_fire_wide_b
             # their brightest pixels together too: a single wide frame catches a tracer by chance (none in g11)
             python -c "import sys; from PIL import Image, ImageChops; f=sys.argv[1]; ImageChops.lighter(Image.open(f+'.png').convert('RGB'), Image.open(f+'_b.png').convert('RGB')).save(f+'_exposure.png')" "$OUT/${T}_${n}_7_fire_wide"
             # a second burst on the first one's heat, close again: the heat builds over bursts (the early stills caught
             # the barrels dark, g8), and a still taken 3 s later had caught the first burst over (g10)
             $P do "fire; cam 0 2.4 0 150 12 18 38"; sleep 1.5; $P shot ${T}_${n}_6_fire_hot; $P do "timescale 1"; sleep 3
             # chosen moments, time frozen: a still takes a second or more to come back and a hop is 1.15 s, so stills taken
             # on a clock caught one moment of two hops (g1), and at a quarter speed 1.6 game-seconds apart (g2)
             $P do "target off; walk 2 1; cam 0 2 0 110 8 20 38"; sleep 2; $P do "timescale 0; hopphase 0.1"; sleep 0.5; $P shot ${T}_${n}_8_hop_a
             $P do "hopphase 0.43"; sleep 0.5; $P shot ${T}_${n}_8_hop_b; $P do "hopphase 0.72"; sleep 0.5; $P shot ${T}_${n}_8_hop_c; $P do "walk 0; timescale 1" ;;
    mercy)   $P do "cam 0 1.5 0 20 20 22 35"; sleep 1; $P shot ${T}_${n}_6_close
             $P do "cam 0 1.5 0 200 20 22 35"; sleep 1; $P shot ${T}_${n}_7_rear ;;
  esac
  # the far view the battle's lens sees
  # (a flyer's focus is up where it flies: the battle's camera would follow the unit, not the ground under it)
  # (with a side colour: in the battle every machine has one, and a flyer's ring on the ground under it is its anchor)
  if [ "$n" = hopper ]; then $P do "team 0; cam 0 12 0 20 25 78 25"; else $P do "team 0; cam 0 2 0 20 25 78 25"; fi
  sleep 1.2; $P shot ${T}_${n}_9_standard
  # the hopper mid-hop at that view too: the read that matters is the battle's, and the hop was only ever shot close (g6)
  if [ "$n" = bullfrog ]; then $P do "walk 2 1"; sleep 1.5; $P do "timescale 0; hopphase 0.3"; sleep 0.5; $P shot ${T}_${n}_9_standard_hop; $P do "walk 0; timescale 1"; sleep 1; fi
  $P do "team -1"
  # LOD pops (a flyer on the ground for it: the meter frames the rig's middle)
  [ "$n" = hopper ] && $P do "fly 0 0" && sleep 1.5
  $P do "lod -1"; sleep 1
  $P do "lodpop $OUT/${T}_pop_${n}.json"; sleep 3
  [ "$n" = hopper ] && $P do "fly 0 14"
done
$P do "model v Brute"
echo "ROUND2 $T DONE"
