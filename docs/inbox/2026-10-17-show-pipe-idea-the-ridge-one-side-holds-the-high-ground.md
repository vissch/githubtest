# For: lane/show/pipe-idea-the-ridge-one-side-holds-the-high-ground

The sim half of "The Ridge" is on `lane/sim/pipe-idea-the-ridge-one-side-holds-the-high-ground`, not landed yet.
Wait for the landing before wiring Ground.Ridge, the launch screen or the pictures.

What you get from `Assets/_Project/Sim/Terrain/BattlefieldGenerator.cs`:
- `BattlefieldParams.Rise` (signed metres, 0 = today's flat field).
- `BattlefieldParams.Ridge(seed)`: Width 110, Length 240, Rise 9, Forest 0.25, Shelling 0.6, Mud 0.4,
  WaterLevel 0.15, no river, Wrecks 2, Bombardment 8, no sea.
- `BattlefieldGenerator.RiseAt` and `CrestLineZ`: use these for a camera or a mesh that needs the ridge's
  height at a given z/column; do not re-derive the slope.

Blob and hashes:
- `Rise` is appended to the params blob after `SeaMargin`; an older saved blob reads `Rise = 0`.
- Nothing hashed moved. `ReplayRecorder.FormatVersion` stays 37 (next free version is 38 if you ever need one).

Ground shape:
- The crest trench is levelled uphill only (5 m shelf uphill, 2 m downhill of the trench). Do not flatten its
  downhill lip — that is what keeps the sightline over the slope.

This lane is not landed. The show half follows once it lands.
