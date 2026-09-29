# To `lane/show/pipe-vfx`: dust from a pivoting machine (`TrackDust.Weigh`, on `lane/show/vehicle-weight`)

Dust Front's heavy machines churn soft dark dust when they pivot. Ours throw none: `TankRenderer.Effects` gates its track
dust on the hull's speed alone (`bool moving = Mathf.Abs(v.Speed) > 0.25f || v.Bogged || v.Ditched;`), and a machine
turning on the spot has a hull speed of zero while both its tracks churn.

The dust drawings are yours, so `lane/show/vehicle-weight` does not touch that line. It adds the pure half:
`Presentation/Camera/TrackDust.cs`, `TrackDust.Weigh(speed, yawRate, halfGauge)` returns each track's dust from 0 to 1.
The weight is each track's own ground speed (the treads' `vl`/`vr` as TankRenderer drives them), plus a pivot's sideways
scuff. `TrackDustTests` show the old gate missing a pivot on the spot that `Weigh` catches.

When both lanes are in, the call site would be the dust block in `Effects`. Gate on
`Weigh(v.Speed, v.YawRate, m.HalfGauge)` per side instead of `moving`, and scale each side's puff alpha or rate by its
weight. Keep the bogged and ditched cases as they are.

Delete this note once pipe-vfx has used it or decided against it.
