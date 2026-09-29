# To every lane: machines with weight, lamps and a ram, on `lane/show/vehicle-weight` (not landed)

The owner, 2026-09-29, on the Dust Front RTS trailer: a craft reference, not a style reference; the lessons start with
vehicle weight; `lane/show/drive-style` is taken over and built on (`decisions.md`). This lane is Phase 1: how machines
move, fire, light and push through props. It is based on the desktop's rebase of drive-style, **`lane/show/drive-style-v20`**
(the ride) on **`lane/sim/drive-feel-v20`** (the momentum seam). The Kettle and Redoubt leg joins are
`lane/show/walker-legs`, not here. Worktree `githubtest-vehicle-weight`. **Waiting for the owner's word to land**.

**Every new look is behind a knob that draws the old picture at 0**, as `fx.deathAbsurd` is (`decisions.md`, 2026-09-28). **The owner set them
on (2026-09-29)**: weight, calibre recoil, 2° shot rock, 0.6 gun hull flash, 0.6° traverse settle, amber lamps, exhaust glow, a pool of 4
machine lights, the ram. A capture or test that wants the old machines sets the knob to 0.

| Surface | Change |
|---|---|
| Replay format | none from this lane. **Its base claims v20, and front-line has since landed v20-v22 on the integration branch**: `drive-feel-v20` renumbers to v23 when it lands, and this lane rebases after it. |
| Knobs | `tank.weight`, `tank.recoil`, `tank.shotRock`, `tank.gunHullFlash`, `tank.traverseSettle`, `tank.squat`, `tank.lamps`, `tank.lampHue`, `tank.lampSize`, `tank.lampPull`, `tank.exhaustGlow`, `tank.lightReach`, `lights.machinePool`, `lights.maxMachineGlows`, `props.ram` (`tasks.md`: Tanks and walkers drawn). |
| `SceneHooks` | + `MachineGlows` (a machine's glow cards, drawn by NightLights), `MachineLight` (a real light from NightLights' machine pool), `LampOut` (a lantern post has gone: its lamp goes out). All three are reset. |
| `NightLights` | now `partial` (`NightLights.Machines.cs`): the flash-glow mesh carries `lights.maxMachineGlows` more cards (zero-sized unless a machine lights one: no draw call); the machine pool; `LampOut`. |
| `TankRenderer` | hook lines only (new partials `.Weight`, `.Lights`, `.Probe`): `Spring.Ride` is the exact solve; `SocketWorld` goes through `MachineSockets` (a walker's one `Socket_Exhaust` answers `Socket_Exhaust0`: walkers had no exhaust and no fire flame, and burned from under the ground) and falls back to the hull's top, not y = 0; `EmitMachineLights` before `Draw`; a cook-off asks for a light. |
| `WalkerGait` | + `TiltCapPitch`, `TiltCapRoll` (the gait's own caps, read by the kick layer). |
| `PropDestruction` | now also `Ram` once a tick, beside `Crush` (`PropDestruction.Ram.cs`, `props.ram`); a lantern that goes by any cause calls `LampOut` (it used to stay lit over the flattened post). |
| Editor | `WeightLab` (traces of a machine's ride as CSV, and knobs set from `unity command eval`). |

If your lane edits `TankRenderer.cs`, `NightLights.cs`, `PropDestruction.cs` or `RenderGround.cs` (`SceneHooks`), expect a
one-line conflict at most: this lane's code lives in its new partial files.

Delete this note once the lane has landed and every lane has rebased over it.
