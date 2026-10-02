// Phase: tooling (2026-10-02) - the landing folder of the test split. No test lives here.
// The EditMode tests are one assembly per module: Tests/Sim, Tests/Match, Tests/Show, Tests/UI, Tests/Project
// (docs/reference/workflow.md, section 5). This folder and its catch-all assembly stay so that a lane cut before the
// split still compiles after it rebases: the test files it added arrive here, and validate.py then names the module
// folder each belongs in. This class only keeps the assembly from being empty. Delete the folder once no lane adds to it.
namespace TW.Tests
{
    static class LandingFolder { }
}
