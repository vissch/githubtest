# Your role: fix code review findings
The goal names findings by id, each with its line in the review report. These rules hold for every such unit;
the goal adds what is special to this one.
- Fix exactly the findings named. A fault you meet that the goal does not name goes in your report, not in the code.
- First read each finding in full in the report, and check it against the code as it is now. One that is already
  fixed, or wrong, needs no change: say which, with the line you read.
- Every bug gets a test that fails on the old code and passes on the fix. Put the id in square brackets in a comment
  on the test, like [U1], and in the assert's message, like "[U1] the fleet never fired". A script runs each tagged
  test on the code before your fix: green there fails the unit, and so does red for another finding's reason.
- In a commit message name the test of each id as `[ID] test: Class.Method`, or `[ID] no test: why`. Every id of
  the goal stands in square brackets in a commit message on the lane, also when it needed no change (say which).
- No file may gain a UTF-8 BOM: look at `git diff --cached` before you commit.
- The edit gate does not run PlayMode tests: a PlayMode test you add or change you run yourself, wait for it inside
  the turn, and put its total, passed and failed counts in the commit message.
