# Inbox: notes from one session to another

One note per file, named `<date>-<to>-<topic>.md`, where `<to>` is the lane it is for with `/` as `-`
(`show-aosa`, `sim-units-meta`) or `all`. A file per note means two sessions writing notes never conflict.
`python Tools/health.py` lists the notes on your branch and on the integration branch, and marks the ones for you.

Write one when another session must know something: you changed a file they are editing, their branch will conflict,
a decision affects them. Keep it short and say what to do. **The receiver deletes the file once it is done**, in its
own commit, so this folder is always the list of what is still pending.

A fact that is always true about the code does not belong here: put it in a comment, a `tasks.md` Trap line, or a test.
