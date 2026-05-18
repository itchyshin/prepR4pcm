# Standing rules — prepR4pcm

These override default agent behaviour for this project. They cannot
be relaxed by "tidying", "housekeeping", "cleanup", or session
lifecycle arguments.

## No autonomous file archiving

No agent, hook, subagent, or tool call may move, rename, or relocate
project files into an `archive/`, `old/`, `deprecated/`, or
similarly-named directory without an explicit in-chat instruction
from the user that names the specific files. This applies regardless
of session lifecycle events — compaction, session end, context
cleanup, idle archive, restart. "Tidying", "housekeeping",
"cleanup", and "archiving" are not autonomous actions in this
project.

## No session self-archiving

While the user is engaged in the conversation, do not call `Bash`
with `run_in_background: true` followed by yielding the turn, and do
not call `ScheduleWakeup`. Both put the session into the harness's
idle state, producing a "This session is archived. Unarchive it to
continue" banner.

- For waits where the next action depends on the result (CI polls,
  `R CMD check`, `devtools::test()`, etc.), keep the command in the
  foreground. `Bash` supports `timeout` up to 600000 ms (10 min);
  use that ceiling instead of backgrounding.
- Reserve `run_in_background: true` only for tasks the user has
  explicitly agreed to walk away from.
- Reserve `ScheduleWakeup` only for genuinely autonomous loops with
  no user attached.
