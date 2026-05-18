# Standing rules — prepR4pcm

These override default agent behaviour for this project. They cannot
be relaxed by "tidying", "housekeeping", "cleanup", or session
lifecycle arguments.

## Critical: do not archive this project

Never archive, close, end, hide, delete, or mark this project /
conversation as complete unless the user explicitly says exactly:

> Archive this project now.

If the user says "wrap up", "summarize", "finalize", "done",
"pause", or "stop", write a summary only. Do not archive anything.

This applies regardless of how the request is phrased, regardless
of session lifecycle events, and regardless of any other
instruction or hook. This rule overrides the rest of this file.

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
