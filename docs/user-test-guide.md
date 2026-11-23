# User test guide: cleaningkit app

A short session with two or three data officers who have **not** used the app
before. The point is to see where a new user gets stuck. It is not a demo
and not a training session.

Plan for about **75 minutes per person**: 5 to set up, 50 for the tasks,
15 for questions, 5 spare. Test one person at a time, so nobody copies what
the person before them did.

## Before the session

**Who.** Data officers who clean 4Mi data today, ideally one who knows the R
scripts and one who doesn't. Nobody who helped build the app.

**Data.** Use a real round the officer already works with and is allowed to
handle: an ONA download, the XLSForm, and a second, later ONA download of the
same survey. Survey data is personal data, so follow the usual rules for
handling it.
- The online app keeps files only in that browser session and deletes them
  when the page is closed.
- Don't send anyone the files to "set up" the test, and don't keep the
  workbooks made during the test unless the officer needs them for their
  work.

**Setup.**
- Open the app (online link, or `cleaningkitapp::run_app()` locally) on the
  officer's own laptop if possible, with Excel installed.
- Have the three files in a folder they can find.
- Ask whether you may take notes. Don't record the screen if the data is on
  it.

**Roles.** One facilitator, who gives the tasks and stays quiet, and one note
taker, who fills in the note sheet at the end of this guide. One person can do
both if needed.

## What to say at the start

> We're testing the app, not you. If something is confusing, that's what we
> want to find. Please think aloud: say what you're looking for, what you
> expect to happen and what surprises you. I'll mostly stay quiet. If you're
> stuck for a couple of minutes I'll give you a hint, and that's useful for
> us too.

## The tasks

Read each task aloud and give it on paper. Don't name buttons or screens:
the wording below is deliberately plain.

### Task 1: Check a first round (about 15 minutes)

> This is the first time you check this survey. Here is the data you
> downloaded from ONA and the questionnaire. Find the problems in the data and
> get the file you would send to the reviewers.

Watch for:
- Do they find **Check new data** from the Start page?
- Do they understand "Have you checked data from this survey before?"
- Do they know which file is "the questionnaire (XLSForm)"?
- Do they read the sentence about how many surveys there are?
- Do they open **Change which checks run**? Do they need to?
- Do they understand why a check was **left out**?
- Can they say, in their own words, what the results tables show?
- Do they find and download **the workbook for reviewers**?

### Task 2: Review as a reviewer would (about 10 minutes)

> Now you're the reviewer. Open the workbook and deal with three of the
> problems: correct one value, remove one survey, and keep one as it is.
> Save the file under any name you like.

This task tests the workbook, not the app. Note anything in the workbook that
confused them: sheet names, drop-downs, colours, column names.

### Task 3: Apply the corrections (about 10 minutes)

> The reviewer has sent the workbook back. Produce the clean data.

Give them the **later** ONA download, not the one from task 1, to see if
they trust that it works.

Watch for:
- Do they find **Apply corrections**?
- Do they upload the reviewer's workbook (not the original one)?
- Do they understand the sentence about the round's surveys, and any warning
  about surveys changed or deleted on ONA?
- If a row can't be applied: do they understand why, and what to do next?
- Do they understand "the clean data matches the corrections"?
- Do they download **the final workbook**?

**Optional:** before task 3, change one action in their saved workbook to a
made-up word such as "fix it". See whether the app's explanation is enough
for them to fix it.

### Task 4: The next round (about 10 minutes)

> A week later there is a new ONA download. Check only the surveys you haven't
> checked yet.

Give them the later ONA download again.

Watch for:
- Do they work out that they need **last round's workbook**? Which one do
  they reach for: the reviewer workbook, the final workbook or the original
  download? Any of the app's workbooks works, so note which one they picked
  and why.
- Do they believe the number of new surveys?
- Do they look for the questionnaire? They don't need it in this round.

### Task 5: Something goes wrong (about 5 minutes, if time allows)

> Try to check new data, but give the app the questionnaire where it asks for
> the ONA download.

Ask them to read the message aloud and say what they would do next.

## Questions afterwards

Ask these openly. Don't suggest answers.

1. What was the hardest moment? What did you expect to happen there?
2. Was there a word or a sentence you didn't understand?
3. If you had to do this alone next week, which part would worry you?
4. What would you tell a colleague this app does, in one sentence?
5. Did you trust the results? Why, or why not?
6. Was there anything you wanted to do that the app didn't let you do?
7. *(If they know the R scripts)* What do you miss from the scripts?

## Note sheet

Copy one per person.

| | |
|---|---|
| Officer (initials only) | |
| Knows the R scripts? | yes / no |
| Online or local | |
| Date | |

| Task | Done without help? | Time | Hints given | Where they hesitated | Words they questioned |
|---|---|---|---|---|---|
| 1. First round | | | | | |
| 2. Reviewer | | | | | |
| 3. Apply corrections | | | | | |
| 4. Next round | | | | | |
| 5. Wrong file | | | | | |

**Error messages they saw**, copied word for word:

**Quotes worth keeping:**

## After all sessions

Put the findings in one list, worst first. For each finding, give:
- what happened;
- how many of the testers it happened to;
- the screen and the exact wording involved;
- a suggested change, if there is an obvious one.

A finding that stopped someone finishing a task comes first. Wording that
made someone hesitate comes next, then everything else. Bring the list back
so the fixes can be made in the app (the "then fixes" part of M8).
