# Plan: a simpler cleaning app (files in, files out)

**Goal:** someone who has never used R, and who doesn't know what a "uuid"
or a "ledger" is, can clean a 4Mi round in two sittings by uploading files
and downloading files. They never handle a project folder.

## 1. What a new user meets today

Walking through the app as someone new to it:

| Step | What they have to do now | Why it's hard |
|---|---|---|
| Project | Choose or create a folder, then "Set up project folders" with `data/`, `resources/`, `output/` | Folder structure is a developer idea. Online it's replaced by a workspace zip they must remember to download. |
| Settings | Type `_uuid`, `username`, `today`, tick "first row holds ONA labels" | Jargon. The defaults are right for almost every 4Mi export, but nothing says so. |
| Tool | Upload the XLSForm, pick the survey and choices sheets, a label language and the "other" text types | Four decisions before anything happens. |
| Data | "Keep only new records", the ledger, archive, at most two Excel files in `data/`, and the bigger one is taken as the new one | The next-round logic (section 2) depends on files sitting in a folder, and it moves and renames them. |
| Checks | 14 checks, each with its own arguments | Fine for an analyst. Too much as the first thing a new user sees. |
| Review workbook | Reviewers must save their copy with a name ending in `_edited` | A file-naming rule that's easy to forget, and the round fails if it is. |
| Import reviewed | Reload exactly the data that was validated, with "Keep only new records" off | The easiest step to get wrong, and the app can't tell if you did. |
| Throughout | uuid, combined log, evaluation, restore types, run log | Words from the code, not from the job. |

## 2. How the next round works today, and why it breaks

`cleaningkit::filter_new_records()` works on the `data/` folder:

- It expects at most two Excel files: the new ONA download and the file left
  by the previous round. It takes the bigger one as the new download.
- It removes surveys whose uuid appears in the previous file or in
  `data/processed_uuids.csv` (the ledger). It writes `data.xlsx` and moves
  the two inputs to `data/archive/`.

That works for one careful person working from one folder. It breaks when:

- **The folder isn't there.** That's always the case online, and true for a
  new laptop or a colleague taking over. The ledger is a CSV inside the
  folder, so losing the folder loses the memory of which surveys were
  checked.
- **Files are in the wrong place.** A third Excel file in `data/`, or a
  smaller new download (for example after records were deleted on ONA),
  confuses the "bigger file is newer" rule.
- **The Clean step needs the exact file that was validated.** By then
  `filter_new_records()` has renamed and archived it, and the user has to
  find it again.

## 3. The idea: the workbook carries the round's memory

Every file the app produces already travels: reviewers get the review
workbook, and the final workbook is the deliverable. So the memory of the
round goes **inside those workbooks**, in a hidden sheet, instead of in a
folder. cleaningkit already does this for the macro's settings (the hidden
`config` sheet), so it's an existing pattern.

A very hidden sheet `ck_round` in the review workbook and the final
workbook holds:

- **Round**: the round number and date, plus the cleaningkit and app
  versions.
- **Surveys checked so far**: every uuid checked in this and earlier rounds,
  with the round it was checked in. This replaces `processed_uuids.csv`.
- **This round's surveys**: the uuids checked in this round, each with a
  fingerprint (a hash of the row's values). The fingerprint lets the app
  notice later if a survey was changed on ONA.
- **Settings**: the column names, the check settings and the review options
  (today's `cleaningkitapp.yml`, as text).
- **Questionnaire**: the XLSForm's survey and choices sheets, and the logical
  checklist if one was used.

There are no names, answers or GPS in this sheet. It holds IDs and hashes,
plus a copy of the questionnaire.

**What the user carries from round to round is then one file: the last
workbook the app gave them.**

| Round | Files to upload |
|---|---|
| First round, checking new data | The ONA download and the XLSForm |
| Every later round, checking new data | The new ONA download and the last round's review or final workbook |
| Applying corrections | The reviewers' edited workbook(s) and an ONA download: the same one, or any later one |

Why "any later ONA download" works for applying corrections: the edited
workbook lists exactly which surveys were in the round. Every ONA download
contains the whole dataset so far, so the app picks those surveys out of
whatever download it gets. The fingerprints then catch the one real risk.
For example: *"3 surveys were changed on ONA after they were checked. Their
old values may not match the log."* Or: *"2 surveys are no longer in the
download."*

**Existing projects** can still start the new way. The "what did you check
last time?" upload also accepts `processed_uuids.csv`, last round's
`data.xlsx`, or a final workbook made before this change. Several files can
be uploaded and are combined.

**If the hidden sheet is lost,** for example because a reviewer copied the
log into a new workbook, the app says so plainly and asks for another
earlier file. The final workbook always has a copy too.

## 4. The new flow

### Start page

Two large choices and nothing else:

> **Check new data**: find problems in a new ONA download and make the
> workbook for your reviewers.
>
> **Apply corrections**: take the reviewers' workbooks back and produce the
> clean data.

A third, smaller link, **Try it with example data**, runs a round on
built-in synthetic data, for training and for partners trying the app.

### Check new data (three steps)

1. **Upload**
   - The new ONA download, which is required.
   - Last round's workbook, under "Is this your first round?". If yes, they
     upload the XLSForm instead.
   - The app answers in one sentence: *"1,240 surveys in this download.
     1,100 were checked in rounds 1–6. **140 are new.**"*
   - Column names and the ONA label row are **detected**, not asked. The app
     only asks if `_uuid`, `username` or `today` is missing.
2. **Check**
   - One button, **Run the standard 4Mi checks**, with defaults on.
   - Individual checks and their arguments move under "Change which checks
     run".
   - Progress shows as plain sentences ("Checking interview duration…").
3. **Download**
   - A short summary: problems found, top enumerators to follow up.
   - One button: **Download the workbook for reviewers**.
   - The `_edited` naming rule goes away: reviewers can save under any name.

### Apply corrections (three steps)

1. **Upload**
   - The edited workbook(s) and an ONA download, which can be the same one or
     a later one.
   - Nothing else is needed: the questionnaire and settings come from the
     workbook.
2. **Review problems**
   - Rows the app can't apply ("no action chosen", "unknown action") show in
     a table with **Download a copy for the reviewer to fix**.
   - Warnings sit under a fold.
   - If nothing blocks, a single **Apply** button.
3. **Download**
   - The final workbook, which also carries the round memory for next time.
   - Any disagreement between raw data, clean data and the log, in plain words.

### What disappears from the user's view

- **The Project screen and the workspace zip.** Nothing needs to be kept in
  between: the workbooks carry the memory.
- **The settings card.** Settings are detected and live in the workbook.
  "Advanced settings" stays one click away.
- **The Run log as a main tab.** It moves to "Show details" on each step.
- **Developer words.** The interface uses this wording instead:

  | Now | Becomes |
  |---|---|
  | Tool | Questionnaire (XLSForm) |
  | uuid | survey ID |
  | Review workbook | workbook for reviewers |
  | Combined log / evaluation | (not shown) |
  | Discard | remove survey |
  | restore types | (default on; under Advanced) |

## 5. Where the changes go

**cleaningkit (the package), so scripts users get it too:**
- `create_review_workbook()` and `export_final_output()` get a `round_info`
  argument that writes the `ck_round` sheet.
- A new `read_round_info(paths)` reads it from one or more workbooks, or from
  a ledger CSV or an old `data.xlsx`.
- A new in-memory `new_records(dataset, checked_uuids)` sits next to
  `filter_new_records()`. The folder version stays as it is for script
  users.
- `round_fingerprint(dataset)` computes the per-survey hashes.
- Tests, a NEWS entry and a CalVer bump.

**cleaningkitapp:**
- A start page and two step-by-step flows. These replace the navbar of
  eight screens, and the existing helpers (`check_registry`,
  `clean_helpers`, `review_helpers`) are reused almost as they are.
- Everything is uploads and downloads, in hosted **and** local mode. That
  means one code path. Hosted mode keeps its per-session temporary folder,
  which is now internal and never shown.
- Columns and the label row are detected automatically.
- Plain error messages for the common failures: wrong file type, a sheet
  missing from the XLSForm, a workbook from another project, a download that
  doesn't contain the round's surveys.
- Example data and a "Try it" mode.
- Removed: `mod_project.R`, the workspace zip, `shinyFiles`, and the
  `file_pattern` rule.

## 6. Milestones

| | What | Size |
|---|---|---|
| M5 | cleaningkit: the round sheet, the reader, `new_records()`, fingerprints, tests | 2–3 days |
| M6 | App: start page, "Check new data" flow, detection, round sheet written | 4–5 days |
| M7 | App: "Apply corrections" flow from the workbook and any ONA download, fingerprint warnings, migration from the old ledger | 3–4 days |
| M8 | Plain language, error messages, example data; a test session with 2–3 data officers who haven't used it, then fixes | 3–4 days |
| M9 | Publish on Connect Cloud | ½ day |

## 7. Decisions (agreed 23 Nov 2026)

1. **Folder mode:** the hosted app is upload/download only. When the app runs
   locally (`run_app()`), the project-folder mode stays, alongside the
   upload flow. Script users keep cleaningkit's folder functions.
2. **Hidden copy:** yes. The workbooks carry the questionnaire and the full
   list of survey IDs already checked. This is needed because the dataset in
   a review workbook holds only that round's surveys, not the whole survey.
3. **No standard XLSForm:** regions add their own modules, so the form always
   differs. The first round always uploads the XLSForm. Later rounds take it
   from the hidden copy, and a new upload replaces it when the form changes.
4. **Round names are free text:** the user names each round however they
   like ("R7", "Nov week 2", "2026-11-20"). The app suggests the date but
   never imposes a numbering scheme, and orders rounds by the date they
   were checked, not by name.

## 8. Risks

- **The hidden sheet is lost** if a reviewer rebuilds the workbook instead
  of editing it, or round-trips it through Google Sheets. Mitigations: the
  final workbook carries a copy, the old files are accepted, and the app
  says exactly what's missing.
- **The wrong "last workbook" is uploaded,** for example an older round. The
  app shows the round and date it found ("This workbook is from round 4,
  12 Nov"), and uploading several workbooks combines them. Uploading every
  past workbook is always safe.
- **Changes on ONA between checking and applying corrections.** The
  fingerprints detect this. The app warns but doesn't block, because the
  reviewer may have made the change on ONA on purpose.
