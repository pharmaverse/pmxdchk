# pmxdchk UI Improvement Plan

A user-perspective review of the Shiny app (Phase 2/3 state) and a prioritized
plan to improve it. The app is organized as a `bslib::page_navbar` with a
persistent "Controls" sidebar and four tabs: Data / Overview / Profiles /
Findings.

## Top-level problems

1. **The workflow is not guided.** A first-time user lands on Data, sees an
   upload card, a blank Study type card, and a "Run checks" button isolated in
   the sidebar. There is no sense of step 1 -> 2 -> 3, and Run is easy to click
   prematurely.
2. **No empty-state guidance.** Before checks run, Overview / Profiles /
   Findings are blank with no instruction on what to do.
3. **Results go stale silently.** Changing thresholds or study type after a run
   does not invalidate the displayed results, and nothing tells the user.
4. **Findings (the core review surface) is hard to scan.** 41 rows, no colour
   coding by status/severity, no filtering, and long messages widen the table.

## Screen-by-screen review

### Sidebar "Controls"
- Run button is always enabled (even before upload) and shows no ready/not-ready
  state.
- Threshold labels are jargon without help text.
- No run status (last run time, study types / thresholds used, stale warning).

### Data — Upload & mapping card
- 24 select inputs stacked vertically in a 600px scroll card; tedious when most
  are auto-mapped.
- Confirm button sits at the very bottom of the scroll card.
- No core-variable validation: Confirm proceeds even if ID/TIME/EVID/MDV/DV/AMT
  are unmapped, and downstream checks then skip silently.

### Data — Preview card
- Shows the first 500 rows silently; the user may think it is the full dataset.

### Data — Study type card
- Blank until the mapping is confirmed (looks broken).
- The inferred table and the confirmation checkboxes are two separate blocks.
- `applicable` shows TRUE/FALSE instead of a clear indicator.

### Overview
- Plain value boxes, no icons/colour; blank before data is loaded.
- BLQ% / missingness% key metrics are not surfaced as boxes.
- No severity summary of findings (Overview should give the at-a-glance picture).

### Profiles
- Entirely blank before checks run, with no guidance.
- Plot has no title naming the subject; no "subject i of N" counter.
- Log-y silently drops DV <= 0 (BLQ / predose) points.
- The event table highlights flagged rows but does not say which check flagged
  them.

### Findings
- Table has no colour coding; pass/flag/skip and severity look identical.
- No filtering (by status / severity / domain).
- Table -> detail -> triage -> export are four stacked cards requiring much
  scrolling.
- Saving triage gives no immediate confirmation.

### Global
- No app-level description; a new user does not know what pmxdchk does.
- Default Bootstrap theme; no branding or dark mode.

## Prioritized plan

**P0 — usability blockers**
1. Empty-state guidance on every tab + Run button enable/disable by readiness.
2. Workflow ordering: numbered step headers on Data, a Run button at the end of
   the Data flow (in addition to the sidebar), and a confirmation prompt that
   points the user to the next step.
3. Core-variable mapping validation: live status and a disabled Confirm button
   until all core variables are mapped.
4. Stale-results notice: after thresholds / study type change, prompt a re-run.

**P1 — review efficiency**
5. Findings: colour coding, status/severity/domain filters, master-detail
   layout, flag-first sorting.
6. Overview: severity summary bar + BLQ% / missingness% metrics.
7. Profiles: subject title/counter, per-subject list of triggered checks, BLQ
   point handling under log-y.

**P2 — polish**
8. Mapping card: grouped/collapsible sections, two-column layout, summary badge.
9. Preview row-count note, threshold tooltips, theme / dark mode, About page.

## Implementation order

Start with P0 as one pass (empty states, workflow ordering, mapping validation,
stale notice), then P1 (Findings first), then P2.
