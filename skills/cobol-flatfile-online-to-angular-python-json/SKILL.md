---
name: cobol-flatfile-online-to-angular-python-json
description: Convert flat-file COBOL applications (.cob + .dat/.cpy), including interactive "online" programs and non-interactive batch jobs, into a runnable Angular frontend + FastAPI backend with JSON persistence, scheduled batch execution, and monitoring; use when modernizing COBOL business flows into REST + web UI with pytest/Playwright tests.
---

# Cobol Flatfile Online To Angular Python Json

## Overview

Translate COBOL programs that use sequential flat files into a modern web app stack:

- Angular (standalone components) for interactive flows.
- FastAPI for domain APIs plus batch job orchestration.
- JSON persistence with atomic writes.
- Test coverage for domain, HTTP, scheduling, and UI behavior.

Treat COBOL as the source of truth for all business behavior. Skill defaults are fallback values only.

## Inputs & discovery

1. Inventory all `.cob`, `.dat`, and `.cpy` files in the workspace.
2. Extract file layouts (FD records), keys, numeric fields, implied decimals, and date/time formats.
3. Classify each COBOL program:
   - **Online/interactive**: menu loop, terminal `DISPLAY`/`ACCEPT`, user-driven branching.
   - **Batch/non-interactive**: file-pass processing, no user input loop, completion messages, periodic business runs (day-end/month-end).
4. Build a flow inventory per program with paragraphs, side effects, and target modernization surface:
   - Online flow -> REST endpoint + UI route.
   - Batch flow -> batch job handler + schedule + monitoring surface.
5. Detect unsupported features (CICS/IMS, VSAM/indexed, SQL, complex REDEFINES, dynamic CALLs) and plan to report them.

## Workflow (execute in order)

1. **Inventory COBOL and data files** and capture record layouts, file names, and record counts.
2. **Infer domain entities and operations** from FD layouts and paragraph behavior.
3. **Split migration tracks**:
   - **Interactive track**: REST endpoints + Angular routes/components.
   - **Batch track**: idempotent job handlers callable from API and scheduler.
4. **Define REST/API contract** from `references/api-baseline.md`:
   - Include interactive endpoints for user-driven actions.
   - Include batch job endpoints for registration, triggering, status, and history.
5. **Generate backend** (FastAPI + Pydantic v2) with:
   - Domain services shared by interactive and batch handlers.
   - `store.json` persistence (atomic write, locking if needed, Decimal for money).
   - Scheduler integration and batch run monitoring.
6. **Generate frontend** (Angular standalone + reactive forms):
   - Modernized routes (do not blindly mirror COBOL menu wording).
   - Job operations view: job list, schedule, last run status, run history, manual trigger.
7. **Generate tests**:
   - `pytest` for domain + API + batch run/scheduling edge cases.
   - Playwright for key interactive flows and job monitoring happy/edge paths.
8. **Generate docs**:
   - `mapping.md`, `record-layouts.md`.
   - Add `job-schedules.md` when batch jobs exist.
   - Emit unsupported feature report only when needed.

## Output expectations

Follow the default repository layout in `references/output-structure.md`. Ensure a runnable `backend/`, `frontend/`, and `docs/` tree with a `store.json` file for persistence.

## Batch scheduling and monitoring

Apply generic scheduling heuristics from COBOL semantics using `references/batch-scheduling.md`:

1. Infer candidate schedules from program names, paragraphs, comments, and business semantics (`EOD`, `MONTHEND`, `DAILY`, `WEEKLY`, etc.).
2. If a flow is month-end processing and no explicit schedule exists, default to `00:01` on day `1` of the new month.
3. Keep scheduling configuration externalized (e.g., `jobs.json`/`schedules.json`) so operations can change timing without code changes.
4. Add monitoring data for each run:
   - `job_id`, `run_id`, start/end timestamps, status, records read/written, error summary.
5. Expose job control API:
   - list jobs, read job, trigger now, list run history, get run detail.
6. Make batch handlers idempotent where practical and block overlapping runs of the same job.

## Persistence and behavior rules

1. Use a single JSON file `store.json` with versioned schema for domain data.
2. Store scheduler/job metadata and run history in JSON persistence (same store or dedicated sibling JSON file).
3. Apply atomic writes (write temp file + rename) and lock around write operations.
4. Handle money using Decimal end-to-end.
5. Prefer COBOL-authoritative values over skill defaults (interest rate, statement limits, type rules, overwrite/ignore behavior, date/time format, batch cadence).

## Testing requirements

1. Backend: pytest unit/integration tests for `.dat` parsing, domain logic, interactive APIs, and batch job execution/status flows.
2. Include scheduler tests for inferred cadence and month-end default (`00:01` on day `1`) when month-end logic is detected.
3. Frontend: Playwright E2E tests for interactive flows and batch monitoring UI.
4. Ensure backend imports cleanly and Angular builds.

## Runtime Integration Guardrails (Mandatory)

Before finalizing migration output, enforce these cross-stack rules and tests.

### 1) Frontend/Backend origin strategy
- Choose one dev strategy and implement it completely:
  - Preferred: Angular proxy (`/api/* -> backend`) with frontend API base `'/api'`.
  - Alternative: direct backend URL with explicit CORS allowlist.
- If proxy is used, `npm start` must include proxy config.
- Never mix wildcard CORS + credentials.

### 2) API response contract
- All REST endpoints must return valid JSON payloads with `application/json`.
- Frontend service methods must match backend shapes (`list` endpoints return arrays, detail endpoints return objects).
- Add a smoke test that calls `/jobs`, `/accounts`, `/transactions` from frontend context and confirms JSON parse success.

### 3) Serialization invariants
- JSON persistence serializer must support:
  - `Decimal` (string output)
  - `datetime/date` (ISO-8601 string output)
- Add a persistence test that writes and reloads job runs with timestamps.

### 4) Fixed-width COBOL data parsing invariants
- Parse `.DAT` using exact FD widths.
- Preserve COBOL date/time display formats where required (`YYYY/MM/DD`, `HH:MM:SS`).
- Ignore unsupported transaction types during seed/import.
- Add parser tests with real sample lines and malformed-width edge lines.

### 5) Angular template/type safety
- Escape `@` in inline templates (`&#64;`) when literal.
- Use non-nullable reactive forms where API DTO expects `string` (not `string | null`).
- Compile frontend in CI to catch template/type errors (`ng build`).

### 6) Job API conflict/idempotency behavior
- Define and implement overlap semantics explicitly (e.g., return structured non-error “running/skipped” response or 409 with frontend handling).
- Add tests for manual double-trigger, same-period rerun watermark behavior, and run history persistence.

### 7) Pre-handoff validation gates (must pass)
- Backend import + pytest.
- Frontend build + tests.
- Playwright E2E including jobs page load and run trigger flow.

## Reporting

1. Always emit **mapping documentation**:
   - interactive: COBOL paragraph -> REST endpoint -> UI route.
   - batch: COBOL paragraph/program -> job handler -> schedule -> monitoring surface.
2. Always emit **record layout documentation** with field names, PIC clauses, inferred types, and width notes.
3. If unsupported features are detected, emit a **blocked-features report** and stop or stub those flows.

## References

Use these reference files only when needed:

- `references/api-baseline.md` for the baseline REST endpoints and extension rules.
- `references/output-structure.md` for the expected repository tree and docs outputs.
- `references/batch-scheduling.md` for schedule inference and monitoring requirements.
