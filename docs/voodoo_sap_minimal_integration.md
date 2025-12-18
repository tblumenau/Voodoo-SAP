# Minimal-Change Integration Strategy for Voodoo SAP

Use these notes alongside the [ECC→S/4HANA table cross-reference](./voodoo_sap_tables.md). The goal is to place the smallest possible interception point in SAP—ideally a single table or view—so future upgrades or functional changes do not require rewrites.

## Principles for Choosing the Integration Point
- **Prefer stable, semantically rich tables/views.** CDS views like `I_OutboundDeliveryItem` or consolidated line-item tables like `MATDOC` change far less often than process-specific custom tables.
- **Read from the highest-status object you can tolerate.** The closer you attach to execution (warehouse task vs. delivery), the more signals you get, but the tighter the coupling. Start with delivery/production objects, drop down to warehouse tasks only if you need bin-level control.
- **Avoid duplicating SAP logic.** Intercept where SAP has already applied checks (availability, ATP, GI posting). This keeps your integration lean and resilient.
- **Pick one writable object per flow.** If you must write back, anchor on a single table (e.g., `MATDOC` postings or `/SCWM/ORDIM_O` confirmations) and keep everything else read-only.

## Quick Selector by Business Goal
- **Create shipments to customers (delivery creation & status).**
  - **Start:** `LIKP` / `I_OutboundDelivery` for headers; `LIPS` / `I_OutboundDeliveryItem` for items.
  - **When to stay here:** You need to launch picking or stage freight but do not control bin movements.
  - **When to go deeper:** You orchestrate picking waves—then add `/SCWM/ORDIM_O` (EWM warehouse tasks) for execution feedback.
- **Picking to a cart or tote (discrete pick tasks).**
  - **Start:** `/SCWM/ORDIM_O` for open warehouse tasks; `/SCWM/AQUA` if you need quant-level visibility.
  - **Why:** Tasks already capture source bin, destination bin, and quantity; you avoid recomputing stock placement.
  - **Write-back:** Confirm tasks via the same table/view path to keep SAP as the system of record.
- **Putwall / sortation by order.**
  - **Start:** `I_OutboundDeliveryItem` plus `/SCWM/ORDIM_C` (warehouse tasks to consolidation).
  - **Why:** Delivery items tell you what to accumulate; consolidation tasks capture the move into slots without touching picking logic.
  - **Tip:** Use delivery item status to close slots; rely on task confirmations to trigger light/put-to-wall events.
- **Kitting parts for manufacturing (component issue).**
  - **Start:** `I_ProductionOrderComponent` (formerly `RESB`) to see required vs. withdrawn quantities.
  - **Why:** It already tracks shortages and backflush; bin-level detail is optional unless you self-direct picking.
  - **If you direct picking:** Pair with `/SCWM/ORDIM_O` to drive the physical pick, but post the goods issue through `MATDOC` (261 movements) to avoid duplicating posting logic.
- **Replenishing pick faces.**
  - **Start:** `/SCWM/ORDIM_O` filtered to replenishment tasks; use `/SCWM/AQUA` for current bin stock.
  - **Why:** Lets SAP plan the move while your system sequences tasks and shows operators where to go.
- **Inventory visibility without control.**
  - **Start:** `I_StockOverview` or `MATDOC` for authoritative stock; avoid task tables unless you need in-flight quantities.

## How to Keep Changes Minimal
- **Read-first posture:** Default to read-only via RFC/ODATA or CDS views; only write when SAP requires a status or confirmation.
- **One choke point per process:** Pick a single table/view for each process leg—delivery, warehouse task, goods movement—so downstream changes in SAP (e.g., new WM strategies) do not ripple through your code.
- **Status-driven workflows:** Drive your state machine off SAP status fields (delivery picking status, WT status, confirmation flags) instead of duplicating states.
- **Configuration over code:** Use plant/storage-location filters, movement types, and warehouse numbers as configuration. Avoid hard-coding bin types or strategies; let SAP’s master data handle it.
- **Fallback to CDS names:** When in doubt, select the CDS view that matches the business object (e.g., `I_OutboundDeliveryItem`, `I_ProductionOrderComponent`) for a forward-compatible API surface.

## Decision Cheatsheet
- Need **shipping docs** and **post-GI visibility** → `LIKP`/`LIPS` + `MATDOC` (BWART=6xx)
- Need **pick execution and bin control** → `/SCWM/ORDIM_O`
- Need **sortation/putwall signaling** → `I_OutboundDeliveryItem` + `/SCWM/ORDIM_C`
- Need **component issue tracking** → `I_ProductionOrderComponent` + `MATDOC` (BWART=261)
- Need **stock snapshots** → `I_StockOverview` or `MATDOC`

Use this guide to choose the highest-leverage table or view for your scenario, minimize custom logic, and keep future upgrades frictionless.
