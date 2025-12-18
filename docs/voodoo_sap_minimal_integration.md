# Minimal-Change Integration Strategy for Voodoo SAP

This note is the companion to the [ECC→S/4HANA table cross-reference](./voodoo_sap_tables.md). Think of it as a map that helps you pick one small, sturdy place in SAP to read from (and occasionally write to) so you can plug in Voodoo without breaking every time SAP is patched. The tone here is intentionally beginner-friendly: if you are newer to SAP, follow the examples and you will stay on the happy path.

## How to Pick Your Anchor Point
Start by looking for tables or CDS views that already carry the meaning you need. Line-item hubs like `MATDOC` or semantic views like `I_OutboundDeliveryItem` change slowly because SAP treats them as canonical. Attach to the highest-level object you can live with. For instance, deliveries are more stable than warehouse tasks; production orders are steadier than bin movements. Only drop down a level when you truly need that detail.

Where possible, let SAP do the heavy lifting. If availability checks, ATP, or goods issue posting already happen in a table or view, read from there instead of re-creating that logic in your code. When you must send something back, choose a single writable object for that flow—`MATDOC` for goods movements or `/SCWM/ORDIM_O` for warehouse task confirmations—and keep the rest read-only. That pattern keeps your integration thin and predictable.

## Picking the Right Spot for Common Goals
Imagine you are deciding where to plug in for a particular outcome. The guidance below walks through the thinking process instead of throwing a checklist at you.

### Shipping to Customers (delivery creation and status)
Start with `LIKP` or the `I_OutboundDelivery` view for delivery headers, and `LIPS` or `I_OutboundDeliveryItem` for items. These objects already know about picking status, staging, and goods issue, so you can trigger your workflows without tracking bins yourself. Stay here if you just need to kick off picking or manage freight stages. If you are running picking waves and need to see every bin movement, add `/SCWM/ORDIM_O` so you can follow warehouse tasks in detail.

### Discrete Picking to a Cart or Tote
Point at `/SCWM/ORDIM_O` to read the open warehouse tasks. You get source bin, destination bin, and quantities without computing stock placement yourself. If you need to see the exact quant in a bin, pair it with `/SCWM/AQUA`. When you confirm the pick, write back through the same task table or view so SAP remains the system of record.

### Putwall or Sortation by Order
Use `I_OutboundDeliveryItem` to understand what each order expects, then watch `/SCWM/ORDIM_C` for the tasks that move items into consolidation slots. Delivery-item status tells you when to close a slot, while task confirmations make reliable signals for lights or put-to-wall events. You avoid touching the picking logic but still know what landed where.

### Kitting Parts for Manufacturing
Read `I_ProductionOrderComponent` (the CDS successor to `RESB`) to see required versus withdrawn quantities. SAP already tracks shortages and backflush, so you get a clean view of what is still owed. If you also direct the physical pick, combine that data with `/SCWM/ORDIM_O` tasks, but post the actual goods issue through `MATDOC` using 261 movement types. That keeps posting logic in SAP and keeps your integration simple.

### Replenishing Pick Faces
Filter `/SCWM/ORDIM_O` down to replenishment tasks to see what SAP wants moved. For a snapshot of what is currently in a bin, read `/SCWM/AQUA`. With those two pieces you can sequence the work for operators while SAP handles the planning and stock figures.

### Inventory Visibility Without Control
If you only need to know what stock exists, stick to `I_StockOverview` or `MATDOC`. These are authoritative for stock positions and do not pull you into warehouse-task complexity. Avoid task tables unless you must account for in-flight moves.

## Keeping the Integration Lightweight
Adopt a “read first” posture by default: pull data via RFC/ODATA or CDS views, and only write when SAP requires a confirmation or status. Choose one choke point per process leg—delivery, warehouse task, goods movement—so that a change in SAP’s internal strategies does not cascade through your code. Drive your workflow off SAP status fields (delivery picking status, warehouse task status, confirmation flags) instead of creating parallel state machines. Configure plant filters, movement types, and warehouse numbers rather than hard-coding bin strategies; let SAP’s master data carry the rules. When in doubt, favor the CDS view that matches the business object name (`I_OutboundDeliveryItem`, `I_ProductionOrderComponent`) for a forward-compatible API surface.

## Quick Reference
If you need a fast pointer after reading the narratives above, use this as a memory jog:

- Shipping documents and post-goods-issue visibility: `LIKP` / `LIPS` plus `MATDOC` (BWART=6xx)
- Pick execution with bin control: `/SCWM/ORDIM_O`
- Sortation or putwall signaling: `I_OutboundDeliveryItem` plus `/SCWM/ORDIM_C`
- Component issue tracking: `I_ProductionOrderComponent` plus `MATDOC` (BWART=261)
- Stock snapshots: `I_StockOverview` or `MATDOC`

Use this guide to select the smallest, strongest integration point for your scenario, minimize custom logic, and keep future SAP upgrades from turning into rewrites.
