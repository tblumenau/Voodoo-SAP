# Minimal-Change Integration Strategy for Voodoo SAP

This note complements the [ECC→S/4HANA table cross-reference](./voodoo_sap_tables.md). It identifies stable tables and CDS views you can rely on so Voodoo integrations remain steady across SAP patches. Follow the examples to anchor your read and write points in predictable places.

## How to Choose an Anchor Point
Begin with tables or CDS views that already represent the business meaning you need. Line-item hubs like `MATDOC` or semantic views like `I_OutboundDeliveryItem` are treated as canonical and change slowly. Prefer the most aggregated object that meets the requirement: deliveries remain steadier than warehouse tasks, and production orders shift less than bin movements. Move to lower-level objects only when that detail is required.

Reuse SAP logic wherever it already exists. If availability checks, ATP, or goods issue posting appear in a table or view, read from that source instead of rebuilding the logic. When you must write back, pick one object for that flow—`MATDOC` for goods movements or `/SCWM/ORDIM_O` for warehouse task confirmations—and keep the rest read-only. This keeps integrations narrow and easier to maintain.

## Picking the Right Spot for Common Goals
Use the scenarios below to match your integration point to the outcome you need.

### Shipping to Customers (delivery creation and status)
Use `LIKP` or the `I_OutboundDelivery` view for delivery headers, and `LIPS` or `I_OutboundDeliveryItem` for items. These sources include picking status, staging, and goods issue, so you can trigger workflows without modeling bins. Stay here for delivery-driven flows. If you need bin-level movements—for example, during picking waves—supplement with `/SCWM/ORDIM_O` to follow warehouse tasks.

### Discrete Picking to a Cart or Tote
Use `/SCWM/ORDIM_O` to read open warehouse tasks. It provides source bin, destination bin, and quantities without computing stock placement yourself. If you need to see the exact quant in a bin, pair it with `/SCWM/AQUA`. Confirm picks through the same task table or view so SAP remains the system of record.

### Putwall or Sortation by Order
Read `I_OutboundDeliveryItem` to determine each order’s expected items, then monitor `/SCWM/ORDIM_C` for tasks that move items into consolidation slots. Delivery-item status indicates when to close a slot, and task confirmations signal put-to-wall events. This keeps pick execution untouched while still tracking where items landed.

### Kitting Parts for Manufacturing
Read `I_ProductionOrderComponent` (the CDS successor to `RESB`) to see required versus withdrawn quantities. SAP tracks shortages and backflush, so you get an accurate picture of remaining needs. If you direct the physical pick, combine that data with `/SCWM/ORDIM_O` tasks, but post the goods issue through `MATDOC` using 261 movement types. Posting through SAP keeps the integration straightforward.

### Replenishing Pick Faces
Filter `/SCWM/ORDIM_O` to replenishment tasks to see what SAP plans to move. For a snapshot of bin contents, read `/SCWM/AQUA`. With those two sources you can sequence operator work while SAP manages planning and stock balances.

### Inventory Visibility Without Control
If you only need to know what stock exists, use `I_StockOverview` or `MATDOC`. These are authoritative for stock positions and avoid warehouse-task complexity. Pull in task tables only when you must account for in-flight moves.

## Keeping the Integration Lightweight
Default to reading data via RFC/ODATA or CDS views, and write only when SAP requires a confirmation or status. Choose one integration point per process leg—delivery, warehouse task, goods movement—so changes in SAP internals do not ripple through your code. Drive workflows from SAP status fields (delivery picking status, warehouse task status, confirmation flags) instead of building parallel state machines. Configure plant filters, movement types, and warehouse numbers rather than hard-coding bin strategies; keep the rules in SAP master data. When deciding between sources, prefer the CDS view that matches the business object name (`I_OutboundDeliveryItem`, `I_ProductionOrderComponent`) because those views stay stable across releases.

## Quick Reference
If you need a fast pointer after reading the narratives above, use this as a memory jog:

- Shipping documents and post-goods-issue visibility: `LIKP` / `LIPS` plus `MATDOC` (BWART=6xx)
- Pick execution with bin control: `/SCWM/ORDIM_O`
- Sortation or putwall signaling: `I_OutboundDeliveryItem` plus `/SCWM/ORDIM_C`
- Component issue tracking: `I_ProductionOrderComponent` plus `MATDOC` (BWART=261)
- Stock snapshots: `I_StockOverview` or `MATDOC`

Use this guide to pick concise integration points, minimize custom logic, and reduce rewrites during SAP upgrades.
