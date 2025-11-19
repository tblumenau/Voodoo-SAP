# ECC 6.0 → S/4HANA Cross-Reference Tables  
**Inventory, Kitting, and Production-Order Picking**

## 🔹 1. Inventory Management (IM)
| Business Purpose | ECC 6.0 Table | S/4HANA Replacement | Notes |
|------------------|--------------|----------------------|-------|
| Goods-movement header | MKPF | V_MKPF → MATDOC | In S/4HANA, header+item merged into MATDOC; MKPF kept as a view. |
| Goods-movement item | MSEG | MATDOC | Unified line-item source; replaces older stock tables. |
| Plant/Storage-location stock | MARD | MARD; CDS I_MaterialStock / I_StockOverview | MARD is no longer fully authoritative. |
| Valuated stock | MBEW | MBEW (view) | Still active; valuation optimized on HANA. |
| Batch stock | MCHB | MCHB (view) | Replaced by CDS I_BatchStock. |

## 🔹 2. Reservations / Requirements (Kitting Inputs)
| Purpose | ECC 6.0 | S/4HANA | Notes |
|--------|----------|----------|-------|
| Reservation header/item | RESB | V_RESB → I_ProductionOrderComponent | Still queryable; CDS preferred for analytics. |
| Planned order components | PLAF/PLAS | I_PlannedOrderComponent | Removes need for PLAF joins. |
| Stock determination | RESB + MARD | I_StockProvidedToOrder | Combines reservation + on-hand stock. |

## 🔹 3. Production / Manufacturing Orders
| Purpose | ECC 6.0 | S/4HANA | Notes |
|---------|----------|----------|-------|
| Order master | AUFK | AUFK (unchanged) | Cost-object logic unchanged. |
| Order header | AFKO | AFKO (view) + I_ProductionOrder | CDS recommended. |
| Order operations | AFVC | AFVC (view) + I_ProductionOrderOperation | Links to routing/work-center. |
| Order confirmations | AFRU | AFRU (view) + I_ProductionOrderConfirmation | For partial/completed postings. |
| BOM header/items | STKO/STPO | STKO/STPO (views) + I_BillOfMaterialItem | Keys simplified. |

## 🔹 4. Warehouse / Picking (EWM or Classic WM)
| Function | ECC 6.0 | S/4HANA (Embedded EWM) | Notes |
|----------|----------|-------------------------|-------|
| Transfer order | LTAK/LTAP | /SCWM/ORDIM_H / ORDIM_O / ORDIM_C | Classic WM replaced by EWM. |
| Bin master | LAGP | /SCWM/LAGP | Extended attributes. |
| Quant data | LQUA | /SCWM/AQUA | Different key structure. |

## 🔹 5. Outbound / Delivery Kitting (SD→EWM)
| Purpose | ECC 6.0 | S/4HANA | Notes |
|---------|------------|----------|-------|
| Delivery header | LIKP | LIKP (view) + I_OutboundDelivery | Structural continuity. |
| Delivery item | LIPS | LIPS (view) + I_OutboundDeliveryItem | Supports open-picking visibility. |
| Picking fields | PSTYV/PIKMG | PickingStatus / PickedQuantity | One‑to‑one mapping. |

## 🔹 6. Summary Mapping for Automation
| Automation Target | ECC 6.0 Read From | S/4HANA Read From |
|-------------------|------------------|-------------------|
| Components still to pick | RESB (BDAUG < BDMNG) | I_ProductionOrderComponent (WithdrawalQty < RequirementQty) |
| Stock at bin | MARD + LQUA | I_StockOverview / /SCWM/AQUA |
| Posted issues (261) | MSEG | MATDOC (BWART=261) |
| Delivery items | LIPS | I_OutboundDeliveryItem (PickingStatus='A') |
| Warehouse tasks | LTAP | /SCWM/ORDIM_O |
