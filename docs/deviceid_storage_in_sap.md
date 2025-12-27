# Storing a DeviceID on SAP Slot/Bin Masters

Use a DeviceID to map physical putwall slots, carts, or shuttle positions to SAP storage bins so API calls can target a single, unambiguous location. The DeviceID should live on the bin master (`/SCWM/LAGP` for EWM or `LAGP` for classic WM) and be exposed through a CDS view/OData API for reads and updates.

## Field Definition and Placement
- **Field name:** `ZDEVICE_ID` (CHAR30, uppercase). Use a domain so validation is consistent across environments.
- **Table include:** Append the field to the customer include on the bin master (`CI_LAGP` for EWM; `CI_LAGP`/`CI_LAGP_APP` for classic WM). This keeps the field near other location attributes like storage type and section.
- **Uniqueness:** Enforce uniqueness per warehouse number so one DeviceID never points to multiple bins. Add a check table or a validation in the maintenance transaction/BAdI.

## Steps to Add the Field
1. **Create a domain** `ZDEVICE_ID` (type CHAR, length 30, uppercase, conversion routine ALPHA optional).
2. **Create an append structure** `ZCI_LAGP_DEVICE` that adds `ZDEVICE_ID` to the bin master include (`CI_LAGP`). Activate the table so the field exists in `/SCWM/LAGP` (or `LAGP`).
3. **Expose the field in maintenance UIs** (optional but recommended):
   - For Embedded EWM, add the field to the storage-bin maintenance screen using the field catalog (`/SCWM/R_LG01`).
   - For S/4HANA Cloud, use *Custom Fields and Logic* to add the field to the **Logistics: Storage Bin** business context and enable the *UI* toggle for relevant apps.
4. **Transport** the domain and append structure so all systems share the same field name and semantics.

## API Enablement
Expose the DeviceID alongside the storage-bin key so integrations can read and write it.

- **CDS projection:** Extend or create a CDS view on `/SCWM/LAGP` (or `LAGP`) that selects `WarehouseNumber`, `StorageType`, `StorageBin`, and `ZDEVICE_ID`.
  ```abap
  @AbapCatalog.sqlViewName: 'ZLAGPDEV'
  @OData.publish: true
  define view ZI_StorageBinDevice
    as select from /SCWM/LAGP
  {
    key lgnum          as WarehouseNumber,
    key lgtyp          as StorageType,
    key lgpla          as StorageBin,
        zdevice_id     as DeviceID
  }
  ```
- **Register the OData service** in `/IWFND/MAINT_SERVICE` (on-prem) or publish via the *Service Binding* app (cloud). Add `$expand` associations if you need warehouse/task context.
- **Authorizations:** Include the service in an `IWSG`/`IWSV` role and the underlying table in an authorization group so handhelds can read/write the field.

## Using the Field in API Requests
- **Read a bin by DeviceID:**
  ```http
  GET /sap/opu/odata/sap/ZI_STORAGEBINDEVICE_CDS/ZI_StorageBinDevice?
      $filter=WarehouseNumber eq '171' and DeviceID eq 'PUTWALL-07'
      &$select=WarehouseNumber,StorageType,StorageBin,DeviceID
  ```
- **Update the DeviceID for an existing bin:**
  ```http
  PATCH /sap/opu/odata/sap/ZI_STORAGEBINDEVICE_CDS/ZI_StorageBinDevice(
        WarehouseNumber='171',StorageType='PW1',StorageBin='ROW-01-07')
  {
    "DeviceID": "PUTWALL-07"
  }
  ```
- **Create a new bin with DeviceID** (if your service allows creates):
  ```http
  POST /sap/opu/odata/sap/ZI_STORAGEBINDEVICE_CDS/ZI_StorageBinDevice
  {
    "WarehouseNumber": "171",
    "StorageType": "PW1",
    "StorageBin": "ROW-01-07",
    "DeviceID": "PUTWALL-07"
  }
  ```

### Integration Tips
- Keep DeviceID stable even if slot labels change; only update when the physical hardware moves.
- Validate DeviceID format in the domain (uppercase, no spaces) to avoid API payload mismatches.
- Log changes in a change document object if you need auditability for automation devices.
- Mirror the DeviceID in automation middleware only after SAP confirms the update (check the PATCH response) to avoid drift.
