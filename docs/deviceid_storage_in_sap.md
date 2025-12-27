# Storing a DeviceID on SAP Slot/Bin Masters

Use a DeviceID to map physical putwall slots, carts, or shuttle positions to SAP storage bins so API calls can target a single, unambiguous location. The DeviceID should live on the bin master (`/SCWM/LAGP` for EWM or `LAGP` for classic WM) and be exposed through a CDS view/OData API for reads and updates.

## Field Definition and Placement
- **Field name:** `ZDEVICE_ID` (CHAR30, uppercase). Use a domain so validation is consistent across environments.
- **Table include:** Append the field to the customer include on the bin master (`CI_LAGP` for EWM; `CI_LAGP`/`CI_LAGP_APP` for classic WM). This keeps the field near other location attributes like storage type and section.
- **Uniqueness:** Default to uniqueness per warehouse number when each hardware endpoint must map to one slot. If a single device legitimately services multiple adjacent bins, allow controlled duplicates—make the check table/validation configurable by storage type or zone so you can opt in to one-to-many mapping only where required.

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
- **Single-device command** (use when targeting one DeviceID): `POST /api/device/{id}/` with the DeviceID in the path and a command body matching the OpenAPI spec.
  Include the `arrow` indicator supported by the [Voodoo Robotics REST API](https://voodoorobotics.com/rest-api/) to point toward the slot (for example `"up"`, `"left"`, `"topright"`, or `"bottomleft"`).
  ```http
  POST /api/device/D4E825:8B665D/
  {
    "deviceid": "D4E825:8B665D",
    "command": "flash",
    "line1": "Pick Item",
    "line2": "Aisle 3",
    "sound": "beep",
    "seconds": 10,
    "color": "red",
    "arrow": "up"
  }
  ```
- **Batch commands** (send multiple DeviceIDs in one request): `POST /api/devices/` with an array of command arrays.
  ```http
  POST /api/devices/
  [
    [
      {
        "deviceid": "D4E825:8B665D",
        "command": "flash",
        "line1": "Pick Item",
        "line2": "Aisle 3",
        "sound": "beep",
        "seconds": 10,
        "color": "red",
        "arrow": "topright"
      },
      {
        "deviceid": "E8F297:F3F2F3",
        "command": "display",
        "color": "blue",
        "line1": "Station 5",
        "line2": "Processing",
        "arrow": "left"
      }
    ]
  ]
  ```

### Integration Tips
- Keep DeviceID stable even if slot labels change; only update when the physical hardware moves.
- Validate DeviceID format in the domain (uppercase, no spaces) to avoid API payload mismatches.
- Log changes in a change document object if you need auditability for automation devices.
- Mirror the DeviceID in automation middleware only after SAP confirms the update (check the PATCH response) to avoid drift.
- Document where duplicates are allowed (storage types/zones) so downstream systems can handle one-to-many mappings without conflict.
