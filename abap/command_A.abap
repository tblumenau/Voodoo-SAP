" ZBIN_DEVICE_MAP
"  - WERKS  : Plant
"  - LGORT  : Storage location (optional)
"  - LGPLA  : Bin / storage bin / EWM bin
"  - DEVICEID : Voodoo device id (string)

"Uses S/4HANA CDS I_ProductionOrderComponent to fetch components still needed, 
"resolves to bin → device, then lights them.

REPORT z_light_pick_from_order.

TYPES: BEGIN OF ty_row,              "must match your FORM's TY_ROW field names
         deviceid TYPE string,
         line1    TYPE string,
         line2    TYPE string,
         line3    TYPE string,
         line4    TYPE string,
         line5    TYPE string,
       END OF ty_row.
TYPES ty_t_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

DATA: lt_rows      TYPE ty_t_rows,
      ls_row       TYPE ty_row,
      lv_base_url  TYPE string VALUE 'https://api.your-voodoo-endpoint.com',
      lv_api_key   TYPE string VALUE '***REDACTED***',
      lv_color     TYPE string VALUE 'green',
      lv_seconds   TYPE i      VALUE 45,
      lv_http_code TYPE i,
      lv_response  TYPE string.

PARAMETERS: p_aufnr TYPE aufnr OBLIGATORY.  " Production order number

" 1) Get order components with open qty (example logic; adapt to your fields)
SELECT poc~ManufacturingOrder     AS aufnr,
       poc~Material               AS matnr,
       poc~Plant                  AS werks,
       poc~StorageLocation        AS lgort,
       poc~RequirementQuantity    AS req_qty,
       COALESCE( poc~WithdrawalQuantity, 0 ) AS wdr_qty,
       ( poc~RequirementQuantity - COALESCE( poc~WithdrawalQuantity, 0 ) ) AS open_qty
  FROM I_ProductionOrderComponent AS poc
  WHERE poc~ManufacturingOrder = @p_aufnr
    AND ( poc~RequirementQuantity - COALESCE( poc~WithdrawalQuantity, 0 ) ) > 0
  INTO TABLE @DATA(lt_comp).

IF lt_comp IS INITIAL.
  WRITE: / 'No open components for order', p_aufnr.
  EXIT.
ENDIF.

" 2) Resolve bin/location → device (example: using LGORT + a custom bin strategy)
"    If you have EWM bin-level staging, join that table here instead.
LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<c>).

  " Example: determine a staging bin for the component. Replace with your logic.
  " Here we fake a bin = MATNR's default bin table ZMAT_DEFAULT_BIN.
  SELECT SINGLE lgpla
    FROM zmat_default_bin
    WHERE matnr = @<c>-matnr
      AND werks = @<c>-werks
      AND ( lgort = @<c>-lgort OR @<c>-lgort IS INITIAL )
    INTO @DATA(lv_lgpla).

  IF sy-subrc <> 0 OR lv_lgpla IS INITIAL.
    CONTINUE. " No known bin mapping for this material
  ENDIF.

  " Map bin → device
  SELECT SINGLE deviceid
    FROM zbin_device_map
    WHERE werks = @<c>-werks
      AND ( lgort = @<c>-lgort OR @<c>-lgort IS INITIAL )
      AND lgpla = @lv_lgpla
    INTO @DATA(lv_deviceid).

  IF sy-subrc <> 0 OR lv_deviceid IS INITIAL.
    CONTINUE. " No device for this bin
  ENDIF.

  " 3) Compose the 5 lines for the Voodoo device
  CLEAR ls_row.
  ls_row-deviceid = lv_deviceid.
  ls_row-line1    = |Order { <c>-aufnr }|.
  ls_row-line2    = |MAT { <c>-matnr }|.
  ls_row-line3    = |Bin { lv_lgpla }|.
  ls_row-line4    = |Pick { <c>-open_qty }|.
  ls_row-line5    = |UoM EA|. " adapt if you have the UoM; e.g., poc~BaseUnit

  APPEND ls_row TO lt_rows.
ENDLOOP.

IF lt_rows IS INITIAL.
  WRITE: / 'No devices resolved for open components.'.
  EXIT.
ENDIF.

" 4) Fire the lights
PERFORM batch_send_pick_commands
  USING    lv_base_url lv_api_key lv_color lv_seconds
  CHANGING lv_http_code lv_response lt_rows.

WRITE: / 'HTTP:', lv_http_code, ' Response:', lv_response.
