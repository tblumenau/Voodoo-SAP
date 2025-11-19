" ZBIN_DEVICE_MAP
"  - WERKS  : Plant
"  - LGORT  : Storage location (optional)
"  - LGPLA  : Bin / storage bin / EWM bin
"  - DEVICEID : Voodoo device id (string)

"Reads /SCWM/ORDIM_O (open WH tasks), maps source bin to device, then lights them.
"Filter the selection to your wave/batch/delivery/ODN as you prefer.

REPORT z_light_pick_from_ewm_wt.

TYPES: BEGIN OF ty_row,
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

" Example parameters: wave, delivery, or WH no.
PARAMETERS: p_whno  TYPE /scwm/lgnum OBLIGATORY,
            p_wave  TYPE /scwm/waveid,         " optional
            p_dlvy  TYPE vbeln_vl.             " optional outbound delivery

" 1) Select open tasks (simplify filters as needed)
SELECT
  t~lgnum      AS whno,
  t~tanum      AS task_id,
  t~matnr      AS matnr,
  t~vsolm      AS qty_to_pick,     " open qty
  t~meins      AS uom,
  t~vlsrc_lgpla AS src_bin,        " source bin
  t~vldst_lgpla AS dest_bin        " destination bin
  FROM /scwm/ordim_o AS t
  WHERE t~lgnum = @p_whno
    AND ( @p_wave IS INITIAL OR t~waveid = @p_wave )
    AND ( @p_dlvy IS INITIAL OR t~vbeln   = @p_dlvy )
  INTO TABLE @DATA(lt_wt).

IF lt_wt IS INITIAL.
  WRITE: / 'No open warehouse tasks match your filter.'.
  EXIT.
ENDIF.

" 2) Map source bin → device and build rows
LOOP AT lt_wt ASSIGNING FIELD-SYMBOL(<wt>).

  SELECT SINGLE deviceid
    FROM zbin_device_map
    WHERE werks IS INITIAL        " if WH spans plants, include WERKS if you have it
      AND lgpla = @<wt>-src_bin
    INTO @DATA(lv_deviceid).

  IF sy-subrc <> 0 OR lv_deviceid IS INITIAL.
    CONTINUE.
  ENDIF.

  CLEAR ls_row.
  ls_row-deviceid = lv_deviceid.
  ls_row-line1    = |WT { <wt>-task_id }|.
  ls_row-line2    = |MAT { <wt>-matnr }|.
  ls_row-line3    = |From { <wt>-src_bin }|.
  ls_row-line4    = |Qty { <wt>-qty_to_pick }|.
  ls_row-line5    = |UoM { <wt>-uom }|.

  APPEND ls_row TO lt_rows.
ENDLOOP.

IF lt_rows IS INITIAL.
  WRITE: / 'No devices found for selected tasks.'.
  EXIT.
ENDIF.

" 3) Fire the lights
PERFORM batch_send_pick_commands
  USING    lv_base_url lv_api_key lv_color lv_seconds
  CHANGING lv_http_code lv_response lt_rows.

WRITE: / 'HTTP:', lv_http_code, ' Response:', lv_response.
