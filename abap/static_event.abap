"Event-driven approach that avoids exits/BAdIs. We subscribe to MaterialDocument.Created from S/4HANA, 
"let S/4 push the event to an ICF HTTP endpoint you host in ABAP, and then do a 
"point read (IM or EWM) before calling your update_device_statics FORM.

"Two self-contained pieces:

"An ICF handler (ZCL_VOODOO_EVENT_ICF) that receives the event payload and dispatches to IM or EWM logic

"A tiny utility (ZCL_VOODOO_PUSH) that does the point reads and calls your update_device_statics

"Just register the ICF node in SICF, and bind the S/4 event to this URL in Event Enablement / Event Mesh.




"======================================================================
" ICF handler for S/4 Enterprise Event: MaterialDocument.Created
" URL example you configure in Event Mesh/CPI:
"   https://<your-abap-host>/sap/bc/zvoodoo_event_in
"======================================================================
CLASS zcl_voodoo_event_icf DEFINITION
  PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_extension.
  PRIVATE SECTION.
    METHODS handle_material_document_created
      IMPORTING is_evt TYPE string
                io_resp TYPE REF TO if_http_response.
ENDCLASS.

CLASS zcl_voodoo_event_icf IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA(lv_body) = request->get_cdata( ).
    DATA(lv_topic) TYPE string.
    DATA: ls_hdr TYPE string.

    " Optional: check topic/type header if you route multiple events here
    request->get_header_field( EXPORTING name = 'CE-Type' IMPORTING value = lv_topic ).
    " Fallback: some setups use 'x-sap-event-type' or similar
    IF lv_topic IS INITIAL.
      request->get_header_field( EXPORTING name = 'x-sap-event-type' IMPORTING value = lv_topic ).
    ENDIF.

    IF lv_topic CS 'MaterialDocument.Created'.
      me->handle_material_document_created( lv_body , response ).
    ELSE.
      response->set_status( code = 204 reason = 'No handler' ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_material_document_created.
    " --- 1) Parse CloudEvent/Business Event payload ---
    TYPES: BEGIN OF ty_matdoc_item,
             Material       TYPE string,
             Plant          TYPE string,
             StorageLocation TYPE string,
             Batch          TYPE string,
           END OF ty_matdoc_item.
    TYPES: BEGIN OF ty_evt,
             id          TYPE string,
             source      TYPE string,
             specversion TYPE string,
             type        TYPE string,
             time        TYPE string,
             data        TYPE string, " may be nested JSON
           END OF ty_evt.

    DATA(ls_evt) TYPE ty_evt.
    /ui2/cl_json=>deserialize( EXPORTING json = is_evt CHANGING data = ls_evt ).

    " data may be nested JSON (string); deserialize again into a structure you expect.
    " Typical S/4 event carries MaterialDocument and the positions.
    TYPES: BEGIN OF ty_data,
             MaterialDocument TYPE string,
             Items            TYPE STANDARD TABLE OF ty_matdoc_item WITH DEFAULT KEY,
           END OF ty_data.
    DATA(ls_data) TYPE ty_data.
    /ui2/cl_json=>deserialize( EXPORTING json = ls_evt-data CHANGING data = ls_data ).

    " --- 2) For each affected item, compute and push new quantity ---
    DATA: lv_http TYPE i,
          lv_resp TYPE string.

    LOOP AT ls_data-Items ASSIGNING FIELD-SYMBOL(<it>).
      " Decide IM vs EWM per location/plant (your rule of thumb/table)
      DATA(lv_mode) = zcl_voodoo_push=>get_mode_for_loc( <it>-Plant , <it>-StorageLocation ). " 'IM' or 'EWM'

      CASE lv_mode.
        WHEN 'IM'.
          zcl_voodoo_push=>push_im(
            iv_werks = <it>-Plant
            iv_lgort = <it>-StorageLocation
            iv_matnr = <it>-Material
            iv_batch = <it>-Batch             "optional
            CHANGING ev_http = lv_http ev_response = lv_resp ).

        WHEN 'EWM'.
          " If you have bin mapping per material, you may push multiple bins; here we push the bin
          " mapped via your Z table.
          zcl_voodoo_push=>push_ewm(
            iv_werks = <it>-Plant
            iv_lgort = <it>-StorageLocation
            iv_matnr = <it>-Material
            iv_batch = <it>-Batch             "optional
            CHANGING ev_http = lv_http ev_response = lv_resp ).

        WHEN OTHERS.
          " no-op
      ENDCASE.
    ENDLOOP.

    io_resp->set_status( code = 202 reason = 'Accepted' ).
  ENDMETHOD.

ENDCLASS.




"======================================================================
" ZCL_VOODOO_PUSH: reads current qty (IM or EWM) and calls update_device_statics
"======================================================================
CLASS zcl_voodoo_push DEFINITION
  PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_mode_for_loc
      IMPORTING iv_werks TYPE werks_d
                iv_lgort TYPE lgort_d
      RETURNING VALUE(rv_mode) TYPE string. " 'IM' or 'EWM'

    CLASS-METHODS push_im
      IMPORTING iv_werks TYPE werks_d
                iv_lgort TYPE lgort_d
                iv_matnr TYPE matnr
                iv_batch TYPE charg_d OPTIONAL
      CHANGING  ev_http  TYPE i
                ev_response TYPE string.

    CLASS-METHODS push_ewm
      IMPORTING iv_werks TYPE werks_d
                iv_lgort TYPE lgort_d
                iv_matnr TYPE matnr
                iv_batch TYPE charg_d OPTIONAL
      CHANGING  ev_http  TYPE i
                ev_response TYPE string.
  PRIVATE SECTION.
    CLASS-METHODS get_device_for
      IMPORTING iv_werks TYPE werks_d
                iv_lgort TYPE lgort_d
                iv_lgpla TYPE lgpla OPTIONAL "bin (IM leaves empty)
                iv_matnr TYPE matnr
      RETURNING VALUE(rv_deviceid) TYPE string.

    CLASS-METHODS get_text_for_material
      IMPORTING iv_matnr TYPE matnr
      RETURNING VALUE(rv_maktx) TYPE makt-maktx.
ENDCLASS.

CLASS zcl_voodoo_push IMPLEMENTATION.

  METHOD get_mode_for_loc.
    " Simple example: decide with a config table ZVOODOO_LOC_MODE (WERKS,LGORT,MODE)
    SELECT SINGLE mode INTO rv_mode
      FROM zvoodoo_loc_mode
      WHERE werks = iv_werks AND lgort = iv_lgort.
    IF sy-subrc <> 0.
      rv_mode = 'IM'.
    ENDIF.
  ENDMETHOD.

  METHOD push_im.
    DATA: lv_qty   TYPE i,
          lv_maktx TYPE makt-maktx,
          lv_dev   TYPE string,
          lv_http  TYPE i,
          lv_resp  TYPE string.

    " Map: plant+sloc+material → device (IM: no bin)
    lv_dev = get_device_for( iv_werks = iv_werks iv_lgort = iv_lgort iv_matnr = iv_matnr ).

    IF lv_dev IS INITIAL.
      ev_http = 204. ev_response = |No device mapping for { iv_werks }/{ iv_lgort }/{ iv_matnr }|.
      RETURN.
    ENDIF.

    " ---- Point read current stock (choose ONE path) ----
    " (A) CDS view (preferred in S/4): I_MaterialStock / I_MaterialStock_Aggr
    TRY.
        " Replace fields with those exposed by your release; example below:
        SELECT SINGLE QuantityInBaseUnit
          FROM I_MaterialStock
          WHERE Material = @iv_matnr
            AND Plant    = @iv_werks
            AND StorageLocation = @iv_lgort
            AND InventorySpecialStockType = ''  " regular stock
          INTO @lv_qty.
      CATCH cx_sy_open_sql_db.
        " (B) Compatibility table fallback: MARD-LABST
        SELECT SINGLE labst INTO lv_qty
          FROM mard
          WHERE matnr = @iv_matnr AND werks = @iv_werks AND lgort = @iv_lgort.
    ENDTRY.

    lv_maktx = get_text_for_material( iv_matnr ).

    PERFORM update_device_statics
      USING    'https://www.voodoodevices.com'
               lv_dev
               '<YOUR_API_KEY>'
               iv_matnr
               lv_maktx
               lv_qty
      CHANGING lv_http lv_resp.

    ev_http = lv_http.
    ev_response = lv_resp.
  ENDMETHOD.

  METHOD push_ewm.
    DATA: lv_qty   TYPE /scwm/de_qty,
          lv_maktx TYPE makt-maktx,
          lv_dev   TYPE string,
          lv_http  TYPE i,
          lv_resp  TYPE string,
          lv_lgpla TYPE lgpla.

    " Resolve the bin (LGPLA) we want to show; often from your mapping
    " (e.g., one device per (plant, sloc, bin, material))
    " If multiple bins per material, you can loop and push each.
    SELECT SINGLE lgpla INTO lv_lgpla
      FROM zvoodoo_loc_map
      WHERE werks = @iv_werks AND lgort = @iv_lgort AND matnr = @iv_matnr.

    IF sy-subrc <> 0 OR lv_lgpla IS INITIAL.
      ev_http = 204. ev_response = |No bin mapping for { iv_werks }/{ iv_lgort }/{ iv_matnr }|.
      RETURN.
    ENDIF.

    " Read EWM quant. If EWM is embedded, /SCWM/QUAN is local; otherwise RFC to EWM system.
    " Example: sum physical qty in that bin for the product
    " (If you have MATID, convert from MATNR with your helper; keeping it simple here.)
    SELECT SINGLE SUM( qty )
      FROM /scwm/quan
      WHERE lgpla = @lv_lgpla
        AND matnr = @iv_matnr
      INTO @lv_qty.

    lv_maktx = get_text_for_material( iv_matnr ).
    lv_dev   = get_device_for( iv_werks = iv_werks iv_lgort = iv_lgort iv_lgpla = lv_lgpla iv_matnr = iv_matnr ).

    IF lv_dev IS INITIAL.
      ev_http = 204. ev_response = |No device mapping for { iv_werks }/{ iv_lgort }/{ lv_lgpla }/{ iv_matnr }|.
      RETURN.
    ENDIF.

    PERFORM update_device_statics
      USING    'https://www.voodoodevices.com'
               lv_dev
               '<YOUR_API_KEY>'
               iv_matnr
               lv_maktx
               lv_qty
      CHANGING lv_http lv_resp.

    ev_http = lv_http.
    ev_response = lv_resp.
  ENDMETHOD.

  METHOD get_device_for.
    SELECT SINGLE deviceid INTO rv_deviceid
      FROM zvoodoo_loc_map
      WHERE werks = @iv_werks
        AND lgort = @iv_lgort
        AND ( @iv_lgpla IS INITIAL OR lgpla = @iv_lgpla )
        AND matnr = @iv_matnr.
  ENDMETHOD.

  METHOD get_text_for_material.
    SELECT SINGLE maktx INTO rv_maktx
      FROM makt
      WHERE matnr = @iv_matnr AND spras = @sy-langu.
    IF sy-subrc <> 0.
      rv_maktx = iv_matnr.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
