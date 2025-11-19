"Hook: MB_DOCUMENT_BADI → method MB_DOCUMENT_UPDATE (fires after material document posts). 
"This is still the workhorse for GR/GI in S/4; if you’re on pure S/4 Cloud, 
"use the equivalent Material Document BAdI in the Event/Extensibility catalog.


METHOD if_ex_mb_document_badi~mb_document_update.
  DATA: lv_http   TYPE i,
        lv_resp   TYPE string,
        lv_qty    TYPE i,
        ls_map    TYPE zvoodoo_loc_map,
        lv_maktx  TYPE makt-maktx.

  " Loop posted items (XMSEG has the final posted quantities)
  LOOP AT xmseg ASSIGNING FIELD-SYMBOL(<x>).
    " Find the device for this material/location (IM level: no bin)
    SELECT SINGLE * INTO ls_map
      FROM zvoodoo_loc_map
      WHERE werks = <x>-werks
        AND lgort = <x>-lgort
        AND lgpla = ''           " empty for IM only
        AND matnr = <x>-matnr.
    IF sy-subrc <> 0 OR ls_map-deviceid IS INITIAL.
      CONTINUE.
    ENDIF.

    " (Re)read current on-hand for plant+sloc+material (OK to use MARD compat view)
    SELECT SINGLE labst INTO lv_qty
      FROM mard
      WHERE matnr = <x>-matnr
        AND werks = <x>-werks
        AND lgort = <x>-lgort.

    " Short text (language-aware)
    SELECT SINGLE maktx INTO lv_maktx
      FROM makt
      WHERE matnr = <x>-matnr
        AND spras = sy-langu.

    " Push to the device
    PERFORM update_device_statics
      USING    'https://www.voodoodevices.com'
               ls_map-deviceid
               <your_api_key>
               <x>-matnr
               lv_maktx
               lv_qty
      CHANGING lv_http lv_resp.
  ENDLOOP.
ENDMETHOD.


"Why here? Every GR/GI/transfer creates a material document; this BAdI is the standard post-commit place to react. 
"In S/4, stock is derived (NSDM) from material documents, so using the doc as your trigger is robust.

"CDS/VDM reads: If you prefer CDS views for stock reads (e.g., I_MaterialStock for IM), 
"replace the MARD select with a released interface CDS read. (Interface “I_*” views are the stable contract in S/4.)