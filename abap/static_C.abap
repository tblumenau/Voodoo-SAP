"Hook: EWM BAdI /SCWM/EX_CORE_CO_POST – Posting of Confirmed Warehouse Tasks. 
"It’s designed for logic right after WT confirmation (the point at which /SCWM/QUAN reflects the new reality).

" Class implementing IF_EX_/SCWM/EX_CORE_CO_POST
METHOD if_ex_/scwm/ex_core_co_post~at_end.
  DATA: lv_http  TYPE i,
        lv_resp  TYPE string,
        lv_qty   TYPE /scwm/de_qty,
        ls_map   TYPE zvoodoo_loc_map,
        lv_maktx TYPE makt-maktx.

  " Loop confirmed tasks from importing parameter (example: et_wtc is your WT set)
  LOOP AT et_wtc ASSIGNING FIELD-SYMBOL(<wt>).
    " Read current quant in the (resulting) bin for this product
    SELECT SINGLE qty INTO lv_qty
      FROM /scwm/quan
      WHERE lgnum   = <wt>-lgnum
        AND lgpla   = <wt>-dest_lgpla   " destination bin (or source for pick)
        AND matid   = <wt>-matid.       " or join to get MATNR

    " Resolve MATNR if you only have MATID
    DATA(lv_matnr) = zcl_matid_helper=>to_matnr( <wt>-matid ).

    " Map bin → device
    SELECT SINGLE * INTO ls_map
      FROM zvoodoo_loc_map
      WHERE werks = <wt>-werks
        AND lgort = <wt>-lgort
        AND lgpla = <wt>-dest_lgpla
        AND matnr = lv_matnr.
    IF sy-subrc <> 0 OR ls_map-deviceid IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE maktx INTO lv_maktx
      FROM makt
      WHERE matnr = lv_matnr AND spras = sy-langu.

    PERFORM update_device_statics
      USING    'https://www.voodoodevices.com'
               ls_map-deviceid
               <your_api_key>
               lv_matnr
               lv_maktx
               lv_qty
      CHANGING lv_http lv_resp.
  ENDLOOP.
ENDMETHOD.

"Why here? EWM is the modern bin-level system. After WT confirmation, 
"/SCWM/QUAN (and, for “available” figures, /SCWM/AQUA) is authoritative.

"CDS/VDM reads: If you prefer CDS views for stock reads (e.g., I_MaterialStock for IM), 
"replace the MARD select with a released interface CDS read. (Interface “I_*” views are the stable contract in S/4.)