"Hook: WM user exit/BAdI on Transfer Order (TO) confirmation; that’s the moment bin quants (LQUA) change. 
"SAP ships WM exits specifically for TO creation/confirmation (see SPRO “Develop User Exits for WM”).


" Example: FORM called from your chosen WM TO-confirm exit
FORM z_wm_after_to_confirm
  USING    is_ltak TYPE ltak   " TO header
           it_ltap TYPE TABLE OF ltap. " TO items (confirmed)
  DATA: lv_http  TYPE i,
        lv_resp  TYPE string,
        lv_qty   TYPE i,
        ls_map   TYPE zvoodoo_loc_map,
        lv_maktx TYPE makt-maktx.

  LOOP AT it_ltap ASSIGNING FIELD-SYMBOL(<i>).
    " For each confirmed TO item, read the *resulting* bin stock
    " (sum of quants after the confirmation in the destination bin)
    SELECT SINGLE SUM( gesme ) INTO lv_qty
      FROM lqua
      WHERE lgnum = <i>-lgnum
        AND lgpla = <i>-nlpla        " destination bin after putaway/picking
        AND matnr = <i>-matnr.

    " Map bin → device
    SELECT SINGLE * INTO ls_map
      FROM zvoodoo_loc_map
      WHERE werks = <i>-werks
        AND lgort = <i>-lgort
        AND lgpla = <i>-nlpla
        AND matnr = <i>-matnr.
    IF sy-subrc <> 0 OR ls_map-deviceid IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE maktx INTO lv_maktx FROM makt
      WHERE matnr = <i>-matnr AND spras = sy-langu.

    PERFORM update_device_statics
      USING    'https://www.voodoodevices.com'
               ls_map-deviceid
               <your_api_key>
               <i>-matnr
               lv_maktx
               lv_qty
      CHANGING lv_http lv_resp.
  ENDLOOP.
ENDFORM.

"Why here? In WM, the authoritative per-bin truth is the quant (LQUA). 
"You want the post-confirmation quantity, not just the IM material doc.

"CDS/VDM reads: If you prefer CDS views for stock reads (e.g., I_MaterialStock for IM), 
"replace the MARD select with a released interface CDS read. (Interface “I_*” views are the stable contract in S/4.)