"----------------------------------------------------------------------
"* Batch 'pick' command with line1–line5, shared color & seconds
"----------------------------------------------------------------------
FORM batch_send_pick_commands
  USING    iv_base_url  TYPE string      " e.g. 'https://www.voodoodevices.com'
           iv_api_key   TYPE string      " API-KEY value
           iv_color     TYPE string      " e.g. 'g' or 'green'
           iv_seconds   TYPE i           " max duration
  CHANGING ev_http_code TYPE i
           ev_response  TYPE string
           it_rows      TYPE STANDARD TABLE.  " see TY_ROW below

  " Per-device input (extend as needed)
  TYPES: BEGIN OF ty_row,
           deviceid TYPE string,
           line1    TYPE string,
           line2    TYPE string,
           line3    TYPE string,
           line4    TYPE string,
           line5    TYPE string,
         END OF ty_row.

  " API payload structure
  TYPES: BEGIN OF ty_device,
           deviceid TYPE string,
           command  TYPE string,
           color    TYPE string,
           seconds  TYPE i,
           line1    TYPE string,
           line2    TYPE string,
           line3    TYPE string,
           line4    TYPE string,
           line5    TYPE string,
         END OF ty_device.

  DATA: lv_url   TYPE string,
        lo_http  TYPE REF TO if_http_client,
        lv_json  TYPE string,
        lt_rows  TYPE STANDARD TABLE OF ty_row,
        lt_payld TYPE STANDARD TABLE OF ty_device.

  " Expect caller to pass IT_ROWS via CHANGING; move/cast if needed
  lt_rows = CORRESPONDING #( it_rows ).

  " Optional: basic parameter checks
  IF iv_seconds LE 0 OR iv_color IS INITIAL OR lt_rows IS INITIAL.
    ev_http_code = -1.
    ev_response  = 'Invalid input: seconds/color/rows'.
    RETURN.
  ENDIF.

  " Map input rows -> API payload
  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<r>).
    APPEND VALUE ty_device(
      deviceid = <r>-deviceid
      command  = 'pick'
      color    = iv_color
      seconds  = iv_seconds  " maximum duration of the light
      line1    = <r>-line1
      line2    = <r>-line2
      line3    = <r>-line3
      line4    = <r>-line4
      line5    = <r>-line5
      "arrow = string,     " Not used in this case
      "barcode = string,   " Not used in this case
      "qrcode = string,    " Not used in this case
    ) TO lt_payld.
  ENDLOOP.

  " Serialize array to JSON
  /ui2/cl_json=>serialize(
    EXPORTING
      data        = lt_payld
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    RECEIVING
      r_json      = lv_json ).

  " Endpoint: /api/devices/
  lv_url = |{ iv_base_url }/api/devices/|.

  " HTTP client
  cl_http_client=>create_by_url(
    EXPORTING url    = lv_url
    IMPORTING client = lo_http
    EXCEPTIONS OTHERS = 1 ).

  IF sy-subrc <> 0 OR lo_http IS INITIAL.
    ev_http_code = -1.
    ev_response  = 'HTTP client creation failed'.
    RETURN.
  ENDIF.

  " Headers & method
  lo_http->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
  lo_http->request->set_header_field( name = 'Accept'       value = 'application/json' ).
  lo_http->request->set_header_field( name = 'API-KEY'      value = iv_api_key ).
  lo_http->request->set_method( if_http_request=>co_request_method_post ).
  lo_http->request->set_cdata( lv_json ).

  " Send & receive
  lo_http->send( ).
  lo_http->receive( ).

  ev_http_code = lo_http->response->get_status( ).
  ev_response  = lo_http->response->get_cdata( ).

  lo_http->close( ).

ENDFORM.
