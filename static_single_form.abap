"----------------------------------------------------------------------
"* Update statics for one device
"* - statica := SKU
"* - staticb := short description
"* - quantity := qty
"----------------------------------------------------------------------
FORM update_device_statics
  USING    iv_base_url    TYPE string     " e.g. 'https://www.voodoodevices.com'
           iv_deviceid    TYPE string     " e.g. 'D4E825:8B665D'
           iv_api_key     TYPE string     " API-KEY value
           iv_sku         TYPE string
           iv_short_desc  TYPE string
           iv_qty         TYPE i
  CHANGING ev_http_code   TYPE i
           ev_response    TYPE string.

  DATA: lv_url        TYPE string,
        lo_http       TYPE REF TO if_http_client,
        lv_json       TYPE string,
        lv_resp       TYPE string,
        lv_len        TYPE i.

  " Build endpoint: /api/device/{id}/
  lv_url = |{ iv_base_url }/api/device/{ iv_deviceid }/|.

  " Build JSON body
  TYPES: BEGIN OF ty_device,
           deviceid TYPE string,
           statica  TYPE string,
           staticb  TYPE string,
           "staticc  TYPE string,  " Not used in this case
           "staticd  TYPE string,  " Not used in this case
           quantity TYPE i,        " API accepts 'quantity' as a display line/field
           "arrow TYPE string,     " Not used in this case
           "color TYPE string,     " Not used in this case
           "barcode TYPE string,   " Not used in this case
           "qrcode TYPE string,    " Not used in this case

         END OF ty_device.

  DATA(ls_body) = VALUE ty_device(
      deviceid = iv_deviceid
      statica  = iv_sku
      staticb  = iv_short_desc
      quantity = iv_qty ).

  " Serialize to JSON
  /ui2/cl_json=>serialize(
    EXPORTING
      data        = ls_body
      pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    RECEIVING
      r_json      = lv_json ).

  " Create HTTP client
  cl_http_client=>create_by_url(
    EXPORTING url                = lv_url
    IMPORTING client             = lo_http
    EXCEPTIONS argument_not_found = 1 plugin_not_active = 2 internal_error = 3 ).

  IF sy-subrc <> 0 OR lo_http IS INITIAL.
    ev_http_code = -1.
    ev_response  = 'HTTP client creation failed'.
    RETURN.
  ENDIF.

  " Headers
  lo_http->request->set_header_field( name = 'Content-Type'  value = 'application/json' ).
  lo_http->request->set_header_field( name = 'Accept'        value = 'application/json' ).
  lo_http->request->set_header_field( name = 'API-KEY'       value = iv_api_key ). " per spec

  " Basic auth and session-based auth can be used instead of API-KEY.
  " OAUTH2 is also supported.  Refer to https://voodoorobotics.com/authentication/ for details
  " and PostMan examples.

  " POST body
  lv_len = strlen( lv_json ).
  lo_http->request->set_method( if_http_request=>co_request_method_post ).
  lo_http->request->set_cdata( lv_json ).

  " Send
  lo_http->send( ).
  lo_http->receive( ).

  ev_http_code = lo_http->response->get_status( ).
  lv_resp      = lo_http->response->get_cdata( ).
  ev_response  = lv_resp.

  lo_http->close( ).

ENDFORM.
