CLASS zcl_salv_google_sheets_publush DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_salv_jpb_badi_data_publish .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA bin_data TYPE REF TO xstring .
    DATA: mo_client TYPE REF TO /goog/cl_drive_v3.
ENDCLASS.



CLASS ZCL_SALV_GOOGLE_SHEETS_PUBLUSH IMPLEMENTATION.


  METHOD if_salv_jpb_badi_data_publish~get_authentication_type.
    RETURN.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_binary_data_to_publish.
    bin_data = r_binary_data_to_publish.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_destination_type.
    destination_type = if_salv_jpb_data_publisher=>cs_destination_type-cloud.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_executable_location.
    RETURN.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_file_download_info.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_item_descriptor.
    Id                    = 1001.           "Optional
    text                  = |Google Sheets| ##NO_TEXT.
    frontend              = if_salv_jpb_data_publisher=>cs_frontend-google_workplace.
    is_default_for_format = abap_true.
    xml_type              = if_salv_bs_xml=>c_type_xlsx.
    output_format         = if_salv_jpb_data_publisher=>cs_output_format-xlsx.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_oa2c_auth_ingredients.
    RETURN.
  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~get_target_url_to_launch.

    DATA: ls_input TYPE /goog/cl_drive_v3=>ty_010.

    ls_input-name = 'ALV_SHEETS_EXPORT' && '_' && sy-tcode && '_' && sy-datum && sy-uzeit.
    ls_input-mime_type = 'application/vnd.google-apps.spreadsheet'.
    GET PARAMETER ID 'ZGOOG_DRIVE_ID' FIELD DATA(lv_folder_id). "TODO - User to maintain drive ID in Tcode SU3

    IF lv_folder_id IS INITIAL.
      MESSAGE 'Drive ID not configured in SPA/GPA parameter ZGOOG_DRIVE_ID (Tcode SU3)' TYPE 'I'.
      RAISE EXCEPTION TYPE cx_salv_connection_error
        EXPORTING
          textid = VALUE #( msgid = '/GOOG/MSG'
                            msgno = 000
                            attr1 = 'Drive ID not configured' ).
    ENDIF.

    ls_input-parents = VALUE #( ( CONV #( lv_folder_id ) ) ).

    TRY.

        mo_client->add_common_qparam( iv_name = 'uploadType' iv_value = 'multipart' ).
        mo_client->add_common_qparam( iv_name = 'fields' iv_value = 'id,webViewLink' ).

        mo_client->create_files(
          EXPORTING
            is_input               = ls_input
            is_data                = bin_data->*
            iv_content_type        = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
            iv_q_supportsalldrives = 'true'
          IMPORTING
            es_output              = DATA(ls_output)
            ev_ret_code            = DATA(lv_code)
            ev_err_text            = DATA(lv_text)
            es_err_resp            = DATA(lv_err_resp) ).

        target_url_to_launch = ls_output-web_view_link.

      CATCH /goog/cx_sdk INTO DATA(lo_error).
        RAISE EXCEPTION TYPE cx_salv_connection_error
          EXPORTING
            previous = lo_error.
    ENDTRY.


  ENDMETHOD.


  METHOD if_salv_jpb_badi_data_publish~is_connection_established.

    IF mo_client IS BOUND.
      is_connection_established = 'X'.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT mo_client
          EXPORTING
            iv_key_name = 'SHEETS_TEST'.  "TODO - Replace 'SHEETS_TEST' with configured Client Key

        is_connection_established = 'X'.

      CATCH /goog/cx_sdk INTO DATA(lo_error).
        RAISE EXCEPTION TYPE cx_salv_connection_error
          EXPORTING
            previous = lo_error.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
