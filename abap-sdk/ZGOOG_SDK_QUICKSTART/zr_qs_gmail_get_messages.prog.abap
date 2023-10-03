**********************************************************************
*  Copyright 2023 Google LLC                                         *
*                                                                    *
*  Licensed under the Apache License, Version 2.0 (the "License");   *
*  you may not use this file except in compliance with the License.  *
*  You may obtain a copy of the License at                           *
*      https://www.apache.org/licenses/LICENSE-2.0                   *
*  Unless required by applicable law or agreed to in writing,        *
*  software distributed under the License is distributed on an       *
*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      *
*  either express or implied.                                        *
*  See the License for the specific language governing permissions   *
*  and limitations under the License.                                *
**********************************************************************
REPORT zr_qs_gmail_get_messages.

* Data declarations
DATA:
  lt_message_parts     TYPE /goog/cl_gmail_v1=>ty_t_038,
  lt_message_sub_parts TYPE /goog/cl_gmail_v1=>ty_t_038,
  lt_bin_data          TYPE STANDARD TABLE OF char1024,
  lv_p_id              TYPE string,
  lv_p_user_id         TYPE string,
  lv_xfile             TYPE xstring,
  lv_email_text        TYPE string,
  lv_file_length       TYPE i.

FIELD-SYMBOLS:
               <lt_message_parts> TYPE data.

TRY.
* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_gmail_v1( iv_key_name = 'TEST_GMAIL' ).

* Populate relevant parameters
    lv_p_id = '<Message ID>'. "Pass the email Message ID here
    lv_p_user_id = 'me'.      "Short hand to pass your Gmail ID

* Call API method: gmail.users.messages.get
    CALL METHOD lo_client->get_messages
      EXPORTING
        iv_p_id      = lv_p_id
        iv_p_user_id = lv_p_user_id
      IMPORTING
        es_output    = DATA(ls_output)
        ev_ret_code  = DATA(lv_ret_code)
        ev_err_text  = DATA(lv_err_text)
        es_err_resp  = DATA(ls_err_resp).
    IF lo_client->is_success( lv_ret_code ).
* Post processing to extract the response data reference to ABAP Type shipped with the SDK
* Serialize and deserialize response data in ABAP types embedded in the SDK
      ASSIGN ls_output-payload-parts->* TO <lt_message_parts>.
      DATA(lv_json) = /goog/cl_json_util=>serialize_json( is_data = <lt_message_parts> ).
      /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_json
                                                      iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                            IMPORTING es_data        = lt_message_parts ).
* Iterate internal table to browse through the message parts
      LOOP AT lt_message_parts ASSIGNING FIELD-SYMBOL(<ls_message_part>).
* If file name is not present, it means that the message part is email body
        IF <ls_message_part>-filename IS INITIAL.
* Serialize and deserialize response data in ABAP types embedded in the SDK
          ASSIGN <ls_message_part>-parts TO <lt_message_parts>.
          lv_json = /goog/cl_json_util=>serialize_json( is_data = <lt_message_parts> ).
          /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_json
                                                          iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                                IMPORTING es_data        = lt_message_sub_parts ).
          READ TABLE lt_message_sub_parts ASSIGNING FIELD-SYMBOL(<ls_message_sub_part>)
            WITH KEY mime_type = 'text/plain'.
          IF sy-subrc EQ 0.
* Call function module to decode the Base64 encoded email text to xstring format
            CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
              EXPORTING
                input    = <ls_message_sub_part>-body-data
                unescape = 'X'
              IMPORTING
                output   = lv_xfile
              EXCEPTIONS
                failed   = 1
                OTHERS   = 2.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.
* Call function module to covert email text from xstring to binary format
            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = lv_xfile
              IMPORTING
                output_length = lv_file_length
              TABLES
                binary_tab    = lt_bin_data.
* Call function module to covert email text from binary to string format
            CALL FUNCTION 'SCMS_BINARY_TO_STRING'
              EXPORTING
                input_length = lv_file_length
              IMPORTING
                text_buffer  = lv_email_text
              TABLES
                binary_tab   = lt_bin_data.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

* Display email text as output
            cl_demo_output=>display( lv_email_text ).

          ENDIF.
        ELSE.
* Display read Attachment ID as output
          WRITE:
                'Attachment ID: ',
                <ls_message_part>-body-attachment_id.

        ENDIF.

      ENDLOOP.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.

    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
