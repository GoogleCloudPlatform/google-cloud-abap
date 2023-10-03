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
REPORT zr_qs_drive_list_files.

* Types declaration for displaying output
TYPES:
  BEGIN OF lty_output_list,
    name      TYPE string,
    kind      TYPE string,
    mime_type TYPE string,
    id        TYPE string,

  END OF lty_output_list.

* Data declarations
DATA:
  lv_q_corpora                   TYPE string,
  lv_q_driveid                   TYPE string,
  lv_q_includeitemsfromalldrives TYPE string,
  lv_q_supportsalldrives         TYPE string,
  lt_output_list                 TYPE STANDARD TABLE OF lty_output_list,
  ls_output_list                 TYPE lty_output_list.

TRY.
* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_drive_v3( iv_key_name = 'TEST_DRIVE' ).

* Populate relevant parameters
    lv_q_corpora = 'drive'.
    lv_q_driveid = '<Drive ID>'.  "Pass the Drive ID here
    lv_q_includeitemsfromalldrives = 'true'.
    lv_q_supportsalldrives = 'true'.

* Call API method: drive.files.list
    CALL METHOD lo_client->list_files
      EXPORTING
        iv_q_corpora                   = lv_q_corpora
        iv_q_driveid                   = lv_q_driveid
        iv_q_includeitemsfromalldrives = lv_q_includeitemsfromalldrives
        iv_q_supportsalldrives         = lv_q_supportsalldrives
      IMPORTING
        es_output                      = DATA(ls_output)
        ev_ret_code                    = DATA(lv_ret_code)
        ev_err_text                    = DATA(lv_err_text)
        es_err_resp                    = DATA(ls_err_resp).

    IF lo_client->is_success( lv_ret_code ).
      LOOP AT ls_output-files ASSIGNING FIELD-SYMBOL(<ls_flie>).
        ls_output_list-name      = <ls_flie>-name.
        ls_output_list-kind      = <ls_flie>-kind.
        ls_output_list-mime_type = <ls_flie>-mime_type.
        ls_output_list-id        = <ls_flie>-id.

        APPEND ls_output_list TO lt_output_list.
        CLEAR ls_output_list.

      ENDLOOP.
      cl_demo_output=>display( lt_output_list ).
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
