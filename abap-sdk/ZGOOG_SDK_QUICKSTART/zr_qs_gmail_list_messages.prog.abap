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
REPORT zr_qs_gmail_list_messages.

* Data declarations
DATA:
  lv_q_labelids TYPE string,
  lv_p_user_id  TYPE string.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_gmail_v1( iv_key_name = 'TEST_GMAIL' ).

* Populate relevant parameters
    lv_q_labelids = '<Label ID>'. "Pass the Label ID here
    lv_p_user_id = 'me'.          "Short hand to pass your Gmail ID

* Call API method: gmail.users.messages.list
    CALL METHOD lo_client->list_messages
      EXPORTING
        iv_q_labelids = lv_q_labelids
        iv_p_user_id  = lv_p_user_id
      IMPORTING
        es_output     = DATA(ls_output)
        ev_ret_code   = DATA(lv_ret_code)
        ev_err_text   = DATA(lv_err_text)
        es_err_resp   = DATA(ls_err_resp).

    IF lo_client->is_success( lv_ret_code ).
* Display returned message ids for messages with label "PODEMO"
      cl_demo_output=>display( ls_output-messages ).
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.

    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
