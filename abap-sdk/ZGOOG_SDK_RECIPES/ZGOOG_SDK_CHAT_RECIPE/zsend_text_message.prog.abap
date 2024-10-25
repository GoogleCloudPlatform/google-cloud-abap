" --------------------------------------------------------------------
"  Copyright 2024 Google LLC                                         -
"                                                                    -
"  Licensed under the Apache License, Version 2.0 (the "License");   -
"  you may not use this file except in compliance with the License.  -
"  You may obtain a copy of the License at                           -
"      https://www.apache.org/licenses/LICENSE-2.0                   -
"  Unless required by applicable law or agreed to in writing,        -
"  software distributed under the License is distributed on an       -
"  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      -
"  either express or implied.                                        -
"  See the License for the specific language governing permissions   -
"  and limitations under the License.                                -
" --------------------------------------------------------------------
REPORT zsend_text_message.

DATA lv_client_key TYPE /goog/keyname.
DATA ls_input      TYPE /goog/cl_chat_v1=>ty_072.
DATA lv_space_id   TYPE string.

" TODO: Developer Set Values Here
" lv_client_key = "Set the configured Client Key here
" ls_input-text = "Provide a text message to be sent
" lv_space_id   = "Set the Space ID here

TRY.
    DATA(lo_chat) = NEW /goog/cl_chat_v1( iv_key_name = lv_client_key ).
  CATCH /goog/cx_sdk INTO DATA(lo_excp).
    " Handle exception here
ENDTRY.

TRY.
    lo_chat->create_messages( EXPORTING iv_p_spaces_id = lv_space_id
                                        is_input       = ls_input
                              IMPORTING ev_ret_code    = DATA(return_code)
                                        ev_err_text    = DATA(error_text)
                                        es_err_resp    = DATA(err_resp) ).
  CATCH /goog/cx_sdk INTO lo_excp.
    " Handle exception here
ENDTRY.

IF /goog/cl_chat_v1=>is_success( iv_code = return_code ) = abap_true.
  " Handle success here
ELSE.
  " Handle error here
ENDIF.
