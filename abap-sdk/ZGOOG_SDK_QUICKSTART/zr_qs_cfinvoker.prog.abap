**********************************************************************
*  Copyright 2024 Google LLC                                         *
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
REPORT zr_qs_cfinvoker.

DATA(lv_cf_name)  = CONV string( 'cf-gen2-hello-with-args' ).
DATA(lv_msg)      = CONV string( '{"firstname": "John", "lastname" : "Doe"}' ).


TRY.
    " Create a Client API stub for Cloud Functions
    DATA(lo_cloudfunc_client) = NEW /goog/cl_cloudfunc_v2( iv_key_name   = 'DEMO_CF' ).
    " Create a Client API stub for Cloud Function Invoker.
    " Internally this uses the Cloud Function instance to fetch the cloud function HTTP endpoint
    DATA(lo_cfinvoker_client) = NEW /goog/cl_cloudfunc_invoker( iv_key_name   = 'DEMO_CF_INVOKER' ).

    " Send additional query parameters as inputs to the cloud function.
    lo_cfinvoker_client->add_common_qparam( iv_name  = 'name'
                                            iv_value = 'Johnny' ).

    lo_cfinvoker_client->invoke(
      EXPORTING
        iv_cf_name      = lv_cf_name            "Cloud Function Name
        iv_cf_location  = 'us-central1'         "Location where the Cloud Function is hosted
        io_cf_instance  = lo_cloudfunc_client   "Instance of cloud Function Client API Stub
        iv_body         = lv_msg                "Input payload to the Cloud Function
        iv_content_type = 'application/json'
        iv_method       = 'POST'
      IMPORTING
        es_output       = DATA(lv_output)
        ev_ret_code     = DATA(lv_ret_code)
        ev_err_text     = DATA(lv_err_text)
        es_err_resp     = DATA(ls_err_resp)
    ).

    IF lo_cfinvoker_client->is_success( iv_code = lv_ret_code ).
      WRITE: / 'HTTP Return Code:', lv_ret_code.
      WRITE: / 'Response:', lv_output. "Output of cloud function
    ELSE.
      WRITE: / 'HTTP Return Code:', lv_ret_code.
      WRITE: / 'Error:', lv_err_text.
    ENDIF.

  CATCH /goog/cx_sdk INTO DATA(lo_exp).
    WRITE: / lo_exp->get_text( ).
ENDTRY.