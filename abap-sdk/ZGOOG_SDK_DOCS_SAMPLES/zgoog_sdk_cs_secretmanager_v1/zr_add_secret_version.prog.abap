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

REPORT zr_add_secret_version.

DATA:
  lv_p_projects_id TYPE string,
  lv_p_secrets_id  TYPE string,
  ls_input         TYPE /goog/cl_secretmgr_v1=>ty_002.

TRY.

*     Open HTTP Connection
    DATA(lo_sm) = NEW /goog/cl_secretmgr_v1( iv_key_name = 'DEMO_SM' ).

*     Populate relevant parameters
    lv_p_projects_id = lo_sm->gv_project_id.
    lv_p_secrets_id = 'Secret_001'.
    ls_input = VALUE #( payload = VALUE #( data = 'Shhh! this is a secret' ) ).

*     Call API method
    CALL METHOD lo_sm->add_version_secrets
      EXPORTING
        iv_p_projects_id = lv_p_projects_id
        iv_p_secrets_id  = lv_p_secrets_id
        is_input         = ls_input
      IMPORTING
        es_output        = DATA(ls_output)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Secret version added successfully' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

*     Close HTTP Connection
    lo_sm->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
