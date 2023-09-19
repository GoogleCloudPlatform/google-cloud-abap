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

REPORT zr_qs_dlp_deidentify.

* data declarations
DATA:
  lv_p_projects_id   TYPE string,
  ls_input           TYPE /goog/cl_dlp_v2=>ty_055,
  ls_transformations TYPE /goog/cl_dlp_v2=>ty_100.

TRY.
* instantiate api client stub
    DATA(lo_dlp) = NEW /goog/cl_dlp_v2( iv_key_name = 'DLP_V2' ).

* pass the sample text for deidentification
    lv_p_projects_id = lo_dlp->gv_project_id.

    INSERT VALUE #( name = 'EMAIL_ADDRESS' ) INTO TABLE ls_input-inspect_config-info_types.
    ls_transformations-primitive_transformation-replace_config-new_value-string_value  = '[EMAIL_ID]'.
    INSERT ls_transformations INTO TABLE ls_input-deidentify_config-info_type_transformations-transformations.

    ls_input-item-value = 'The Email ID of Mr. Foo is foobar@example.com'.


* call the api method to deidentify
    CALL METHOD lo_dlp->deidentify_content
      EXPORTING
        iv_p_projects_id = lv_p_projects_id
        is_input         = ls_input
      IMPORTING
        es_output        = DATA(ls_output)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

    IF lo_dlp->is_success( lv_ret_code ).
      WRITE: / 'Deidentification Successful'.
      WRITE: / 'The replaced text is: ', ls_output-item-value.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* close the http connection
    lo_dlp->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
* write code here to handle exceptions
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
