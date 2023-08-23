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

REPORT zr_cancel_operations.

DATA:
  lv_p_projects_id   TYPE string,
  lv_p_locations_id  TYPE string,
  lv_p_operations_id TYPE string.

TRY.

* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_documentai_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    lv_p_projects_id = lo_client->gv_project_id.
    lv_p_locations_id = 'LOCATION_ID'.  "Location ID of the Processor
    lv_p_operations_id = 'OPERATION_ID'.  "Operation ID of a Long Running Operation

* Call API method
    CALL METHOD lo_client->cancel_operations
      EXPORTING
        iv_p_projects_id   = lv_p_projects_id
        iv_p_locations_id  = lv_p_locations_id
        iv_p_operations_id = lv_p_operations_id
      IMPORTING
        es_output          = DATA(ls_output)
        ev_ret_code        = DATA(lv_ret_code)
        ev_err_text        = DATA(lv_err_text)
        es_err_resp        = DATA(ls_err_resp).

    IF /goog/cl_http_client=>is_success( lv_ret_code ).
      MESSAGE 'Success' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
