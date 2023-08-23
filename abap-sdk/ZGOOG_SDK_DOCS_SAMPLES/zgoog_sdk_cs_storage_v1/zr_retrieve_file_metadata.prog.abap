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

REPORT zr_retrieve_file_metadata.

* Data Declarations
DATA:
  lv_p_bucket TYPE string,
  lv_p_object TYPE string.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*  Populate the data that needs to be passed to the api
    "Name of the bucket
    lv_p_bucket       = 'sample-bucket'.
    "Name of the object
    lv_p_object        = 'sample-object'.

*     Call API method
    CALL METHOD lo_client->get_objects
      EXPORTING
        iv_p_bucket = lv_p_bucket
        iv_p_object = lv_p_object
      IMPORTING
        es_output   = DATA(ls_output)
        ev_ret_code = DATA(lv_ret_code)
        ev_err_text = DATA(lv_err_text).

    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Object metadata retrieved successfully!' TYPE 'S'.
      WRITE: 'Bucket Name:', ls_output-bucket.
      WRITE:/ 'Object Name:', ls_output-name.
      WRITE:/ 'Content Type:', ls_output-content_type.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.
*   Close HTTP connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
