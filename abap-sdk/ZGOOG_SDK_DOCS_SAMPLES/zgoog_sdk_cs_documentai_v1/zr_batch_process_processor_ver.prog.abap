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

REPORT zr_batch_process_processor_ver.

DATA:
  lv_p_projects_id           TYPE string,
  lv_p_locations_id          TYPE string,
  lv_p_processors_id         TYPE string,
  lv_p_processor_versions_id TYPE string,
  ls_input                   TYPE /goog/cl_documentai_v1=>ty_017.

TRY.
* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_documentai_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    lv_p_projects_id = lo_client->gv_project_id.
    lv_p_locations_id = 'LOCATION_ID'.  "Location ID of the Processor
    lv_p_processors_id = 'PROCESSOR_ID'.  "Processor ID of the Processor
    lv_p_processor_versions_id = 'PROCESSOR_VERSION_ID'.  "Processor Version of the Processor
    ls_input-input_documents-gcs_prefix-gcs_uri_prefix = 'SOURCE_GCS_BUCKET_URI_PREFIX'.  "Source GCS Bucket URI Prefix
    ls_input-document_output_config-gcs_output_config-gcs_uri = 'TARGET_GCS_BUCKET_URI'.  "Target GCS Bucket URI

* Call API method
    CALL METHOD lo_client->batch_process_processor_ver
      EXPORTING
        iv_p_projects_id           = lv_p_projects_id
        iv_p_locations_id          = lv_p_locations_id
        iv_p_processors_id         = lv_p_processors_id
        iv_p_processor_versions_id = lv_p_processor_versions_id
        is_input                   = ls_input
      IMPORTING
        es_output                  = DATA(ls_output)
        ev_ret_code                = DATA(lv_ret_code)
        ev_err_text                = DATA(lv_err_text)
        es_err_resp                = DATA(ls_err_resp).

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
