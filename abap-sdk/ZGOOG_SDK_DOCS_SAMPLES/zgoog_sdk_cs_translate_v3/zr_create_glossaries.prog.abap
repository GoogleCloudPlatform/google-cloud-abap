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

REPORT zr_create_glossaries.

DATA: lv_p_projects_id  TYPE string,
      lv_p_locations_id TYPE string,
      ls_input          TYPE /goog/cl_translation_v3=>ty_022.

TRY.

*    Open HTTP Connection
    DATA(lo_translate) = NEW /goog/cl_translation_v3( iv_key_name = 'TRANSLATE_DEMO' ). "Pass the configured client key

*    Populate required parameters
    lv_p_projects_id  = lo_translate->gv_project_id.    " Derive project id from the object
    lv_p_locations_id = 'us-central1'.                  "Set a location id, 'us-central1' is used as example

    ls_input-display_name = 'Finance Term Glossary EN to ES'.  "Pass a display name

    ls_input-language_pair-source_language_code = 'en-US'.   "Source language in BCP-47 format
    ls_input-language_pair-target_language_code = 'es-ES'.   "Target language in BCP-47 format

*    Complete name of glossary has following format:
*    projects/<PROJECT_ID>/locations/<LOCATION_ID>/glossaries/<GLOSSARY_ID>
    CONCATENATE 'projects/'
                 lo_translate->gv_project_id
                 '/locations/us-central1/glossaries/'
                 'FI_GLOSSARY_EN_ES'
                 INTO ls_input-name.

*    Pass the complete path of glossary file which is stored in GCS bucket
*    Below example shows a file named: fi_glossary_data.tsv is stored in a GCS bucket named: glossary_repo
    ls_input-input_config-gcs_source-input_uri = 'gs://glossary_repo/fi_glossary_data.tsv'.

*     Call API method
    CALL METHOD lo_translate->create_glossaries
      EXPORTING
        iv_p_projects_id  = lv_p_projects_id
        iv_p_locations_id = lv_p_locations_id
        is_input          = ls_input
      IMPORTING
        es_output         = DATA(ls_output)
        ev_ret_code       = DATA(lv_ret_code)
        ev_err_text       = DATA(lv_err_text)
        es_err_resp       = DATA(ls_err_resp).
    IF /goog/cl_http_client=>is_success( lv_ret_code ).
*      This returns a long running operation id as glossary creation is adhoc
*      You can use the LRO ID to poll to check the status of the operation (SUCCESS Or FAILURE)
      WRITE: 'LRO ID:', ls_output-name.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

*    Close HTTP Connection
    lo_translate->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
