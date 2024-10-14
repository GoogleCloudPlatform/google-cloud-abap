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

"Data declarations
DATA: lv_p_projects_id  TYPE string,
      lv_p_locations_id TYPE string,
      ls_input          TYPE /goog/cl_translation_v3=>ty_022.

TRY.

    "Open HTTP Connection
    "Pass the configured client key - TRANSLATE_DEMO is used an example, replace it with actual value.
    DATA(lo_translate) = NEW /goog/cl_translation_v3( iv_key_name = 'TRANSLATE_DEMO' ).

    "Populate required parameters
    "Derive project id from the object
    lv_p_projects_id  = lo_translate->gv_project_id.

    "Set a location id, 'us-central1' is used as example, replace it with actual value.
    lv_p_locations_id = 'us-central1'.

    "Pass a display name, the value passed below is an example, replace it with actual value
    ls_input-display_name = 'Finance Term Glossary EN to ES'.

    "Source language in BCP-47 format. The value passed below is an example, replace it with actual value
    ls_input-language_pair-source_language_code = 'en-US'.

    "Target language in BCP-47 format. The value passed below is an example, replace it with actual value
    ls_input-language_pair-target_language_code = 'es-ES'.

    "Complete name of glossary has following format:
    "projects/<PROJECT_ID>/locations/<LOCATION_ID>/glossaries/<GLOSSARY_ID>
    "The value used for GLOSSARY ID is an exmaple, replace it with actual value
    CONCATENATE 'projects/'
                 lo_translate->gv_project_id
                 '/locations/us-central1/glossaries/'
                 'FI_GLOSSARY_EN_ES'
                 INTO ls_input-name.

    "TODO: Pass the complete path of glossary file which is stored in GCS bucket
    "ls_input-input_config-gcs_source-input_uri =

    "Call API method
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
      "This returns a long running operation id for the job that is scheduled for glossary creation
      "You can use the LRO ID to poll and check the status of the operation
      WRITE: 'LRO ID:', ls_output-name.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

    "Close HTTP Connection
    lo_translate->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
