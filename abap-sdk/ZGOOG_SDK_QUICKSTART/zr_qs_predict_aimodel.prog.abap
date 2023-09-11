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
REPORT zr_qs_predict_aimodel.

TYPES:
  BEGIN OF t_instances,
    content TYPE string,
  END OF t_instances .
TYPES:
  BEGIN OF t_parameters,
    max_output_tokens TYPE i,
    temperature       TYPE f,
    top_k             TYPE i,
    top_p             TYPE f,
  END OF t_parameters .
TYPES:
  tt_instances TYPE STANDARD TABLE OF t_instances WITH DEFAULT KEY .
TYPES t_categories TYPE string .
TYPES:
  BEGIN OF t_scores,
    scores TYPE string,
  END OF t_scores .
TYPES:
  tt_categories TYPE STANDARD TABLE OF t_categories WITH DEFAULT KEY .
TYPES:
  tt_scores TYPE STANDARD TABLE OF t_scores WITH DEFAULT KEY .
TYPES:
  BEGIN OF t_safety_attributes,
    blocked    TYPE abap_bool,
    categories TYPE tt_categories,
    scores     TYPE tt_scores,
  END OF t_safety_attributes .
TYPES:
  BEGIN OF t_predictions,
    content           TYPE string,
    safety_attributes TYPE t_safety_attributes,
  END OF t_predictions .
TYPES:
  tt_predictions TYPE STANDARD TABLE OF t_predictions WITH DEFAULT KEY .
TYPES:
  BEGIN OF t_output,
    deployed_model_id  TYPE string,
    metadata           TYPE REF TO data,
    model              TYPE string,
    model_display_name TYPE string,
    model_version_id   TYPE string,
    predictions        TYPE tt_predictions,
  END OF t_output .


DATA:
  lo_aiplatform      TYPE REF TO /goog/cl_aiplatform_v1,
  lt_instances       TYPE tt_instances,
  ls_params          TYPE t_parameters,
  ls_instance        type t_instances,
  lv_p_projects_id   TYPE string,
  lv_p_locations_id  TYPE string,
  lv_p_publishers_id TYPE string,
  lv_p_models_id     TYPE string,
  lv_err_text        TYPE string,
  lv_ret_code        TYPE i,
  ls_output_llm      TYPE t_output,
  lv_raw             TYPE string,
  ls_err_resp        TYPE /goog/err_resp,
  ls_input           TYPE /goog/cl_aiplatform_v1=>ty_001,
  lo_exception       TYPE REF TO /goog/cx_sdk.

ls_instance-content = 'What is ABAP?'.
APPEND ls_instance TO lt_instances.
ls_params-max_output_tokens  = 256.
ls_params-temperature = '0.3'.
ls_params-top_k = '40'.
ls_params-top_p  = '0.8'.

GET REFERENCE OF lt_instances INTO ls_input-instances.
GET REFERENCE OF ls_params INTO ls_input-parameters.

TRY.

* Instantiate the client stub
    CREATE OBJECT lo_aiplatform
      EXPORTING
        iv_key_name = 'DEMO_AIPLATFORM'.

* Populate relevant parameters
    lv_p_projects_id = lo_aiplatform->gv_project_id.
    lv_p_locations_id = 'us-central1'.
    lv_p_publishers_id = 'google'.
    lv_p_models_id = 'text-bison'.


* Call API method
    CALL METHOD lo_aiplatform->predict_models
      EXPORTING
        iv_p_projects_id   = lv_p_projects_id
        iv_p_locations_id  = lv_p_locations_id
        iv_p_publishers_id = lv_p_publishers_id
        iv_p_models_id     = lv_p_models_id
        is_input           = ls_input
      IMPORTING
        es_raw             = lv_raw
        ev_ret_code        = lv_ret_code
        ev_err_text        = lv_err_text
        es_err_resp        = ls_err_resp.

* Deserialize raw output
    /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_raw
                                                    iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                          IMPORTING es_data        = ls_output_llm ).

* Display LLM answer
    DATA: ls_prediction type t_predictions.
    READ TABLE ls_output_llm-predictions INTO ls_prediction INDEX 1.

    WRITE: / ls_prediction-content.

* Close the HTTP Connection
    lo_aiplatform->close( ).

  CATCH /goog/cx_sdk INTO lo_exception.
* Implement suitable error handling
ENDTRY.
