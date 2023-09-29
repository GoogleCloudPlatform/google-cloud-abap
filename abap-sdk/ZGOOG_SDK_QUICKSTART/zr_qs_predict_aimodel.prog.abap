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


CONSTANTS: lc_ob type c VALUE '{',
           lc_cb type c VALUE '}'.

TRY.
* Instantiate the client stub & Call API method
    DATA(lv_raw) = VALUE string( ).
    DATA(lo_aiplatform) = NEW /goog/cl_aiplatform_v1( iv_key_name = 'DEMO_AIPLATFORM' ).

    lo_aiplatform->predict_models(
      EXPORTING
        iv_p_projects_id   = CONV #( lo_aiplatform->gv_project_id )
        iv_p_locations_id  = 'us-central1'
        iv_p_publishers_id = 'google'
        iv_p_models_id     = 'text-bison'
        is_input           = VALUE #(
parameters = NEW t_parameters(
      max_output_tokens  = 256
      temperature = '0.2'
      top_k = '40'
      top_p  = '0.8' )
instances = NEW tt_instances( ( content =
* Context to AI Model
 |I will give you an email context, you identify a function name with the parameters from the email to match given cases as following, and | &&
 |return the results in json format, | &&
 |provide concise answers, no explanation. | &&
 |CASE 1. When a retailer order : | && lc_ob &&
* Desired Output for an order email
 |"function":"Z_ORDER_DEMO","parameters": | && lc_ob && |"IVendor":Vendor name,"IItem":Item name,"IBoxqty":Box qty| && lc_cb && lc_cb &&
 |CASE 2. Others : None | &&
* Actual email content passed to AI Model. You can also try with different verbiage and evaluate the output
 |Email Content: Hi Team, I need 5 cases of Pepsi 0 Sugar.| ) ) )
      IMPORTING
        es_raw             = lv_raw
        ev_ret_code        = data(lv_ret_code)
        ev_err_text        = data(lv_err_text)
        es_err_resp        = data(ls_err_resp) ).

* Close the HTTP Connection
    lo_aiplatform->close( ).

* Deserialize raw output
    DATA(ls_output_llm) = VALUE t_output( ).
    /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_raw
                                                    iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                          IMPORTING es_data        = ls_output_llm ).

* Display LLM answer
    WRITE: / VALUE #( ls_output_llm-predictions[ 1 ]-content OPTIONAL ).

  CATCH /goog/cx_sdk.
* Implement suitable error handling
ENDTRY.
