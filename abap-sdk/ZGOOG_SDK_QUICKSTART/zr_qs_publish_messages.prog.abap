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

REPORT zr_qs_publish_messages.

* Data Declaration
DATA:
  lv_p_projects_id TYPE string,
  lv_p_topics_id   TYPE string,
  ls_input         TYPE /goog/cl_pubsub_v1=>ty_023.

TRY.
* Instantiate the client stub
    DATA(lo_pubsub) = NEW /goog/cl_pubsub_v1( iv_key_name = 'DEMO_PUBSUB' ).

* Pass the relevant input parameters
    lv_p_topics_id = 'SAMPLE_TOPIC_01'.
    lv_p_projects_id = lo_pubsub->gv_project_id.
    APPEND VALUE #( data = cl_http_utility=>encode_base64( 'Hello World!' ) ) to ls_input-messages.

* Call the API
    CALL METHOD lo_pubsub->publish_topics
      EXPORTING
        iv_p_projects_id = lv_p_projects_id
        iv_p_topics_id   = lv_p_topics_id
        is_input         = ls_input
      IMPORTING
        es_output        = DATA(ls_output)
        ev_ret_code      = DATA(lv_ret_code)
        ev_err_text      = DATA(lv_err_text)
        es_err_resp      = DATA(ls_err_resp).

* Handle the output
    IF lo_pubsub->is_success( lv_ret_code ).
      MESSAGE 'Message was published!' TYPE 'S'.
    ELSE.
      MESSAGE 'Message was not published!' TYPE 'E'.
    ENDIF.

* Close the HTTP Connection
    lo_pubsub->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
