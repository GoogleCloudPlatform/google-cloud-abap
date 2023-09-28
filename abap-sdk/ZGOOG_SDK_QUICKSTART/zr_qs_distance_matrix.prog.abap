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
REPORT zr_qs_distance_matrix NO STANDARD PAGE HEADING.

"Data declarations
DATA:
  lt_destinations   TYPE /goog/cl_distance_matrix=>ty_t_string,
  lt_origins        TYPE /goog/cl_distance_matrix=>ty_t_string,
  lv_departure_time TYPE /goog/num_float,
  ls_output         TYPE /goog/if_maps=>ty_014.

TRY.
    "Instantiate the client stub - replace the client key name with the one you have configured in /GOOG/CLIENT_KEY table
    DATA(lo_dist_matrix) = NEW /goog/cl_distance_matrix( iv_key_name   = 'TEST-DISTANCE_MATRIX' ).

    WRITE:/ |Scenario 1 ==> Single origin and single destination|.

    lt_origins = VALUE #( ( |NewYork City, NY| ) ).
    lt_destinations = VALUE #( ( |Washington DC| ) ).

    "Call the Google Map Platform's Distance Matrix API
    CALL METHOD lo_dist_matrix->distance_matrix
      EXPORTING
        it_q_destinations = lt_destinations
        it_q_origins      = lt_origins
      IMPORTING
        es_output         = ls_output
        ev_ret_code       = DATA(lv_ret_code)
        ev_err_text       = DATA(lv_err_text)
        es_err_resp       = DATA(ls_err_resp).
    IF lo_dist_matrix->is_success( lv_ret_code ).
      IF lo_dist_matrix->is_status_ok( ).
        TRY.
            WRITE:/ |Travel Mode: driving(default)|.
            WRITE:/ |Distance Between the Places: { ls_output-rows[ 1 ]-elements[ 1 ]-distance-text }|.
            WRITE:/ |Estimated Duration of Travel:{ ls_output-rows[ 1 ]-elements[ 1 ]-duration-text }|.
          CATCH cx_sy_itab_line_not_found.
            WRITE:/ |Distance & Travel Time Not Found|.
        ENDTRY.
      ELSE.
        WRITE: ls_output-status && ls_output-error_message.
      ENDIF.
    ELSE.
      WRITE:/ |Error message: { lv_err_text }|.
    ENDIF.

    WRITE:/ |-------------------------------------------------------|.
    WRITE:/ |Senario 2 ==> Multiple origin and multiple destinations|.

    lt_origins = VALUE #( ( |San Francisco| )
                            ( |Victoria BC| ) ).

    lt_destinations = VALUE #( ( |Vancouver BC| )
                                 ( |Seattle| ) ).

    "Call the Google Map Platform's Distance Matrix API
    CALL METHOD lo_dist_matrix->distance_matrix
      EXPORTING
        it_q_destinations = lt_destinations
        it_q_origins      = lt_origins
      IMPORTING
        es_output         = ls_output
        ev_ret_code       = lv_ret_code
        ev_err_text       = lv_err_text
        es_err_resp       = ls_err_resp.
    IF lo_dist_matrix->is_success( lv_ret_code ).
      IF lo_dist_matrix->is_status_ok( ).
        TRY.
            WRITE:/ |Travel Mode: driving(default)|.
            WRITE:/ |Distance Between the San Franciso & Vancouver BC: { ls_output-rows[ 1 ]-elements[ 1 ]-distance-text }|.
            WRITE:/ |Distance Between San Franciso & Seattle: { ls_output-rows[ 1 ]-elements[ 2 ]-distance-text }|.
            WRITE:/ |Distance Between Vancouver BC & Victoria BC: { ls_output-rows[ 2 ]-elements[ 1 ]-distance-text }|.
            WRITE:/ |Distance Between Vancouver BC & Seattle: { ls_output-rows[ 2 ]-elements[ 2 ]-distance-text }|.
          CATCH cx_sy_itab_line_not_found.
            WRITE:/ |Error in retrieving data|.
        ENDTRY.
      ELSE.
        WRITE: ls_output-status && ls_output-error_message.
      ENDIF.
    ELSE.
      WRITE:/ |Error message: { lv_err_text }|.
    ENDIF.

    WRITE:/ |-------------------------------------------------------|.
    WRITE:/ |Senario 3 ==> Call with preferred modes and stipulated departure time|.

    lt_origins = VALUE #( ( |NewYork City, NY| ) ).
    lt_destinations = VALUE #( ( |Washington DC| ) ).

    cl_pco_utility=>convert_abap_timestamp_to_java(
  EXPORTING
    iv_date      = sy-datum
    iv_time      = sy-uzeit
  IMPORTING
    ev_timestamp = lv_departure_time
).
    "Convert millisecond to seconds
    lv_departure_time = lv_departure_time / 1000.

    "Call the Google Map Platform's Distance Matrix API
    CALL METHOD lo_dist_matrix->distance_matrix
      EXPORTING
        it_q_destinations   = lt_destinations
        it_q_origins        = lt_origins
        iv_q_departure_time = lv_departure_time
        iv_q_mode           = 'driving'
        iv_q_traffic_model  = 'best_guess'
        iv_q_transit_mode   = 'bus'
      IMPORTING
        es_output           = ls_output
        ev_ret_code         = lv_ret_code
        ev_err_text         = lv_err_text
        es_err_resp         = ls_err_resp.
    IF lo_dist_matrix->is_success( lv_ret_code ).
      IF lo_dist_matrix->is_status_ok( ).
        TRY.
            WRITE:/ |Travel Mode: driving(default) - Transit Mode: bus|.
            WRITE:/ |Distance Between the Places: { ls_output-rows[ 1 ]-elements[ 1 ]-distance-text }|.
            WRITE:/ |Duration of Travel: { ls_output-rows[ 1 ]-elements[ 1 ]-duration-text }|.
            WRITE:/ |Duration in Traffic: { ls_output-rows[ 1 ]-elements[ 1 ]-duration_in_traffic-text }|.

          CATCH cx_sy_itab_line_not_found.
            WRITE:/ |Error in retrieving data|.
        ENDTRY.
      ELSE.
        WRITE: ls_output-status && ls_output-error_message.
      ENDIF.
    ELSE.
      WRITE:/ |Error message: { lv_err_text }|.
    ENDIF.


    WRITE:/ |-------------------------------------------------------|.
    WRITE:/ |Senario 4 ==> Google Maps Analogous Call|.
    DATA: lv_str TYPE string.

    lv_str = cl_http_utility=>escape_url( unescaped = 'SAP LABS, Whitefield, Bengaluru, Karnataka, India' ).
    lt_origins = VALUE #( ( lv_str ) ).

    lv_str = cl_http_utility=>escape_url( unescaped = '|Google India, Ferns city, Doddanekkundi, Bengaluru, Karnataka, India' ).
    lt_destinations = VALUE #( ( lv_str ) ).

    "Call the Google Map Platform's Distance Matrix API
    CALL METHOD lo_dist_matrix->distance_matrix
      EXPORTING
        it_q_destinations = lt_destinations
        it_q_origins      = lt_origins
        iv_q_mode         = 'walking'
      IMPORTING
        es_output         = ls_output
        ev_ret_code       = lv_ret_code
        ev_err_text       = lv_err_text
        es_err_resp       = ls_err_resp.
    IF lo_dist_matrix->is_success( lv_ret_code ).
      IF lo_dist_matrix->is_status_ok( ).
        TRY.
            WRITE:/ |Travel Mode: walking|.
            WRITE:/ |Distance Between the Places: { ls_output-rows[ 1 ]-elements[ 1 ]-distance-text }|.
            WRITE:/ |Duration of Travel: { ls_output-rows[ 1 ]-elements[ 1 ]-duration-text }|.
          CATCH cx_sy_itab_line_not_found.
            WRITE:/ |Error in retrieving data|.
        ENDTRY.
      ELSE.
        WRITE: ls_output-status && ls_output-error_message.
      ENDIF.
    ELSE.
      WRITE:/ |Error message: { lv_err_text }|.
    ENDIF.

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    "Display exception message
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
