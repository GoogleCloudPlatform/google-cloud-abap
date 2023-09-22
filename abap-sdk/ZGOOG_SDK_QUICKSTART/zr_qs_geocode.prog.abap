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

 REPORT zr_qs_geocode.

 PARAMETERS: p_fg RADIOBUTTON GROUP rbg1 DEFAULT 'X', "Forward Geocoding
             p_rg RADIOBUTTON GROUP rbg1,             "Reverse Geocoding
             p_pg RADIOBUTTON GROUP rbg1,             "Place ID Geocoding
             p_vb RADIOBUTTON GROUP rbg1,             "Viewport Biasing
             p_rb RADIOBUTTON GROUP rbg1,             "Region Biasing
             p_cf RADIOBUTTON GROUP rbg1.             "Component Filtering

 CLASS lcl_geocode_features DEFINITION .
   PUBLIC SECTION.
     METHODS: execute.
   PRIVATE SECTION.
     DATA: mo_geocode TYPE REF TO /goog/cl_geocode.
     METHODS:
       forward_geocoding RAISING /goog/cx_sdk,
       reverse_geocoding RAISING /goog/cx_sdk,
       place_id_geocoding RAISING /goog/cx_sdk,
       viewport_biasing RAISING /goog/cx_sdk,
       region_biasing RAISING /goog/cx_sdk,
       component_filtering RAISING /goog/cx_sdk.
 ENDCLASS.

 CLASS lcl_geocode_features IMPLEMENTATION.

   METHOD execute.

     TRY.

* Open HTTP Connection
         "Replace the client key "TEST-GEOCODE-V1" with the user configured client key
         mo_geocode = NEW /goog/cl_geocode( iv_key_name = 'TEST-GEOCODE-V1' ).

         IF p_fg = abap_true.
           forward_geocoding( ).
         ELSEIF p_rg = abap_true.
           reverse_geocoding( ).
         ELSEIF p_pg = abap_true.
           place_id_geocoding( ).
         ELSEIF p_vb = abap_true.
           viewport_biasing( ).
         ELSEIF p_rb = abap_true.
           region_biasing( ).
         ELSEIF p_cf = abap_true.
           component_filtering( ).
         ENDIF.

* Close HTTP Connection
         mo_geocode->close( ).

       CATCH /goog/cx_sdk INTO DATA(lo_exception).
         MESSAGE lo_exception->get_text( ) TYPE 'E'.
     ENDTRY.
   ENDMETHOD.


   METHOD reverse_geocoding.

     DATA: lv_q_latlng       TYPE string.
     WRITE: |Reverse Geocoding: Convert geographic coordinates into an address.|.
     ULINE.

* Populate relevant parameters
     lv_q_latlng = '37.4223878, -122.0841877'.

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_latlng = lv_q_latlng
       IMPORTING
         es_output   = DATA(ls_output)
         ev_ret_code = DATA(lv_ret_code)
         ev_err_text = DATA(lv_err_text)
         es_err_resp = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful|.
       TRY.
           WRITE:/ |Address: { ls_output-results[ 1 ]-formatted_address }|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.

   ENDMETHOD.


   METHOD place_id_geocoding.

     DATA: lv_q_place_id       TYPE string.
     WRITE: |Place ID geocoding: Convert a Place ID into an address or geographic coordinates.|.
     ULINE.

* Populate relevant parameters
     "A Place ID is a textual identifier that uniquely identifies a place.
     "It is a string of characters that is assigned to a place by Google Maps.
     lv_q_place_id = 'ChIJfTUzxX-F54gRQbkH5E4QhD8'.

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_place_id = lv_q_place_id
       IMPORTING
         es_output     = DATA(ls_output)
         ev_ret_code   = DATA(lv_ret_code)
         ev_err_text   = DATA(lv_err_text)
         es_err_resp   = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful|.
       TRY.
           WRITE:/ |Address: { ls_output-results[ 1 ]-formatted_address }|.
           WRITE:/ |Latitude, Longitude: ({ ls_output-results[ 1 ]-geometry-location-lat }, { ls_output-results[ 1 ]-geometry-location-lng })|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.

   ENDMETHOD.


   METHOD viewport_biasing.
     DATA: lv_q_address TYPE string,
           lt_q_bounds  TYPE /goog/cl_geocode=>ty_t_string.

     WRITE: |Viewport biasing: Constrain the results to a specific viewport.|.
     ULINE.

* Populate relevant parameters
     lv_q_address = 'Washington'.
     "Specifying a bounds parameter defining a bounding box for the San Fernando Valley of Los Angeles
     lt_q_bounds = VALUE #( ( |36.47,-84.72%7C43.39,-65.90| ) ).

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_address = lv_q_address
         it_q_bounds  = lt_q_bounds
       IMPORTING
         es_output    = DATA(ls_output)
         ev_ret_code  = DATA(lv_ret_code)
         ev_err_text  = DATA(lv_err_text)
         es_err_resp  = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful: |.
       TRY.
           WRITE:/ |Address: { ls_output-results[ 1 ]-formatted_address }|.
           WRITE:/ |Latitude, Longitude: ({ ls_output-results[ 1 ]-geometry-location-lat }, { ls_output-results[ 1 ]-geometry-location-lng })|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.


   ENDMETHOD.

   METHOD region_biasing.
     DATA: lv_q_address TYPE string,
           lv_q_region  TYPE string.
     WRITE: |Region biasing: Constrain the results to a specific region.|.
     ULINE.

* Populate relevant parameters
     lv_q_address = 'Toledo'.
     lv_q_region = 'es'.

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_address = lv_q_address
         iv_q_region  = lv_q_region
       IMPORTING
         es_output    = DATA(ls_output)
         ev_ret_code  = DATA(lv_ret_code)
         ev_err_text  = DATA(lv_err_text)
         es_err_resp  = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful|.
       TRY.
           WRITE:/ |Address: { ls_output-results[ 1 ]-formatted_address }|.
           WRITE:/ |Latitude, Longitude: ({ ls_output-results[ 1 ]-geometry-location-lat }, { ls_output-results[ 1 ]-geometry-location-lng })|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.
   ENDMETHOD.

   METHOD component_filtering.
     DATA: lv_q_address    TYPE string,
           lt_q_components TYPE /goog/cl_geocode=>ty_t_string.

     WRITE: |Component filtering: Restrict the results to a specific country, postal code, or locality.|.
     ULINE.

* Populate relevant parameters
     lv_q_address = 'High St hasting'.
     lt_q_components = VALUE #( ( |country:GB| ) ).

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_address    = lv_q_address
         it_q_components = lt_q_components
       IMPORTING
         es_output       = DATA(ls_output)
         ev_ret_code     = DATA(lv_ret_code)
         ev_err_text     = DATA(lv_err_text)
         es_err_resp     = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful|.
       TRY.
           WRITE:/ |Address: { ls_output-results[ 1 ]-formatted_address }|.
           WRITE:/ |Latitude, Longitude: ({ ls_output-results[ 1 ]-geometry-location-lat }, { ls_output-results[ 1 ]-geometry-location-lng })|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.
   ENDMETHOD.


   METHOD forward_geocoding.

     DATA: lv_q_address       TYPE string.
     WRITE: |Forward Geocoding: Convert an address into geographic coordinates.|.
     ULINE.

* Populate relevant parameters
     lv_q_address = '1600 Amphitheatre Parkway, Mountain View, CA'.

* Call API method: org.openapitools.api.GeocodingAPI
     CALL METHOD mo_geocode->geocode
       EXPORTING
         iv_q_address = lv_q_address
       IMPORTING
         es_output    = DATA(ls_output)
         ev_ret_code  = DATA(lv_ret_code)
         ev_err_text  = DATA(lv_err_text)
         es_err_resp  = DATA(ls_err_resp).

     IF mo_geocode->is_success( lv_ret_code ) AND mo_geocode->is_status_ok( ).
       WRITE: / |Geocode Lookup Successful|.
       TRY.
           WRITE:/ |Latitude, Longitude: ({ ls_output-results[ 1 ]-geometry-location-lat }, { ls_output-results[ 1 ]-geometry-location-lng })|.
         CATCH cx_sy_itab_line_not_found.
       ENDTRY.
     ELSE.
       MESSAGE lv_err_text TYPE 'E'.
     ENDIF.

   ENDMETHOD.

 ENDCLASS.

 START-OF-SELECTION.
   DATA(lo_geocode) = NEW lcl_geocode_features( ).
   lo_geocode->execute( ).
