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

REPORT zr_qs_validate_address.

* data declarations
DATA: lv_ret_code          TYPE i,
      lv_err_text          TYPE string,
      ls_input             TYPE /goog/cl_addrvaldn_v1=>ty_012,
      ls_output            TYPE /goog/cl_addrvaldn_v1=>ty_013,
      ls_err_resp          TYPE /goog/err_resp,
      lo_exception         TYPE REF TO /goog/cx_sdk,
      lo_address_validator TYPE REF TO /goog/cl_addrvaldn_v1.

* instantiate api client stub
TRY.
    CREATE OBJECT lo_address_validator
      EXPORTING
        iv_key_name = 'DEMO_ADDR_VAL'.

* pass the address to be validated
    ls_input-address-region_code = 'US'.
    ls_input-address-locality = 'Mountain View'.
    APPEND '1600, Amphitheatre, Parkway' TO ls_input-address-address_lines.

* call the api method to validate address
    CALL METHOD lo_address_validator->validate_address
      EXPORTING
        is_input    = ls_input
      IMPORTING
        es_output   = ls_output
        ev_ret_code = lv_ret_code
        ev_err_text = lv_err_text
        es_err_resp = ls_err_resp.
    IF lo_address_validator->is_success( lv_ret_code ) = abap_true AND
      ls_output-result-verdict-address_complete = abap_true.
      WRITE: / 'Address is complete'.
    ENDIF.

  CATCH /goog/cx_sdk INTO lo_exception.
* write code here to handle exceptions
ENDTRY.
