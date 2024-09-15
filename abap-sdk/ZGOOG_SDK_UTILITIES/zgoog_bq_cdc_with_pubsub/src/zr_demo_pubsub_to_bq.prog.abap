**********************************************************************
*  Copyright 2024 Google LLC                                         *
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
REPORT zr_demo_pubsub_to_bq.

PARAMETERS:
  p_src  TYPE zgoog_pubsub_bq-datasource DEFAULT 'DEMO_CDC',
  p_date TYPE datum DEFAULT sy-datum,
  p_int  TYPE i DEFAULT 100,
  p_oper TYPE c DEFAULT 'I'.

TYPES:
  BEGIN OF t_message,
    date           TYPE string,
    int_value      TYPE i,
    last_updated   TYPE string,
    int_timestamp  TYPE int8,
    operation_flag TYPE string,
  END OF t_message,

  tt_message TYPE STANDARD TABLE OF t_message WITH EMPTY KEY.

DATA: lt_message TYPE tt_message.

DATA: lv_timestamp TYPE timestamp.
DATA: lv_timestamp_str TYPE string.

GET TIME STAMP FIELD lv_timestamp.

lv_timestamp_str  = lv_timestamp.
CONDENSE lv_timestamp_str.

CONCATENATE lv_timestamp_str+0(4)  '-' lv_timestamp_str+4(2)  '-'
            lv_timestamp_str+6(2)  'T' lv_timestamp_str+8(2)  ':'
            lv_timestamp_str+10(2) ':' lv_timestamp_str+12(2) 'Z'
       INTO lv_timestamp_str.

lt_message =
   VALUE #(
     ( date = p_date(4) && '-' && p_date+4(2) && '-' && p_date+6(2)
       int_value =  p_int
       int_timestamp = lv_timestamp
       last_updated  = lv_timestamp_str
       operation_flag = p_oper ) ) .

zgoog_cl_pubsub_to_bq=>send_to_pubsub(
 EXPORTING
   it_data                       = lt_message
   iv_datasource                 = p_src
   iv_set_cdc_fields             = 'X'
   iv_cdc_field                  = 'OPERATION_FLAG'
   iv_cdc_del_value              = 'D'
   iv_change_sequence_number_fld = 'INT_TIMESTAMP'
 IMPORTING
   ev_ret_code = DATA(lv_ret_code)
   ev_err_text = DATA(lv_err_text) ).

IF lv_ret_code IS NOT INITIAL AND lv_ret_code <> 200.
  cl_demo_output=>display( data = lv_err_text ).
ELSE.
  cl_demo_output=>display(
   EXPORTING
     data    = lt_message
     name    = 'Record successfully sent to Pub/Sub' ).
ENDIF.
