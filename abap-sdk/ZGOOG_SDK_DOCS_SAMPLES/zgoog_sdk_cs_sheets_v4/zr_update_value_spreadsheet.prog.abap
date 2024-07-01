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
*&---------------------------------------------------------------------*
*& Report ZR_UPDATE_VALUE_SPREADSHEET
*&---------------------------------------------------------------------*
*& Update values in a spreadsheet
*& In this code Sample we are fetching data from ADRC table and updating
*& the same in an existing spreadsheet
*&---------------------------------------------------------------------*
REPORT zr_update_value_spreadsheet_in.

TYPES: ty_t_string TYPE TABLE OF string WITH NON-UNIQUE DEFAULT KEY .

TYPES : BEGIN OF lty_adrc,
          addrnumber TYPE ad_addrnum,
          name1      TYPE ad_name1,
        END OF lty_adrc,
        BEGIN OF ty_string,
          values TYPE ty_t_string,
        END OF ty_string.

DATA : lv_range          TYPE string,
       lv_spreadsheet_id TYPE string,
       ls_data_app       TYPE /goog/cl_sheets_v4=>ty_240,
       ls_values         TYPE REF TO data.


DATA: lt_in_tab    TYPE ty_t_string,
      lt_out_tab   TYPE STANDARD TABLE OF ty_string,
      ls_out_tab   TYPE ty_string,
      lt_adrc_upd  TYPE STANDARD TABLE OF lty_adrc,
      ls_input_upd TYPE /goog/cl_sheets_v4=>ty_240.

FIELD-SYMBOLS : <fs_value> TYPE any.
TRY.
    " Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_sheets_v4( iv_key_name = 'SHEETS_TEST' ).

    lv_range = 'Sheet1!A5' . " Range provided using A1 notation " Updating data in Sheet1 from Cell A5
    lv_spreadsheet_id = 'XXX'. " Populate ID of the spreadsheet to be updated

    SELECT addrnumber name1
    FROM adrc
    INTO CORRESPONDING FIELDS OF TABLE lt_adrc_upd
    UP TO 20 ROWS.
    DELETE lt_adrc_upd WHERE name1 IS INITIAL.

    LOOP AT lt_adrc_upd INTO DATA(ls_adrc).
      APPEND ls_adrc-addrnumber TO lt_in_tab.
      APPEND ls_adrc-name1 TO lt_in_tab.   " Data to be updated in a single row is added in lt_in_tab
      ls_out_tab-values = lt_in_tab.
      APPEND ls_out_tab TO lt_out_tab.   "Nested table of all the row data
      CLEAR lt_in_tab.
    ENDLOOP.

    CREATE DATA ls_values TYPE TABLE OF ty_string.
    ASSIGN ls_values->* TO <fs_value>.
    <fs_value> = lt_out_tab.
    ls_input_upd-values = ls_values.   " Mapping to type data

    " Call API Method : Update Values
    CALL METHOD lo_client->update_values
      EXPORTING
        iv_q_valueinputoption = 'RAW'               " valueInputOption " The values the user has entered will not be parsed and will be stored as-is.
        iv_p_range            = lv_range            " range
        iv_p_spreadsheet_id   = lv_spreadsheet_id   " spreadsheetId
        is_input              = ls_input_upd        " ValueRange
      IMPORTING
        es_output             = DATA(ls_output)     " UpdateValuesResponse
        ev_ret_code           = DATA(lv_ret_code)   " Return Code
        ev_err_text           = DATA(lv_err_text)   " Error Text
        es_err_resp           = DATA(ls_err_resp).  " Error Response


    IF lo_client->is_success( lv_ret_code ).
* Display the ID of the updated spreadsheet
      cl_demo_output=>display( ls_output-spreadsheet_id ).
    ELSE.
      MESSAGE lv_err_text  TYPE 'E'.

    ENDIF.
  CATCH /goog/cx_sdk INTO DATA(lo_exception). " ABAP SDK for Google Cloud: Exception Class
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
