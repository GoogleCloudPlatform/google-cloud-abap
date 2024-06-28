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
*& Report ZR_CREATE_SPREADSHEET
*&---------------------------------------------------------------------*
*& Create a Spreadsheet
** In this sample code data is fetched from ADRC table and
*&populated in a newly created spreadsheet
*&---------------------------------------------------------------------*
REPORT zr_create_spreadsheet.

TYPES :
  BEGIN OF lty_adrc,
    addrnumber TYPE ad_addrnum,
    name1      TYPE ad_name1,
  END OF lty_adrc.

DATA : ls_input    TYPE /goog/cl_sheets_v4=>ty_202,
       ls_ss_prop  TYPE /goog/cl_sheets_v4=>ty_203,
       lt_sheets   TYPE /goog/cl_sheets_v4=>ty_t_195,
       ls_sheet    TYPE /goog/cl_sheets_v4=>ty_195,
       ls_value    TYPE /goog/cl_sheets_v4=>ty_059,
       lt_values   TYPE /goog/cl_sheets_v4=>ty_t_059,
       ls_row_data TYPE /goog/cl_sheets_v4=>ty_189,
       lt_row_data TYPE /goog/cl_sheets_v4=>ty_t_189,
       ls_data     TYPE /goog/cl_sheets_v4=>ty_144,
       lt_data     TYPE /goog/cl_sheets_v4=>ty_t_144.

DATA : lt_adrc TYPE STANDARD TABLE OF lty_adrc,
       ls_adrc TYPE lty_adrc.


TRY.
    " Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_sheets_v4( iv_key_name = 'ABAP_SDK_WORKSPACE' ).

    " Populate properties of the spreadsheet
    ls_ss_prop-title = 'Address Book'.

    " Populate properties of the sheet
    ls_sheet-properties-sheet_id = '0001'.
    ls_sheet-properties-title    = 'Address Sheet'.

    " Populate data in the sheet
    SELECT addrnumber name1 FROM adrc INTO CORRESPONDING FIELDS OF TABLE lt_adrc UP TO 3 ROWS WHERE name1 IS NOT NULL.
    LOOP AT lt_adrc INTO ls_adrc.
      CLEAR : lt_values.
      ls_value-user_entered_value-string_value = ls_adrc-addrnumber.
      APPEND ls_value TO lt_values.
      ls_value-user_entered_value-string_value = ls_adrc-name1.
      APPEND ls_value TO lt_values. " All the values for be entered in a single row are appended in table lt_values in single iteration
      ls_row_data-values = lt_values.
      APPEND ls_row_data TO lt_row_data.
    ENDLOOP.

    ls_data-row_data     = lt_row_data. " lt_row_data contains data for multiple rows
    ls_data-start_column = 0.
    ls_data-start_row    = 0.
    APPEND ls_data TO lt_data.

    ls_sheet-data = lt_data.
    APPEND ls_sheet TO lt_sheets. " lt_sheets contains data for multiple sheets of the spreadsheet

    ls_input-properties = ls_ss_prop.
    ls_input-sheets = lt_sheets.

    " Call API method : Create Spreadsheets
    CALL METHOD lo_client->create_spreadsheets
      EXPORTING
        is_input    = ls_input            " Spreadsheet
      IMPORTING
        es_output   = DATA(ls_output)     " Spreadsheet
        ev_ret_code = DATA(lv_ret_code)   " Return Code
        ev_err_text = DATA(lv_err_text)   " Error Text
        es_err_resp = DATA(lv_err_resp).  " Error Response

    IF lo_client->is_success( lv_ret_code ).
      " Display the URL of the created spreadsheet
      cl_demo_output=>display( ls_output-spreadsheet_url ).
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.

    ENDIF.

  CATCH /goog/cx_sdk INTO DATA(lo_exception). " ABAP SDK for Google Cloud: Exception Class
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
