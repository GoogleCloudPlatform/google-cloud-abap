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
REPORT zr_qs_sheets_get_values.

* Types declaration for Sheets structure
TYPES:
  BEGIN OF lty_sheet_data,
    item_number    TYPE string,
    product_name   TYPE string,
    quantity       TYPE string,
    price_per_item TYPE string,
    currency       TYPE string,

  END OF lty_sheet_data.

* Data declarations
DATA:
  lt_data             TYPE string_table,
  lt_sheet_data       TYPE STANDARD TABLE OF lty_sheet_data,
  ls_sheet_data       TYPE lty_sheet_data,
  lv_p_range          TYPE string,
  lv_p_spreadsheet_id TYPE string.

FIELD-SYMBOLS:
  <lt_data>          TYPE data.

TRY.
* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_sheets_v4( iv_key_name = 'ABAP_SDK_WORKSPACE' ).

* Populate relevant parameters
    lv_p_range = '<Worksheet Name>'.    "Pass the Worksheet Name here
    lv_p_spreadsheet_id = '<Sheet ID>'. "Pass the Sheet ID here

* Call API method: sheets.spreadsheets.values.get
    CALL METHOD lo_client->get_values
      EXPORTING
        iv_p_range          = lv_p_range
        iv_p_spreadsheet_id = lv_p_spreadsheet_id
      IMPORTING
        es_output           = DATA(ls_output)
        ev_ret_code         = DATA(lv_ret_code)
        ev_err_text         = DATA(lv_err_text)
        es_err_resp         = DATA(ls_err_resp).
    IF lo_client->is_success( lv_ret_code ).
* Sheets API returns the sheet data in a nested string table reference
* Iterate internal table to get the sheet data and map to a local types with respect to the structure in Sheet
      LOOP AT ls_output-values->* ASSIGNING FIELD-SYMBOL(<ls_values>).
        ASSIGN <ls_values>->* TO <lt_data>.
        DATA(lv_json) = /goog/cl_json_util=>serialize_json( is_data = <lt_data> ).
        /goog/cl_json_util=>deserialize_json( EXPORTING iv_json        = lv_json
                                                        iv_pretty_name = /ui2/cl_json=>pretty_mode-extended
                                              IMPORTING es_data        = lt_data ).
        LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
          CASE sy-tabix.
            WHEN 1.
              ls_sheet_data-item_number = <ls_data>.
            WHEN 2.
              ls_sheet_data-product_name = <ls_data>.
            WHEN 3.
              ls_sheet_data-quantity = <ls_data>.
            WHEN 4.
              ls_sheet_data-price_per_item = <ls_data>.
            WHEN 5.
              ls_sheet_data-currency = <ls_data>.

          ENDCASE.

        ENDLOOP.

        APPEND ls_sheet_data TO lt_sheet_data.
        CLEAR ls_sheet_data.

      ENDLOOP.
* Display read Sheet data
      cl_demo_output=>display( lt_sheet_data ).
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.

    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
