**********************************************************************
*  Copyright 2025 Google LLC                                         *
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
*& Report ZR_CREATE_SHEET_IN_DRIVE
*&---------------------------------------------------------------------*
*& Create a spreadsheet in shared drive and insert values
*& In this sample code: A spreadsheet is created in a shared drive
*& and then data is fetched from ADRC table and inserted into this sheet
*&---------------------------------------------------------------------*
REPORT zr_create_sheet_in_drive.

TYPES: ltt_string TYPE TABLE OF string WITH NON-UNIQUE DEFAULT KEY .

TYPES: BEGIN OF lty_adrc,
         addrnumber TYPE ad_addrnum,
         name1      TYPE ad_name1,
       END OF lty_adrc,
       BEGIN OF lty_string,
         values TYPE ltt_string,
       END OF lty_string.

DATA lv_supportsalldrives TYPE string.
DATA lv_range             TYPE string.
DATA lv_spreadsheet_id    TYPE string.
DATA ls_input_drive       TYPE /goog/cl_drive_v3=>ty_010.
DATA ls_sheet_input       TYPE /goog/cl_sheets_v4=>ty_240.
DATA ls_input_upd         TYPE /goog/cl_sheets_v4=>ty_240.
DATA ls_out_tab           TYPE lty_string.
DATA ls_data_app          TYPE /goog/cl_sheets_v4=>ty_240.
DATA ls_values            TYPE REF TO data.
DATA lt_in_tab            TYPE ltt_string.
DATA lt_out_tab           TYPE STANDARD TABLE OF lty_string.
DATA lt_adrc_upd          TYPE STANDARD TABLE OF lty_adrc.


FIELD-SYMBOLS : <fs_value> TYPE any.


" TODO Developer: Replace with actual values
" ------------------------------------------------------------------------------------
" Replace with your actual client key name
CONSTANTS lc_client_key       TYPE /goog/keyname VALUE '<CLIENT_KEY_NAME>'.
" Desired name for the spreadsheet
CONSTANTS lc_spreadsheet_name TYPE string      VALUE '<Spreadsheet_Name>'.
" ID of the target Shared Drive
CONSTANTS lc_shared_drive_id  TYPE string      VALUE '<Shared_Drive_ID>'.
" ID of the folder within the Shared Drive
CONSTANTS lc_parent_folder_id TYPE string      VALUE '<Folder_ID>'.
" ------------------------------------------------------------------------------------


" Step 1: Create a new Spreadsheet in a Shared Drive
TRY.
    " Create an instance of the Google Drive V3 client
    DATA(lo_client) = NEW /goog/cl_drive_v3( iv_key_name = lc_client_key ).

    " supportsAllDrives should be true for shared drives
    lv_supportsalldrives = 'true'.

    ls_input_drive-name = lc_spreadsheet_name.

    " Set the MIME type for a Google Spreadsheet
    ls_input_drive-mime_type = 'application/vnd.google-apps.spreadsheet'.

    " Specify the parent folder ID where the spreadsheet will be created
    " For a Shared Drive, this should be the ID of a folder within that Shared Drive.
    " The main Shared Drive ID can also be used if creating directly in the root.
    APPEND lc_parent_folder_id TO ls_input_drive-parents.

    " Call the Drive API files.create method
    lo_client->create_files(
      EXPORTING
        iv_q_supportsalldrives = lv_supportsalldrives
        is_input               = ls_input_drive
      IMPORTING
        es_output              = DATA(ls_drive_resp)
        ev_ret_code            = DATA(lv_ret_code)
        ev_err_text            = DATA(lv_err_text)
        es_err_resp            = DATA(ls_err_resp)
    ).

    "  Handle the API Response
    IF lo_client->is_success( lv_ret_code ).
      " Success: Display details of the newly created spreadsheet
      WRITE: / 'Google Spreadsheet created successfully!'.
      WRITE: / 'File ID:', ls_drive_resp-id.
      WRITE: / 'File Name:', ls_drive_resp-name.
    ELSE.
      " Error: Display error details
      WRITE: / 'Error creating Google Spreadsheet:'.
      WRITE: / 'Return Code:', lv_ret_code.
      WRITE: / 'Error Text:', lv_err_text.
      IF ls_err_resp IS NOT INITIAL.
        WRITE: / 'Error Type:', ls_err_resp-error.
        WRITE: / 'Error Description:', ls_err_resp-error_description.
      ENDIF.
      " Close the HTTP connection
      lo_client->close( ).
      EXIT.  "No need to proceed further
    ENDIF.

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.


"Step 2: Update Values in the Spreadsheet using Spreadsheet ID
TRY.
    " Create an instance of the Google Spreadsheet V4 client
    DATA(lo_sheet) = NEW /goog/cl_sheets_v4( iv_key_name = lc_client_key ).

    ls_sheet_input-major_dimension = 'ROWS'.
    ls_sheet_input-range = 'Sheet1!A5'.

    SELECT addrnumber name1
      FROM adrc
      INTO CORRESPONDING FIELDS OF TABLE lt_adrc_upd
      UP TO 20 ROWS.

    DELETE lt_adrc_upd WHERE name1 IS INITIAL.

    LOOP AT lt_adrc_upd INTO DATA(ls_adrc).
      APPEND ls_adrc-addrnumber TO lt_in_tab.
      APPEND ls_adrc-name1 TO lt_in_tab.
      ls_out_tab-values = lt_in_tab.
      APPEND ls_out_tab TO lt_out_tab.
      CLEAR lt_in_tab.
    ENDLOOP.

    CREATE DATA ls_values TYPE TABLE OF lty_string.
    ASSIGN ls_values->* TO <fs_value>.
    <fs_value> = lt_out_tab.
    ls_sheet_input-values = ls_values.

    " Call API method : Update Values
    lo_sheet->update_values(
      EXPORTING
        iv_p_range          = 'Sheet1!A5'
        iv_p_spreadsheet_id = ls_drive_resp-id
        iv_q_valueinputoption = 'RAW'
        is_input            = ls_sheet_input
      IMPORTING
        es_output           = DATA(ls_sheet_resp)
        ev_ret_code         = lv_ret_code
        ev_err_text         = lv_err_text
        es_err_resp         = ls_err_resp
    ).
    "  Handle the API Response
    IF lo_client->is_success( lv_ret_code ).
      " Success: Display details of the newly created spreadsheet
      WRITE: / 'Google Spreadsheet values updated successfully'.
      WRITE: / | Rows updated: { ls_sheet_resp-updated_rows } |.
      WRITE: / | Columns updated: { ls_sheet_resp-updated_columns } |.
    ELSE.
      " Error: Display error details
      WRITE: / 'Error creating Google Spreadsheet:'.
      WRITE: / 'Return Code:', lv_ret_code.
      WRITE: / 'Error Text:', lv_err_text.
      IF ls_err_resp IS NOT INITIAL.
        WRITE: / 'Error Type:', ls_err_resp-error.
        WRITE: / 'Error Description:', ls_err_resp-error_description.
      ENDIF.
      " Close the HTTP connection
      lo_client->close( ).
      EXIT.  "No need to proceed further
    ENDIF.

  CATCH /goog/cx_sdk INTO lo_exception.
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
