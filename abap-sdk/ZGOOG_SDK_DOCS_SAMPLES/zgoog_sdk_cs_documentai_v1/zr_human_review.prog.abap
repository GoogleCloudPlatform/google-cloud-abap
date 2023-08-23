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

REPORT zr_human_review.

DATA:
  lt_bin_data        TYPE STANDARD TABLE OF char1024,
  ls_input_proc      TYPE /goog/cl_documentai_v1=>ty_084,
  ls_input           TYPE /goog/cl_documentai_v1=>ty_092,
  lv_p_projects_id   TYPE string,
  lv_p_locations_id  TYPE string,
  lv_p_processors_id TYPE string,
  lv_file            TYPE string,
  lv_file_length     TYPE i,
  lv_xfile           TYPE xstring,
  lv_output          TYPE string.

lv_file = 'BROWSED_FILE_PATH'.  "File path of document to be processed

* Call function module to upload the file contents
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename   = lv_file
    filetype   = 'BIN'
  IMPORTING
    filelength = lv_file_length
  TABLES
    data_tab   = lt_bin_data.

* Call function module to convert file contents from 'BIN' to 'XSTRIG' format
CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
  EXPORTING
    input_length = lv_file_length
  IMPORTING
    buffer       = lv_xfile
  TABLES
    binary_tab   = lt_bin_data.

* Call function module to convert the file contents from 'XSTRING' to 'base-64 encoded' format
CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
  EXPORTING
    input  = lv_xfile
  IMPORTING
    output = lv_output.

TRY.
* Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_documentai_v1( iv_key_name = 'CLIENT_KEY' ).

* Populate relevant parameters
    lv_p_projects_id = lo_client->gv_project_id.
    lv_p_locations_id = 'LOCATION_ID'.  "Location ID of the Processor
    lv_p_processors_id = 'PROCESSOR_ID'.  "Processor ID of the Processor
    ls_input_proc-raw_document-content   = lv_output.  "File contents in base64-encoded format
    ls_input_proc-raw_document-mime_type = 'MIME_TYPE'.  "MIME Type of the file

* Call API method to process the document
    CALL METHOD lo_client->process_processors
      EXPORTING
        iv_p_projects_id   = lv_p_projects_id
        iv_p_locations_id  = lv_p_locations_id
        iv_p_processors_id = lv_p_processors_id
        is_input           = ls_input_proc
      IMPORTING
        es_output          = DATA(ls_output_proc)
        ev_ret_code        = DATA(lv_ret_code)
        ev_err_text        = DATA(lv_err_text)
        es_err_resp        = DATA(ls_err_resp).
    IF /goog/cl_http_client=>is_success( lv_ret_code ).
* Pass the processed document to importing parameters to send for 'Human Review'
      ls_input-inline_document = ls_output_proc-document.

* Call API method to send processed document for 'Human Review'
      CALL METHOD lo_client->review_document_human_revie
        EXPORTING
          iv_p_projects_id   = lv_p_projects_id
          iv_p_locations_id  = lv_p_locations_id
          iv_p_processors_id = lv_p_processors_id
          is_input           = ls_input
        IMPORTING
          es_output          = DATA(ls_output)
          ev_ret_code        = lv_ret_code
          ev_err_text        = lv_err_text
          es_err_resp        = ls_err_resp.
      IF /goog/cl_http_client=>is_success( lv_ret_code ).
        MESSAGE 'Success' TYPE 'S'.
      ELSE.
        MESSAGE lv_err_text TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

* Close HTTP Connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
