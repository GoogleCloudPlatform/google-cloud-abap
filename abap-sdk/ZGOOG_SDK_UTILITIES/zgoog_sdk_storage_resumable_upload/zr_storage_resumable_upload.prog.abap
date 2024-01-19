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
REPORT zr_storage_resumable_upload.

PARAMETERS: p_file TYPE rlgrap-filename LOWER CASE.

DATA: lv_filedata_string TYPE string.

OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
READ DATASET p_file INTO lv_filedata_string.
CLOSE DATASET p_file.

DATA: lv_filedata_xstring TYPE xstring.

CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
  EXPORTING
    text   = lv_filedata_string
  IMPORTING
    buffer = lv_filedata_xstring.

DATA: lv_tot_bytes  TYPE i,
      lv_chunk_size TYPE i,
      lv_start_byte TYPE i,
      lv_last_byte  TYPE i.
DATA:
  lv_data_chunk TYPE xstring.

TRY.

    DATA(lo_storage) = NEW zcl_storage_v1_resumable( iv_key_name = 'DEMO_STORAGE' ).

    lo_storage->add_common_qparam( iv_name = 'uploadType' iv_value = 'resumable' ).

    lo_storage->resumable_upload_metadata( EXPORTING iv_q_name  = 'example_file2.txt'
                                          iv_p_bucket     = 'abap_sdk_resumable_upload'
                                          iv_content_type = 'text/plain'
                                IMPORTING ev_ret_code     = DATA(lv_ret_code)
                                          ev_location     = DATA(lv_location) ).

    IF lv_ret_code <> 308.
      "Handle Error
      RETURN.
    ENDIF.

    lv_tot_bytes = xstrlen( lv_filedata_xstring ).
    lv_chunk_size = 8000000.
    lv_ret_code = 308.

    WHILE lv_start_byte < lv_tot_bytes AND lv_ret_code = 308.

      IF ( ( lv_tot_bytes - lv_start_byte ) < lv_chunk_size ).
        lv_chunk_size = lv_tot_bytes - lv_start_byte.
      ENDIF.

      lv_data_chunk = lv_filedata_xstring+lv_start_byte(lv_chunk_size).
      lv_last_byte = lv_start_byte + lv_chunk_size - 1.

      CLEAR lv_ret_code.
      lo_storage->resumable_upload_data(
        EXPORTING
          is_data         = lv_data_chunk
          iv_content_type = 'text/plain'
          iv_session_id   = lv_location
          iv_start_byte   = lv_start_byte
          iv_last_byte    = lv_last_byte
          iv_tot_bytes    = lv_tot_bytes
        IMPORTING
          ev_ret_code     = lv_ret_code
          ev_last_byte    = lv_last_byte ).

      lv_start_byte = lv_last_byte + 1.
    ENDWHILE.

    lo_storage->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.

ENDTRY.
