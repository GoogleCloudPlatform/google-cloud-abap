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

REPORT zr_upload_object_rescskey.

* Data Declarations
DATA:
  lv_p_bucket     TYPE string,
  lv_q_name       TYPE string,
  ls_data         TYPE xstring,
  lv_content_type TYPE string.

TRY.
*  Open HTTP Connection
    DATA(lo_client) = NEW /goog/cl_storage_v1( iv_key_name = 'CLIENT_KEY' ).

*   Populate the data that needs to be passed to the api
    "Name of the bucket
    lv_p_bucket       = 'sample-bucket'.
    "Name of the object
    lv_q_name         = 'sample-object'.
    "Read object data into ls_data
    ls_data          = '<Object data in XSTRING form>'.
    "Content type of the object
    lv_content_type  = '<Content Type>'.

    "Set the common query parameter uploadType as 'resumable'
    "Default chunk size is 8MB
    CALL METHOD lo_client->add_common_qparam
      EXPORTING
        iv_name  = 'uploadType'
        iv_value = 'resumable'.

*   Set custom headers for Customer Supplied Encryption keys
    CALL METHOD lo_client->add_custom_header
      EXPORTING
        iv_name  = 'X-GOOG-ENCRYPTION-ALGORITHM'
        iv_value = 'AES256'. "Supported encryption algorithm

    "Base64-encoded string of your AES-256 encryption key
    CALL METHOD lo_client->add_custom_header
      EXPORTING
        iv_name  = 'X-Goog-Encryption-Key'
        iv_value = '<Encryption Key>'.

    "Base64-encoded string of the SHA256 hash of your encryption key
    CALL METHOD lo_client->add_custom_header
      EXPORTING
        iv_name  = 'X-Goog-Encryption-Key-Sha256'
        iv_value = '<SHA256 of the Encryption Key>'.

* Call API method
    CALL METHOD lo_client->insert_objects
      EXPORTING
        iv_q_name       = lv_q_name
        iv_p_bucket     = lv_p_bucket
        is_data         = ls_data
        iv_content_type = lv_content_type
      IMPORTING
        es_output       = DATA(ls_output)
        ev_ret_code     = DATA(lv_ret_code)
        ev_err_text     = DATA(lv_err_text)
        es_err_resp     = DATA(ls_err_resp).



    IF lo_client->is_success( lv_ret_code ).
      MESSAGE 'Bucket created successfully!' TYPE 'S'.
    ELSE.
      MESSAGE lv_err_text TYPE 'E'.
    ENDIF.

*   Close HTTP connection
    lo_client->close( ).

  CATCH /goog/cx_sdk INTO DATA(lo_exception).
    MESSAGE lo_exception->get_text( ) TYPE 'E'.
ENDTRY.
