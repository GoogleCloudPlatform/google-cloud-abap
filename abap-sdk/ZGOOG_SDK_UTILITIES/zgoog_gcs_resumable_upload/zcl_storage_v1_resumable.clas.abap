class ZCL_STORAGE_V1_RESUMABLE definition
  public
  inheriting from /GOOG/CL_STORAGE_V1
  final
  create public .

public section.

  methods RESUMABLE_UPLOAD_METADATA
    importing
      !IV_Q_CONTENTENCODING type STRING optional
      !IV_Q_IFGENERATIONMATCH type STRING optional
      !IV_Q_IFGENERATIONNOTMATCH type STRING optional
      !IV_Q_IFMETAGENERATIONMATCH type STRING optional
      !IV_Q_IFMETAGENERATIONNOTMATCH type STRING optional
      !IV_Q_KMSKEYNAME type STRING optional
      !IV_Q_NAME type STRING optional
      !IV_Q_PREDEFINEDACL type STRING optional
      !IV_Q_PROJECTION type STRING optional
      !IV_Q_USERPROJECT type STRING optional
      !IV_P_BUCKET type STRING optional
      !IS_INPUT type TY_013 optional
      !IV_CONTENT_TYPE type STRING optional
    exporting
      !ES_RAW type DATA
      !ES_OUTPUT type TY_013
      !EV_RET_CODE type I
      !EV_ERR_TEXT type STRING
      !ES_ERR_RESP type /GOOG/ERR_RESP
      !EV_LOCATION type STRING
    raising
      /GOOG/CX_SDK .
  methods RESUMABLE_UPLOAD_DATA
    importing
      !IS_DATA type XSTRING
      !IV_CONTENT_TYPE type STRING
      !IV_SESSION_ID type STRING
      !IV_START_BYTE type I optional
      !IV_LAST_BYTE type I optional
      !IV_TOT_BYTES type I optional
    exporting
      !ES_RAW type DATA
      !ES_OUTPUT type TY_013
      !EV_RET_CODE type I
      !EV_ERR_TEXT type STRING
      !ES_ERR_RESP type /GOOG/ERR_RESP
      !EV_LAST_BYTE type I
    raising
      /GOOG/CX_SDK .
protected section.

  methods INSERT_OBJECTS_RESUMABLE
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_STORAGE_V1_RESUMABLE IMPLEMENTATION.


  METHOD resumable_upload_data.
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

    resume_upload(
      EXPORTING
        is_data         = is_data
        iv_content_type = iv_content_type
        iv_session_id   = iv_session_id
        iv_start_byte   = iv_start_byte
        iv_last_byte    = iv_last_byte
        iv_tot_bytes    = iv_tot_bytes
      IMPORTING
        es_raw          = es_raw
        es_output       = es_output
        ev_ret_code     = ev_ret_code
        ev_err_text     = ev_err_text
        es_err_resp     = es_err_resp
        ev_last_byte    = ev_last_byte ).

  ENDMETHOD.


  METHOD RESUMABLE_UPLOAD_METADATA.
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

    insert_objects(
     EXPORTING
       iv_q_contentencoding          = iv_q_contentencoding
       iv_q_ifgenerationmatch        = iv_q_ifgenerationmatch
       iv_q_ifgenerationnotmatch     = iv_q_ifgenerationnotmatch
       iv_q_ifmetagenerationmatch    = iv_q_ifmetagenerationmatch
       iv_q_ifmetagenerationnotmatch = iv_q_ifmetagenerationnotmatch
       iv_q_kmskeyname               = iv_q_kmskeyname
       iv_q_name                     = iv_q_name
       iv_q_predefinedacl            = iv_q_predefinedacl
       iv_q_projection               = iv_q_projection
       iv_q_userproject              = iv_q_userproject
       iv_p_bucket                   = iv_p_bucket
       is_input                      = is_input
       iv_content_type               = iv_content_type
     IMPORTING
       es_raw                        = es_raw
       es_output                     = es_output
       ev_ret_code                   = ev_ret_code
       ev_err_text                   = ev_err_text
       es_err_resp                   = es_err_resp ).

    ev_location = go_http->response->get_header_field( 'location' ).

  ENDMETHOD.


METHOD insert_objects_resumable.
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

  DATA: lv_json_final TYPE string,
        lv_response   TYPE string,
        lv_session_id TYPE string.
  DATA: lv_tot_bytes  TYPE i,
        lv_rem_bytes  TYPE i,
        lv_chunk_size TYPE i,
        lv_start_byte TYPE i,
        lv_last_byte  TYPE i,
        lv_ret_code   TYPE i.
  DATA: ls_output LIKE es_output,
        ls_data   TYPE xstring,
        lv_mid    TYPE string.

  CONCATENATE c_service_name
  '#'
  'storage.objects.insert'
  INTO lv_mid.

  IF is_input IS SUPPLIED.
    lv_json_final = /goog/cl_json_util=>serialize_json(
      is_data      = is_input
      iv_method_id = lv_mid
    ).
  ENDIF.

  go_http->request->set_header_field( name = 'X-Upload-Content-Type' value = iv_content_type ).

  CALL METHOD make_request
    EXPORTING
      iv_uri      = iv_uri
      iv_body     = lv_json_final
      iv_method   = c_method_post
    IMPORTING
      es_response = lv_response
      ev_ret_code = ev_ret_code
      ev_err_text = ev_err_text
      ev_err_resp = es_err_resp.

  IF ev_ret_code >= 200 AND ev_ret_code <= 299 .

    lv_session_id = go_http->response->get_header_field( 'location' ).

    IF iv_chunk_size IS INITIAL AND is_data IS NOT INITIAL.
      CALL METHOD me->resume_upload
        EXPORTING
          is_data         = is_data
          iv_content_type = iv_content_type
          iv_session_id   = lv_session_id
        IMPORTING
          es_raw          = es_raw
          es_output       = es_output
          ev_ret_code     = ev_ret_code
          ev_err_text     = ev_err_text
          es_err_resp     = es_err_resp.
    ELSE.

      lv_tot_bytes = xstrlen( is_data ).
      lv_rem_bytes = lv_tot_bytes.
      lv_chunk_size = iv_chunk_size.
      lv_ret_code = 308.

      WHILE lv_start_byte < lv_tot_bytes AND lv_ret_code = 308.
*
        IF ( ( lv_tot_bytes - lv_start_byte ) < lv_chunk_size ).
          lv_chunk_size = lv_tot_bytes - lv_start_byte.
        ENDIF.

        ls_data = is_data+lv_start_byte(lv_chunk_size).
        lv_last_byte = lv_start_byte + lv_chunk_size - 1.

        CLEAR lv_ret_code.
        CALL METHOD me->resume_upload
          EXPORTING
            is_data         = ls_data
            iv_content_type = iv_content_type
            iv_session_id   = lv_session_id
            iv_start_byte   = lv_start_byte
            iv_last_byte    = lv_last_byte
            iv_tot_bytes    = lv_tot_bytes
          IMPORTING
            es_raw          = es_raw
            es_output       = es_output
            ev_ret_code     = lv_ret_code
            ev_err_text     = ev_err_text
            ev_last_byte    = lv_last_byte
            es_err_resp     = es_err_resp.

        lv_start_byte = lv_last_byte + 1.
      ENDWHILE.
      ev_ret_code = lv_ret_code.
    ENDIF.

  ELSE.
    es_raw = lv_response.
    /goog/cl_json_util=>deserialize_json( EXPORTING iv_json          = lv_response
                                                    iv_pretty_name   = /ui2/cl_json=>pretty_mode-extended
                                                    iv_method_id     = lv_mid
                                                    it_name_mappings = gt_name_mappings
                                          IMPORTING es_data          = ls_output ).
    es_output = ls_output .

  ENDIF.
ENDMETHOD.
ENDCLASS.
