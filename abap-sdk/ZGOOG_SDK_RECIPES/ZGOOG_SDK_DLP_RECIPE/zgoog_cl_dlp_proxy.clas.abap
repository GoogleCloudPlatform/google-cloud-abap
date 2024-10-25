" --------------------------------------------------------------------
"  Copyright 2024 Google LLC                                         -
"                                                                    -
"  Licensed under the Apache License, Version 2.0 (the "License");   -
"  you may not use this file except in compliance with the License.  -
"  You may obtain a copy of the License at                           -
"      https://www.apache.org/licenses/LICENSE-2.0                   -
"  Unless required by applicable law or agreed to in writing,        -
"  software distributed under the License is distributed on an       -
"  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      -
"  either express or implied.                                        -
"  See the License for the specific language governing permissions   -
"  and limitations under the License.                                -
" --------------------------------------------------------------------
CLASS zgoog_cl_dlp_proxy DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA mt_dlp_config TYPE STANDARD TABLE OF zgoog_dlp_config.

    CLASS-METHODS call_dlp
      IMPORTING iv_input_value  TYPE string
                iv_input_type   TYPE string
      EXPORTING ev_output_value TYPE string
                ev_message      TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS fetch_config.
ENDCLASS.


CLASS zgoog_cl_dlp_proxy IMPLEMENTATION.
  METHOD call_dlp.
    DATA ls_input              TYPE /goog/cl_dlp_v2=>ty_055.
    DATA ls_transformations    TYPE /goog/cl_dlp_v2=>ty_100.
    DATA ls_kms_wrapped_key    TYPE /goog/cl_dlp_v2=>ty_123.
    DATA ls_crypto_key         TYPE /goog/cl_dlp_v2=>ty_040.
    DATA ls_crypto_hash_config TYPE /goog/cl_dlp_v2=>ty_039.
    DATA lo_redact             TYPE REF TO data.

    TRY.
        DATA(lo_client) = NEW /goog/cl_dlp_v2( iv_key_name = 'TEST_ABAP_SDK' ).
        DATA(lv_p_projects_id) = CONV string( lo_client->gv_project_id ).

        fetch_config( ).
        " Read the configuration
        TRY.
            DATA(ls_dlp_config) = mt_dlp_config[ keyword = iv_input_type ].
          CATCH cx_sy_itab_line_not_found INTO DATA(lx_not_found).
            ev_output_value = iv_input_value.
        ENDTRY.

        CASE iv_input_type.

          WHEN 'EMAIL'.

            " Populate the input parameters to DLP API for replacement
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_input-inspect_config-info_types.
            ls_transformations-primitive_transformation-replace_config-new_value-string_value = 'EMAIL_ID@EXAMPLE.COM'.
            INSERT ls_transformations INTO TABLE ls_input-deidentify_config-info_type_transformations-transformations.
            ls_input-item-value = iv_input_value.
            " Call DLP API client stub
            TRY.
                lo_client->deidentify_content( EXPORTING iv_p_projects_id = lv_p_projects_id
                                                         is_input         = ls_input
                                               IMPORTING es_output        = DATA(ls_output)
                                                         ev_ret_code      = DATA(lv_ret_code)
                                                         ev_err_text      = DATA(lv_err_text) ).
              CATCH /goog/cx_sdk INTO DATA(lx_sdk_exception).
                ev_message = lx_sdk_exception->get_text( ).
            ENDTRY.

            IF lo_client->is_success( lv_ret_code ).
              ev_message = lv_err_text.
            ELSE.
              ev_output_value = ls_output-item-value.
            ENDIF.

          WHEN 'PHONE NUMBER'.

            CLEAR : ls_input,
                    ls_transformations,
                    ls_dlp_config.
            " Populate the input parameters to DLP API for masking
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_input-inspect_config-info_types.
            ls_transformations-primitive_transformation-character_mask_config-number_to_mask    = ls_dlp_config-number_to_mask.
            ls_transformations-primitive_transformation-character_mask_config-masking_character = ls_dlp_config-masking_char.
            INSERT ls_transformations INTO TABLE ls_input-deidentify_config-info_type_transformations-transformations.
            ls_input-item-value = iv_input_value.
            " Call DLP API client stub
            TRY.
                lo_client->deidentify_content( EXPORTING iv_p_projects_id = lv_p_projects_id
                                                         is_input         = ls_input
                                               IMPORTING es_output        = ls_output
                                                         ev_ret_code      = lv_ret_code
                                                         ev_err_text      = lv_err_text ).
              CATCH /goog/cx_sdk INTO lx_sdk_exception.
                ev_message = lx_sdk_exception->get_text( ).
            ENDTRY.

            IF lo_client->is_success( lv_ret_code ).
              ev_message = lv_err_text.
            ELSE.
              ev_output_value = ls_output-item-value.
            ENDIF.

          WHEN 'BANK ACCOUNT'.

            CLEAR : ls_input,
                    ls_transformations.

            ls_kms_wrapped_key-crypto_key_name = 'projects/gcpsaptesting/locations/global/keyRings/dlp-keyring/cryptoKeys/dlp-key'.
            ls_kms_wrapped_key-wrapped_key     = 'CiQA85aLgyexTTZ419oR/CtF2E8YLlW3ihcDsOlfI4Jq0oZ5T6ASSQBKCQd7GSBN/iLa4IoOBOUfSPtm8uTdE/ZH9yn9WpVA+bg0YqjxbeIXQDMJxWM8IC3iQL46UogFj6yMN9YYKk0Dlh2JgZDHCOw='.

            ls_crypto_key-kms_wrapped = ls_kms_wrapped_key.
            ls_crypto_hash_config-crypto_key = ls_crypto_key.
            " Populate the input parameters to DLP API for cryptographic encryption
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_input-inspect_config-info_types.
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_transformations-info_types.
            ls_transformations-primitive_transformation-crypto_replace_ffx_fpe_config-crypto_key-kms_wrapped = ls_kms_wrapped_key.
            ls_transformations-primitive_transformation-crypto_replace_ffx_fpe_config-surrogate_info_type-name = ls_dlp_config-surrogate_infotype.
            ls_transformations-primitive_transformation-crypto_replace_ffx_fpe_config-common_alphabet = ls_dlp_config-common_alphabet.
            INSERT ls_transformations INTO TABLE ls_input-deidentify_config-info_type_transformations-transformations.
            ls_input-item-value = iv_input_value.
            " Add the info type identification string to map the subsequent value to relevant infotype
            CONCATENATE 'Bank Account' ls_input-item-value INTO ls_input-item-value SEPARATED BY space.
            " Call DLP API client stub
            TRY.
                lo_client->deidentify_content( EXPORTING iv_p_projects_id = lv_p_projects_id
                                                         is_input         = ls_input
                                               IMPORTING es_output        = ls_output
                                                         ev_ret_code      = lv_ret_code
                                                         ev_err_text      = lv_err_text ).
              CATCH /goog/cx_sdk INTO lx_sdk_exception.
                ev_message = lx_sdk_exception->get_text( ).
            ENDTRY.

            IF lo_client->is_success( lv_ret_code ).
              ev_message = lv_err_text.
            ELSE.
              " Removing the info type identification string added earlier and keeping only the encrypted value
              REPLACE ALL OCCURRENCES OF SUBSTRING 'Bank Account' IN ls_output-item-value WITH ''.
              REPLACE ALL OCCURRENCES OF SUBSTRING 'ACCOUNT(10):' IN ls_output-item-value WITH ''.
              ev_output_value = ls_output-item-value.
            ENDIF.

          WHEN 'REMARKS'.

            CLEAR : ls_input,
                    ls_transformations.
            " Populate the input parameters to DLP API for redaction
            CREATE DATA lo_redact TYPE REF TO string.
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_input-inspect_config-info_types.
            INSERT VALUE #( name = ls_dlp_config-infotype ) INTO TABLE ls_transformations-info_types.
            ls_transformations-primitive_transformation-redact_config = lo_redact.
            INSERT ls_transformations INTO TABLE ls_input-deidentify_config-info_type_transformations-transformations.
            ls_input-item-value = iv_input_value.
            " Call DLP API client stub
            TRY.
                lo_client->deidentify_content( EXPORTING iv_p_projects_id = lv_p_projects_id
                                                         is_input         = ls_input
                                               IMPORTING es_output        = ls_output
                                                         ev_ret_code      = lv_ret_code
                                                         ev_err_text      = lv_err_text ).
              CATCH /goog/cx_sdk INTO lx_sdk_exception.
                ev_message = lx_sdk_exception->get_text( ).
            ENDTRY.

            IF lo_client->is_success( lv_ret_code ).
              ev_message = lv_err_text.
            ELSE.
              ev_output_value = ls_output-item-value.
            ENDIF.

          WHEN OTHERS.
            ev_output_value = iv_input_value.
        ENDCASE.

        lo_client->close_http_client( ).

      CATCH /goog/cx_sdk INTO DATA(lx_sdk).
        ev_message = lx_sdk->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD fetch_config.
  ENDMETHOD.
ENDCLASS.
