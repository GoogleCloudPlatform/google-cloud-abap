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

REPORT zr_qs_translate_texts.

* data declarations
      data: lv_text         type string,
            lv_msg          type string,
            lv_ret_code     type i,
            lv_err_text     type string,
            ls_err_resp     type /goog/err_resp,
            ls_input        type /goog/cl_translation_v2=>ty_006,
            ls_output       type /goog/cl_translation_v2=>ty_007,
            lt_translations type /goog/cl_translation_v2=>ty_translations,
            ls_texts        type /goog/cl_translation_v2=>ty_008,
            lo_translate    type ref to /goog/cl_translation_v2,
            lo_exception    type ref to /goog/cx_sdk.

TRY.
* instantiate api client stub
          create object lo_translate
            exporting
              iv_key_name = 'DEMO_TRANSLATE'.

* pass the text to be translated to the required parameter
          lv_text = 'The Earth is the third planet from the Sun'.
    APPEND lv_text TO ls_input-q.

    ls_input-format = 'text'.
    ls_input-source = 'en'.
    ls_input-target = 'de'.

* call the api method to translate text
          call method lo_translate->translate_translations
            exporting
              is_input    = ls_input
            importing
              es_output   = ls_output
              ev_ret_code = lv_ret_code
              ev_err_text = lv_err_text
              es_err_resp = ls_err_resp.
    IF lo_translate->is_success( lv_ret_code ) = abap_true.
      lt_translations = ls_output-data.
      READ TABLE lt_translations-translations INTO ls_texts INDEX 1.
      WRITE: / 'Translation Successful'.
      WRITE: / 'Translated Text is: ', ls_texts-translated_text.
    ENDIF.

* close the http connection
          lo_translate->close( ).

  CATCH /goog/cx_sdk INTO lo_exception.
* write code here to handle exceptions
          endtry.
