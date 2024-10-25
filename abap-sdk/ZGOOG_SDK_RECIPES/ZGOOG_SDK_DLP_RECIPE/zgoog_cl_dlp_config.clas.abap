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
CLASS zgoog_cl_dlp_config DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.


CLASS zgoog_cl_dlp_config IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA lt_dlp_config TYPE TABLE OF zgoog_dlp_config.

    " fill internal table (itab)
    lt_dlp_config = VALUE #(
        ( client = sy-mandt keyword  = 'EMAIL'        infotype = 'EMAIL_ADDRESS'  )
        ( client = sy-mandt keyword  = 'PHONE NUMBER' infotype = 'PHONE_NUMBER' number_to_mask = 5 masking_char = '*' )
        ( client = sy-mandt keyword  = 'REMARKS'      infotype = 'EMAIL_ADDRESS'  )
        ( client = sy-mandt keyword  = 'REMARKS'      infotype = 'PHONE_NUMBER'  )
        ( client = sy-mandt keyword  = 'BANK ACCOUNT' infotype = 'FINANCIAL_ACCOUNT_NUMBER' surrogate_infotype = 'ACCOUNT' common_alphabet = 'ALPHA_NUMERIC' ) ).

    " insert the new table entries and print the result
    MODIFY zgoog_dlp_config FROM TABLE @lt_dlp_config.
    IF sy-subrc = 0.
      out->write( sy-dbcnt ).
      out->write( 'records inserted successfully!' ).
    ELSE.
      out->write( 'Problem during data modification' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
