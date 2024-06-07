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
REPORT zr_bq_table_read_odata_gen.

PARAMETERS: p_key   TYPE /goog/client_key-name OBLIGATORY,
            p_prj   TYPE /goog/client_key-project_id OBLIGATORY,
            p_dset  TYPE string LOWER CASE OBLIGATORY,
            p_table TYPE string LOWER CASE OBLIGATORY,
            p_devc  TYPE sxco_package OBLIGATORY,
            p_tr    TYPE sxco_transport OBLIGATORY,
            p_view  TYPE sxco_ao_object_name OBLIGATORY,
            p_strg  AS CHECKBOX,
            p_keys  TYPE i OBLIGATORY,
            p_qpcls TYPE sxco_ao_object_name OBLIGATORY,
            p_srvd  TYPE sxco_srvd_object_name OBLIGATORY,
            p_srvb  TYPE sxco_srvb_object_name OBLIGATORY,
            p_btyp  TYPE srvb_bnd_type LOWER CASE OBLIGATORY.


START-OF-SELECTION.

  TRY.

      DATA(lt_result) =
        zcl_bq_table_read_odata_gen=>main(
            p_key = p_key
            p_prj = p_prj
            p_dset = p_dset
            p_table = p_table
            p_devc = p_devc
            p_tr = p_tr
            p_view = p_view
            p_keys = p_keys
            p_strg = p_strg
            p_qpcls = p_qpcls
            p_srvd = p_srvd
            p_srvb = p_srvb
            p_btyp = p_btyp ).

    CATCH cx_root INTO DATA(lo_error).
      WRITE: / lo_error->get_text( ).
  ENDTRY.

  WRITE: / 'Findings'.
  LOOP AT lt_result INTO DATA(ls_result).
    LOOP AT ls_result->findings->if_xco_news~get_messages( ) INTO DATA(ls_message).
      WRITE: / ls_message->get_text( ).
    ENDLOOP.
  ENDLOOP.
