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
REPORT zr_util_create_bq_table.


TABLES: /goog/bq_mastr, /goog/bq_table.

TYPES:
  BEGIN OF t_repl_table,
     mass_tr_id type /GOOG/MT_ID,
     tabname_s4 type /GOOG/TABNM_S4,
  END OF t_repl_table.


SELECT-OPTIONS: s_trkey FOR /goog/bq_mastr-transf_key MATCHCODE OBJECT /goog/sh_transf_key,
                s_table FOR /goog/bq_table-tabname_s4  MATCHCODE OBJECT /goog/sh_tabname_s4.


DATA: lt_tables TYPE STANDARD TABLE OF t_repl_table WITH DEFAULT KEY.

SELECT a~mass_tr_id
       b~tabname_s4
  FROM /goog/bq_mastr AS a INNER JOIN
       /goog/bq_table AS b
  ON a~transf_key = b~transf_key
  INTO TABLE lt_tables
  WHERE a~transf_key IN s_trkey[]
   AND b~tabname_s4 IN s_table[].

DATA: lo_badi TYPE REF TO /goog/cl_iuuc_repl_runtime_bq.

DATA: ls_table TYPE t_repl_table.

LOOP AT lt_tables INTO ls_table.

  CREATE OBJECT lo_badi.

  CALL METHOD lo_badi->create_settings_object
    EXPORTING
      iv_mass_tr_id = ls_table-mass_tr_id
      iv_tabname_s4 = ls_table-tabname_s4.

  CALL METHOD lo_badi->create_target_table( abap_true ).

  CALL METHOD lo_badi->write_table_data( ).

ENDLOOP.
