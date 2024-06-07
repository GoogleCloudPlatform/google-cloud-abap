class ZCL_BQ_TABLE_READ_ODATA_GEN definition
  public
  final
  create public .

public section.

  types T_RESULT type ref to IF_XCO_GEN_O_PUT_RESULT .
  types:
    tt_result      TYPE STANDARD TABLE OF t_result WITH EMPTY KEY .

  constants:
    BEGIN OF c_srvb_bind_type,
        odata_v2_ui      TYPE srvb_bnd_type VALUE 'OData V2 - UI', "#EC NOTEXT
        odata_v2_web_api TYPE srvb_bnd_type VALUE 'OData V2 - Web API', "#EC NOTEXT
        odata_v4_ui      TYPE srvb_bnd_type VALUE 'OData V4 - UI', "#EC NOTEXT
        odata_v4_web_api TYPE srvb_bnd_type VALUE 'OData V4 - Web API', "#EC NOTEXT
      END OF c_srvb_bind_type .

  class-methods MAIN
    importing
      !P_KEY type /GOOG/CLIENT_KEY-NAME
      !P_PRJ type /GOOG/CLIENT_KEY-PROJECT_ID
      !P_DSET type STRING
      !P_TABLE type STRING
      !P_DEVC type SXCO_PACKAGE
      !P_TR type SXCO_TRANSPORT
      !P_VIEW type SXCO_AO_OBJECT_NAME
      !P_STRG type FLAG
      !P_KEYS type I
      !P_QPCLS type SXCO_AO_OBJECT_NAME
      !P_SRVD type SXCO_SRVD_OBJECT_NAME
      !P_SRVB type SXCO_SRVB_OBJECT_NAME
      !P_BTYP type SRVB_BND_TYPE
    returning
      value(RT_RESULT) type TT_RESULT
    raising
      /GOOG/CX_SDK .
private section.

  class-data MO_ENVIRONMENT type ref to IF_XCO_CP_GEN_ENV_DEV_SYSTEM .
  class-data MO_PUT_OPERATION type ref to IF_XCO_CP_GEN_D_O_PUT .

  class-methods GET_DATA_TYPE
    importing
      !BQ_TYPE type STRING
    returning
      value(RV_SAP_TYPE) type ref to IF_XCO_GEN_DDLS_FIELD_TYPE .
  class-methods PUT_DDLS
    importing
      !P_DEVC type SXCO_PACKAGE
      !P_QPCLS type SXCO_AO_OBJECT_NAME
      !P_VIEW type SXCO_AO_OBJECT_NAME
      !P_STRG type FLAG
      !P_KEYS type I
      !IS_BQ_TABLE type /GOOG/CL_BIGQUERY_V2=>TY_131 .
  class-methods PUT_SRVD
    importing
      !P_VIEW type SXCO_AO_OBJECT_NAME
      !P_DEVC type SXCO_PACKAGE
      !P_SRVD type SXCO_SRVD_OBJECT_NAME .
  class-methods ADD_SRVB
    importing
      !P_DEVC type SXCO_PACKAGE
      !P_BTYP type SRVB_BND_TYPE
      !P_SRVD type SXCO_SRVD_OBJECT_NAME
      !P_SRVB type SXCO_SRVB_OBJECT_NAME .
  class-methods GET_BINDING_TYPE
    importing
      !P_BTYP type SRVB_BND_TYPE
    returning
      value(RV_BTYP) type ref to CL_XCO_SRVB_BINDING_TYPE .
  class-methods PUT_CLAS
    importing
      !P_DEVC type SXCO_PACKAGE
      !IS_BQ_TABLE type /GOOG/CL_BIGQUERY_V2=>TY_131
      !P_KEY type /GOOG/CLIENT_KEY-NAME
      !P_DSET type STRING
      !P_QPCLS type SXCO_AO_OBJECT_NAME .
ENDCLASS.



CLASS ZCL_BQ_TABLE_READ_ODATA_GEN IMPLEMENTATION.


  METHOD ADD_SRVB.
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

    DATA(lo_specification_header) = mo_put_operation->for-srvb->add_object( p_srvb
      )->set_package( p_devc
      )->create_form_specification( ).

    lo_specification_header->set_short_description( 'Service binding for ' && p_srvd  ).

    lo_specification_header->set_binding_type( get_binding_type( p_btyp ) ).

    lo_specification_header->add_service( CONV #( p_srvd )  )->add_version( '0001' )->set_service_definition( p_srvd  ).

  ENDMETHOD.


  METHOD GET_BINDING_TYPE.
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

    CASE p_btyp.
      WHEN c_srvb_bind_type-odata_v2_ui.
        rv_btyp = xco_cp_service_binding=>binding_type->odata_v2_ui.
      WHEN c_srvb_bind_type-odata_v2_web_api.
        rv_btyp = xco_cp_service_binding=>binding_type->odata_v2_web_api.
      WHEN c_srvb_bind_type-odata_v4_ui.
        rv_btyp = xco_cp_service_binding=>binding_type->odata_v4_ui.
      WHEN c_srvb_bind_type-odata_v4_web_api.
        rv_btyp = xco_cp_service_binding=>binding_type->odata_v4_web_api.
      WHEN OTHERS.
        rv_btyp = xco_cp_service_binding=>binding_type->odata_v4_ui.
    ENDCASE.

  ENDMETHOD.


  METHOD get_data_type.
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

    CASE bq_type.
      WHEN 'STRING'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->string( 0 ).
      WHEN 'FLOAT'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->fltp.
      WHEN 'BYTES'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->rawstring( 0 ).
      WHEN 'INTEGER'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->int8.
      WHEN 'NUMERIC'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->dec(
                                         iv_length = 31
                                         iv_decimals = 14  ).
      WHEN 'DATE'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->dats.
      WHEN 'TIME'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->tims.
      WHEN 'BOOLEAN'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->char( 1 ).
      WHEN 'TIMESTAMP'.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->utclong.
      WHEN OTHERS.
        rv_sap_type = xco_cp_abap_dictionary=>built_in_type->string( 0 ).
    ENDCASE.

  ENDMETHOD.


  METHOD main.
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

    mo_environment = xco_cp_generation=>environment->dev_system( p_tr ).
    mo_put_operation = mo_environment->create_put_operation( ).

    DATA(lo_bq) = NEW /goog/cl_bigquery_v2( iv_key_name = p_key ).

    lo_bq->get_tables(
      EXPORTING
        iv_p_datasets_id     = p_dset
        iv_p_projects_id     = CONV #( p_prj )
        iv_p_tables_id       = p_table
      IMPORTING
        es_output           = DATA(ls_table_get) ).

    REPLACE ALL OCCURRENCES OF ':' IN ls_table_get-id WITH '.'.

    put_ddls( p_devc = p_devc
              p_view = p_view
              p_qpcls = p_qpcls
              p_keys = p_keys
              p_strg = p_strg
              is_bq_table = ls_table_get ).

    put_clas( p_devc = p_devc
              is_bq_table = ls_table_get
              p_key = p_key
              p_dset = p_dset
              p_qpcls = p_qpcls ).

    put_srvd( p_devc = p_devc
              p_view = p_view
              p_srvd = p_srvd ).

    APPEND mo_put_operation->execute( ) TO rt_result .

    mo_environment = xco_cp_generation=>environment->dev_system( p_tr ).
    mo_put_operation = mo_environment->create_put_operation( ).

    add_srvb( p_btyp = p_btyp
              p_devc = p_devc
              p_srvd = p_srvd
              p_srvb = p_srvb ).

    APPEND mo_put_operation->execute( ) TO rt_result .

  ENDMETHOD.


  METHOD PUT_CLAS.
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

    DATA(lo_specification) = mo_put_operation->for-clas->add_object( p_qpcls
      )->set_package( p_devc
      )->create_form_specification( ).
    lo_specification->set_short_description( |Query Provider Class for { is_bq_table-id }| ).
    lo_specification->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).
    lo_specification->definition->add_interface( 'IF_RAP_QUERY_PROVIDER' ).
    lo_specification->definition->set_superclass( 'ZCL_BQ_TABLE_READ_BASE' ).

    lo_specification->implementation->add_method( |IF_RAP_QUERY_PROVIDER~SELECT|
        )->set_source( VALUE #(
            ( | select(  | )
            ( |   iv_key      = '{ p_key }' | )
            ( |   iv_table    = '{ is_bq_table-id }' | )
            ( |   iv_dataset  = '{ p_dset }' | )
            ( |   io_request  = io_request | )
            ( |   io_response = io_response ).| )
             ) ).

  ENDMETHOD.


  METHOD PUT_DDLS.
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

    DATA(lo_form_specification) = mo_put_operation->for-ddls->add_object( p_view
      )->set_package( p_devc
      )->create_form_specification( ).

    DATA(lo_custom_entity) = lo_form_specification->set_short_description( |CDS View for BQ Table { is_bq_table-id }|
      )->add_custom_entity( ).

    lo_custom_entity->add_annotation( 'ObjectModel.query.implementedBy' )->value->build( )->add_string( |ABAP:{ p_qpcls }| ).
    lo_custom_entity->add_annotation( 'EndUserText.label' )->value->build( )->add_string( is_bq_table-id ).

    LOOP AT is_bq_table-schema-fields REFERENCE INTO DATA(ls_field).
      DATA(lv_field_tabix) = sy-tabix.
      DATA(lo_id_field) = lo_custom_entity->add_field( xco_cp_ddl=>field( CONV #( ls_field->name ) ) ).
      IF lv_field_tabix <= p_keys.
        lo_id_field->set_key( ).
      ENDIF.
      lo_id_field->set_type(
         COND #( WHEN p_strg IS INITIAL
                  THEN get_data_type( ls_field->type )
                 ELSE xco_cp_abap_dictionary=>built_in_type->string( 0 ) ) ).
      lo_id_field->add_annotation( 'EndUserText.label' )->value->build( )->add_string( ls_field->description ).
      lo_id_field->add_annotation( 'UI.lineItem' )->value->build(
        )->begin_array(
          )->begin_record(
            )->add_member( 'label' )->add_string( ls_field->description
          )->end_record(
        )->end_array( ).

    ENDLOOP.

  ENDMETHOD.


  METHOD PUT_SRVD.
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

    DATA(lo_specification_header) = mo_put_operation->for-srvd->add_object( p_srvd
      )->set_package( p_devc
      )->create_form_specification( ).

    lo_specification_header->set_short_description( 'Service definition for ' && p_view ).

    lo_specification_header->add_annotation( 'EndUserText.label' )->value->build( )->add_string( 'Service definition for ' && p_view ) ##NO_TEXT.
    "add exposure for root node
    lo_specification_header->add_exposure( p_view ).

  ENDMETHOD.
ENDCLASS.
