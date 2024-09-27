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
REPORT zr_vector_search_recipe.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_rb1 TYPE char1 RADIOBUTTON GROUP rbg1 DEFAULT 'X' USER-COMMAND stt,
    p_rb2 TYPE char1 RADIOBUTTON GROUP rbg1,
    p_rb3 TYPE char1 RADIOBUTTON GROUP rbg1,
    p_rb4 TYPE char1 RADIOBUTTON GROUP rbg1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_sh_key TYPE /goog/search_key MATCHCODE OBJECT /goog/sh_search_key OBLIGATORY,
    p_sh_str TYPE string MODIF ID l01 LOWER CASE,
    p_embkey TYPE /goog/model_key LOWER CASE MATCHCODE OBJECT /goog/sh_model_key MODIF ID l02,
    p_dim    TYPE int2 DEFAULT 768 MODIF ID l02,
    p_nb_cnt TYPE int4 DEFAULT 5 MODIF ID l03,
    p_im_gcs TYPE string LOWER CASE MODIF ID l04,
    p_entity TYPE string LOWER CASE MODIF ID l05.

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_rb1 IS NOT INITIAL.
      IF screen-group1 = 'L04' OR
         screen-group1 = 'L05'.
        screen-input     = 0.
        screen-invisible = 1.

      ENDIF.
    ELSEIF p_rb2 IS NOT INITIAL.
      IF screen-group1 = 'L04' OR
         screen-group1 = 'L05'.
        screen-input     = 0.
        screen-invisible = 1.

      ENDIF.
    ELSEIF p_rb3 IS NOT INITIAL.
      IF screen-group1 = 'L01' OR
         screen-group1 = 'L05'.
        screen-input     = 0.
        screen-invisible = 1.

      ENDIF.
    ELSEIF p_rb4 IS NOT INITIAL.
      IF screen-group1 = 'L01' OR
         screen-group1 = 'L02' OR
         screen-group1 = 'L03' OR
         screen-group1 = 'L04'.
        screen-input     = 0.
        screen-invisible = 1.

      ENDIF.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS: execute_search.

  PRIVATE SECTION.
    CLASS-METHODS:
      search_data_by_string,
      search_images_by_string,
      search_images_by_image,
      search_by_entity_id.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>execute_search( ).

CLASS lcl_main IMPLEMENTATION.

  METHOD execute_search.

    CASE abap_true.
      WHEN p_rb1.
        search_data_by_string( ).
      WHEN p_rb2.
        search_images_by_string( ).
      WHEN p_rb3.
        search_images_by_image( ).
      WHEN p_rb4.
        search_by_entity_id( ).
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD search_data_by_string.

    DATA:
          lv_search_string  TYPE string.

    lv_search_string = p_sh_str.

    TRY.
        DATA(lo_vector_search) = NEW /goog/cl_vector_search( iv_search_key  = p_sh_key ).
        DATA(lt_vector_search_response) = lo_vector_search->find_neighbors_by_string( iv_search_string         = lv_search_string
                                                                                      iv_embeddings_dimensions = p_dim
                                                                                      iv_embeddings_model_key  = p_embkey
                                                                                      iv_neighbor_count        = p_nb_cnt
                                                        )->get_nearest_neighbors( ).
        cl_demo_output=>display( lt_vector_search_response ).
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        cl_demo_output=>display( 'Search not successful.' && lo_cx_sdk->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD search_images_by_string.

    DATA:
          lv_search_string  TYPE string.

    lv_search_string = p_sh_str.

    TRY.
        DATA(lo_vector_search) = NEW /goog/cl_vector_search( iv_search_key  = p_sh_key ).
        DATA(lt_vector_search_response) = lo_vector_search->find_neighbors_by_string( iv_search_string         = lv_search_string
                                                                                      iv_embeddings_dimensions = p_dim
                                                                                      iv_embeddings_model_key  = p_embkey
                                                                                      iv_neighbor_count        = p_nb_cnt
                                                        )->get_nearest_neighbors( ).
        cl_demo_output=>display( lt_vector_search_response ).
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        cl_demo_output=>display( 'Search not successful.' && lo_cx_sdk->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD search_images_by_image.

    DATA:
      lv_search_string TYPE string,
      ls_image         TYPE /goog/cl_embeddings_model=>ty_image.

    TRY.
        DATA(lo_vector_search) = NEW /goog/cl_vector_search( iv_search_key  = p_sh_key ).
        DATA(lo_embeddings_model) = NEW /goog/cl_embeddings_model( iv_model_key  = p_embkey ).

        ls_image-gcs_uri = p_im_gcs.
        DATA(lt_embeddings) = lo_embeddings_model->gen_image_embeddings( iv_image     = ls_image
                                                                         iv_dimension = p_dim
                                                )->get_vector( ).
        DATA(lt_vector_search_response) = lo_vector_search->find_neighbors_by_embedding( iv_neighbor_count = p_nb_cnt
                                                                                         it_embeddings     = lt_embeddings
                                                         )->get_nearest_neighbors( ).
        cl_demo_output=>display( lt_vector_search_response ).
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        cl_demo_output=>display( 'Search not successful.' && lo_cx_sdk->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD search_by_entity_id.

    TRY.
        DATA(lo_vector_search) = NEW /goog/cl_vector_search( iv_search_key  = p_sh_key ).
        DATA(lt_vector_search_response) = lo_vector_search->find_neighbors_by_entity_id( iv_entity_id = p_entity
                                                         )->get_nearest_neighbor( ).
        cl_demo_output=>display( lt_vector_search_response ).
      CATCH /goog/cx_sdk INTO DATA(lo_cx_sdk).
        cl_demo_output=>display( 'Search not successful.' && lo_cx_sdk->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
