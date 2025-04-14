CLASS zgoog_cl_rap_evthdlr_generator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.



    METHODS: create_class IMPORTING iv_clsname      TYPE seoclsname
                                    iv_entity_name  TYPE evtb_entity_name
                                    iv_entity_event TYPE evtb_entity_event_name
                                    iv_devc         TYPE devclass
                                    iv_trkorr       TYPE trkorr
                                    iv_overwrite    TYPE seox_boolean
                                    iv_event_key    TYPE /goog/ce_key.
  PROTECTED SECTION .

    METHODS: get_class_properties IMPORTING iv_clsname      TYPE seoclsname
                                            iv_entity_name  TYPE evtb_entity_name
                                  RETURNING VALUE(rs_class) TYPE vseoclass.

    METHODS: gen_evt_handler_local IMPORTING iv_entity_name  TYPE evtb_entity_name
                                             iv_entity_event TYPE evtb_entity_event_name
                                             iv_event_key    TYPE /goog/ce_key
                                   RETURNING VALUE(rv_code)  TYPE rswsourcet.

ENDCLASS.



CLASS ZGOOG_CL_RAP_EVTHDLR_GENERATOR IMPLEMENTATION.


  METHOD create_class.
    DATA: ls_class TYPE vseoclass.

    ls_class = get_class_properties( iv_clsname     = iv_clsname
                                     iv_entity_name = iv_entity_name ).

    DATA(lt_code) = gen_evt_handler_local(
      iv_entity_name  = iv_entity_name
      iv_entity_event = iv_entity_event
      iv_event_key    = iv_event_key ).

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        corrnr          = iv_trkorr
        devclass        = iv_devc
        version         = seoc_version_active
        genflag         = abap_false
        authority_check = abap_true
        overwrite       = iv_overwrite
        locals_imp      = lt_code
        suppress_dialog = abap_true
      CHANGING
        class           = ls_class
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE 'Class created' TYPE 'S'.
    ENDIF.
  ENDMETHOD.


  METHOD gen_evt_handler_local.

    TYPES: BEGIN OF ty_rap_param,
             entity_name TYPE evtb_entity_name,
             event_name  TYPE evtb_entity_name,
             method_name TYPE char30,
           END OF ty_rap_param,
           tt_rap_params TYPE STANDARD TABLE OF ty_rap_param WITH NON-UNIQUE DEFAULT KEY,
           BEGIN OF ty_params,
             event_key  TYPE /goog/ce_key,
             event_list TYPE tt_rap_params,
           END OF ty_params.

    DATA: ls_params TYPE ty_params,
          ls_event  TYPE ty_rap_param.

    DATA: lo_cmp TYPE REF TO cl_cmp_composer,
          lx_cmp TYPE REF TO cx_cmp_failure.
    CONSTANTS: c_template TYPE string VALUE 'ZGOOG_I_GEN_RAP_EVTHANDLER_TMP'.

    TRY.
***instantiate composer
        lo_cmp = cl_cmp_composer=>s_create( ).

        ls_params-event_key = iv_event_key.

        ls_event-entity_name = iv_entity_name.
        ls_event-event_name = iv_entity_event.
        ls_event-method_name = |ON_{ iv_entity_event }|.

        APPEND ls_event TO ls_params-event_list.

*Generate Define Method code snippet
        lo_cmp->add_var( i_name = 'params' i_value = ls_params ).

        rv_code = lo_cmp->build_code( i_template_include = c_template ).

        DATA lo_pretty_printer TYPE REF TO cl_sedi_pretty_printer.
        CREATE OBJECT lo_pretty_printer.

        lo_pretty_printer->format_source( CHANGING c_source = rv_code ).

      CATCH cx_cmp_failure INTO lx_cmp.
        DATA(lv_err_text) = lx_cmp->get_text( ).
      CATCH cx_sedi_pretty_printer INTO DATA(lx_exp).
        lv_err_text = lx_exp->get_text( ).
      CATCH /iwbep/cx_sb_builder_exception INTO DATA(lx_build).
        lv_err_text = lx_build->get_text( ).

    ENDTRY.


  ENDMETHOD.


  METHOD get_class_properties.
    DATA: ls_class   TYPE vseoclass.

    ls_class-clsname  = iv_clsname.
    ls_class-langu    = sy-langu.
    ls_class-descript = '[Generated] Event Handler for:' && iv_entity_name.

    ls_class-version     = seoc_version_active.
    ls_class-category    = seoc_category_general.
    ls_class-exposure    = seoc_exposure_public.
    ls_class-state       = seoc_state_implemented.
    ls_class-release     = seoc_relstat_not_released.
    ls_class-author      = sy-uname.
    ls_class-createdon   = sy-datum.
    ls_class-fixpt       = abap_true.
    ls_class-unicode     = abap_true.
    ls_class-r3release   = sy-saprl.
    ls_class-clsccincl   = abap_true.


    ls_class-clsdefint   = iv_entity_name.
    ls_class-clsabstrct  = abap_true.
    ls_class-clsfinal    = abap_true.
    ls_class-category    = '07'.

    rs_class = ls_class.

  ENDMETHOD.
ENDCLASS.
