*&---------------------------------------------------------------------*
*& Report ZGOOG_R_GEN_RAP_EVTHANDLER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgoog_r_gen_rap_evthandler.

INCLUDE zgoog_i_gen_rap_evthandler_sel.

START-OF-SELECTION.
  DATA(lo_generator) = NEW zgoog_cl_rap_evthdlr_generator( ).
  IF lo_generator IS BOUND.
    lo_generator->create_class(
      iv_clsname      = p_clas
      iv_entity_name  = p_entn
      iv_entity_event = p_evtn
      iv_devc         = p_devc
      iv_trkorr       = p_trko
      iv_overwrite    = p_owrt
      iv_event_key    = p_ekey
    ).
  ENDIF.
