*&---------------------------------------------------------------------*
*& Include          ZGOOG_I_GEN_RAP_EVTHANDLER_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  PARAMETERS: p_entn TYPE evtb_entity_name DEFAULT 'R_PRODUCTTP' MATCHCODE OBJECT scsp_sh_root_entity_name, "scsp_sh_entity_name,
              p_evtn TYPE evtb_entity_event_name DEFAULT 'CREATED',
              p_ekey TYPE /goog/ce_key DEFAULT 'EVENT_TEST' MATCHCODE OBJECT /goog/sh_event_router_key.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS: p_clas TYPE seoclsname DEFAULT 'ZCL_PRODUCT_EXT',
              p_devc TYPE devclass DEFAULT '$TMP',
              p_trko TYPE trkorr MATCHCODE OBJECT shlp_trn_reg_find_request,
              p_owrt TYPE seox_boolean DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
