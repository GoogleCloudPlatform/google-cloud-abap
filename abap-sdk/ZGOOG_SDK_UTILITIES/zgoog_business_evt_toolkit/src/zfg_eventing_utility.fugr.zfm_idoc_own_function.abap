FUNCTION zfm_idoc_own_function.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_EDIDC STRUCTURE  EDIDC
*"----------------------------------------------------------------------

  DATA: lt_return TYPE bapiret2_t,
        lt_edidd  TYPE tab_edidd,
        ls_edidc  TYPE edidc,
        ls_edi_ds TYPE edi_ds.

  FIELD-SYMBOLS: <ls_return> TYPE bapiret2.
  DATA: lv_error TYPE c LENGTH 1.


  LOOP AT i_edidc.

    CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_PROCESS'
      EXPORTING
        document_number          = i_edidc-docnum
      IMPORTING
        idoc_control             = ls_edidc
      EXCEPTIONS
        document_foreign_lock    = 1
        document_not_exist       = 2
        document_number_invalid  = 3
        document_is_already_open = 4
        OTHERS                   = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR lt_edidd.
    REFRESH lt_edidd.

    CALL FUNCTION 'EDI_SEGMENTS_GET_ALL'
      EXPORTING
        document_number         = i_edidc-docnum
      TABLES
        idoc_containers         = lt_edidd
      EXCEPTIONS
        document_number_invalid = 1
        end_of_document         = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_error = 'N'.


    /goog/cl_idoc_event_forward=>publish_event(
      EXPORTING
        is_control = ls_edidc
        it_data    = lt_edidd
      IMPORTING
        et_return  = lt_return
    ).

    LOOP AT lt_return ASSIGNING <ls_return> WHERE type CA 'AEX'.
      lv_error = 'Y'.
      EXIT.
    ENDLOOP.

    CLEAR ls_edi_ds.
    ls_edi_ds-docnum = i_edidc-docnum.
    ls_edi_ds-tabnam = 'EDI_DS'.
    ls_edi_ds-logdat = sy-datum.
    ls_edi_ds-logtim = sy-uzeit.
    ls_edi_ds-repid  = sy-repid.
    IF lv_error = 'Y'.
      " In case unsuccessful, i.e. errors encountered
      " 20  MESSAGE_SEND  1 I   Error triggering EDI subsystem

      ls_edi_ds-status = '20'.
      ls_edi_ds-stamqu = 'SAP'.

      IF <ls_return> IS ASSIGNED.
        ls_edi_ds-stamid = <ls_return>-id.
        ls_edi_ds-stamno = <ls_return>-number.
        ls_edi_ds-stapa1 = <ls_return>-message_v1.
        ls_edi_ds-stapa2 = <ls_return>-message_v2.
        ls_edi_ds-stapa3 = <ls_return>-message_v3.
        ls_edi_ds-stapa4 = <ls_return>-message_v4.

        UNASSIGN: <ls_return>.
      ENDIF.

      CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
        EXPORTING
          document_number         = i_edidc-docnum
          idoc_status             = ls_edi_ds
        EXCEPTIONS
          document_number_invalid = 1
          other_fields_invalid    = 2
          status_invalid          = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      CALL FUNCTION 'IDOC_ERROR_WORKFLOW_START'
        EXPORTING
          docnum                  = i_edidc-docnum
          eventcode               = 'EDIO'
        EXCEPTIONS
          no_entry_in_tede5       = 1
          error_in_start_workflow = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      "If successful.
      " 18    1 I   Triggering EDI subsystem OK
      ls_edi_ds-status = '18'.
      ls_edi_ds-stamqu = 'SAP'.
      ls_edi_ds-stamid = /goog/cl_ce_utility=>c_msg_class_eventing.
      ls_edi_ds-stamno = '003'.

      CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
        EXPORTING
          document_number         = i_edidc-docnum
          idoc_status             = ls_edi_ds
        EXCEPTIONS
          document_number_invalid = 1
          other_fields_invalid    = 2
          status_invalid          = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'EDI_DOCUMENT_CLOSE_PROCESS'
      EXPORTING
        document_number     = i_edidc-docnum
      EXCEPTIONS
        document_not_open   = 1
        failure_in_db_write = 2
        parameter_error     = 3
        status_set_missing  = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.
ENDFUNCTION.
