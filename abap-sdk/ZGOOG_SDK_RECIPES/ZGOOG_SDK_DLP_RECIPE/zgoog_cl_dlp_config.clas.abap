class ZGOOG_CL_DLP_CONFIG definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .
protected section.
private section.
ENDCLASS.



CLASS ZGOOG_CL_DLP_CONFIG IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA : lt_dlp_config TYPE TABLE OF zgoog_dlp_config.

*   fill internal table (itab)
    lt_dlp_config = VALUE #(
    ( client = sy-mandt keyword  = 'EMAIL'        infotype = 'EMAIL_ADDRESS'  )
    ( client = sy-mandt keyword  = 'PHONE NUMBER' infotype = 'PHONE_NUMBER' number_to_mask = 5 masking_char = '*' )
    ( client = sy-mandt keyword  = 'REMARKS'      infotype = 'EMAIL_ADDRESS'  )
    ( client = sy-mandt keyword  = 'REMARKS'      infotype = 'PHONE_NUMBER'  )
    ( client = sy-mandt keyword  = 'BANK ACCOUNT' infotype = 'FINANCIAL_ACCOUNT_NUMBER' surrogate_infotype = 'ACCOUNT' common_alphabet = 'ALPHA_NUMERIC' )
    ).

*   insert the new table entries and print the result
    MODIFY zgoog_dlp_config FROM TABLE @lt_dlp_config.
    IF sy-subrc EQ 0.
      out->write( sy-dbcnt ).
      out->write( 'records inserted successfully!' ).
    ELSE.
      out->write( 'Problem during data modification' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
