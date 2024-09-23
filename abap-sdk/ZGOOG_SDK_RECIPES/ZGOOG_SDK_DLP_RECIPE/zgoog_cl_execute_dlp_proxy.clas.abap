class ZGOOG_CL_EXECUTE_DLP_PROXY definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .
protected section.
private section.
ENDCLASS.



CLASS ZGOOG_CL_EXECUTE_DLP_PROXY IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    " Replacement: Replaces a detected sensitive value with a specified surrogate value.
    " Input has email id
    zgoog_cl_dlp_proxy=>call_dlp(
    EXPORTING
      iv_input_value  = 'foo.bar@example.com' " to be replaced with actual input value
      iv_input_type   = 'EMAIL'
    IMPORTING
    ev_output_value = DATA(lv_output_email)
    ev_message      = DATA(lv_message)
          ).
    out->write( 'Replaced value of email: ' ).
    out->write( lv_output_email ).
    out->write( lv_message ).

    " Masking: Replaces a number of characters of a sensitive value with a specified surrogate character, such as a hash (#) or asterisk (*).
    " Input has phone number
    zgoog_cl_dlp_proxy=>call_dlp(
    EXPORTING
      iv_input_value  = '010-2345-6789' " to be replaced with actual input value
      iv_input_type   = 'PHONE NUMBER'
    IMPORTING
    ev_output_value = DATA(lv_output_phone)
          ev_message      = lv_message
          ).
    out->write( 'Masked value of phone number: ' ).
    out->write( lv_output_phone ).
    out->write( lv_message ).

    " Crypto-based tokenization: Encrypts the original sensitive data value using a cryptographic key. Sensitive Data Protection supports several types of tokenization,
    " including transformations that can be reversed, or "re-identified."
    " Input has bank account number
    zgoog_cl_dlp_proxy=>call_dlp(
    EXPORTING
      iv_input_value  = '1234567890' " to be replaced with actual input value
      iv_input_type   = 'BANK ACCOUNT'
    IMPORTING
    ev_output_value = DATA(lv_output_bank_acc)
          ev_message      = lv_message
          ).
    out->write( 'Hashed value of bank account number: ' ).
    out->write( lv_output_bank_acc ).
    out->write( lv_message ).

    " Redaction: Deletes all or part of a detected sensitive value
    " Input has remarks with PII
    zgoog_cl_dlp_proxy=>call_dlp(
    EXPORTING
      iv_input_value  = 'Alternate Email : foo_bar@example.com' " to be replaced with actual input value
      iv_input_type   = 'REMARKS'
    IMPORTING
    ev_output_value = DATA(lv_output_remarks)
          ev_message      = lv_message
          ).
    out->write( 'Redacted email id from remarks: ' ).
    out->write( lv_output_remarks ).
    out->write( lv_message ).

  ENDMETHOD.
ENDCLASS.
