************************************************************************************************************************
* Generated class for handling RAP Events                                                                              *
************************************************************************************************************************
CLASS lcl_event_extension DEFINITION INHERITING FROM cl_abap_behavior_event_handler.

  PRIVATE SECTION.
@table params-event_list
    METHODS $params-event_list-method_name$ FOR ENTITY EVENT
       $params-event_list-event_name$ FOR $params-event_list-entity_name$~$params-event_list-event_name$.
@end
ENDCLASS.

CLASS lcl_event_extension IMPLEMENTATION.

@table params-event_list
  METHOD $params-event_list-method_name$.
    TRY.
        /goog/cl_event_publisher=>publish_event(
          EXPORTING
            iv_event_key = '$params-event_key$'
            it_data      = VALUE #( FOR <ls_created> IN created (
                                    data       = /goog/cl_json=>serialize( data = <ls_created> )  ) )
          IMPORTING
            et_output    = DATA(lt_output)
        ).
      CATCH /goog/cx_sdk INTO DATA(lo_exp).

    ENDTRY.
  ENDMETHOD.
@end
ENDCLASS.
