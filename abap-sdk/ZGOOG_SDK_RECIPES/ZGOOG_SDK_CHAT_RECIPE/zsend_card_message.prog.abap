" --------------------------------------------------------------------
"  Copyright 2024 Google LLC                                         -
"                                                                    -
"  Licensed under the Apache License, Version 2.0 (the "License");   -
"  you may not use this file except in compliance with the License.  -
"  You may obtain a copy of the License at                           -
"      https://www.apache.org/licenses/LICENSE-2.0                   -
"  Unless required by applicable law or agreed to in writing,        -
"  software distributed under the License is distributed on an       -
"  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,      -
"  either express or implied.                                        -
"  See the License for the specific language governing permissions   -
"  and limitations under the License.                                -
" --------------------------------------------------------------------
REPORT zsend_card_message.

DATA lv_client_key TYPE /goog/keyname.
DATA ls_input      TYPE /goog/cl_chat_v1=>ty_072.
DATA lv_space_id   TYPE string.
DATA ls_widget     TYPE zgoog_if_chat_cards_v2=>ty_widget.
DATA ls_section    TYPE zgoog_if_chat_cards_v2=>ty_sections.
DATA ls_card_v2    TYPE zgoog_if_chat_cards_v2=>ty_cards_v2.
DATA ls_card       TYPE /goog/cl_chat_v1=>ty_012.
DATA ls_rb         TYPE zgoog_if_chat_cards_v2=>ty_selection_item.
DATA ls_button     TYPE zgoog_if_chat_cards_v2=>ty_button.
DATA ls_param      TYPE zgoog_if_chat_cards_v2=>ty_action_parameters.

" TODO: Developer Set Values Here
" lv_client_key = "Set the configured Client Key here
" lv_space_id   = "Set the Space ID here

TRY.

    DATA(lo_chat) = NEW /goog/cl_chat_v1( iv_key_name = lv_client_key ).
  CATCH /goog/cx_sdk INTO DATA(lo_excp).
    " Handle exception here
ENDTRY.

" Building the Card Structure

" Set the header
ls_card_v2-header = VALUE zgoog_if_chat_cards_v2=>ty_header(
                              title     = 'Purchase Order Workflow - Level 2 Approval Alert!'
                              subtitle  = 'PO Number: 8700000034'
                              image_url = 'https://developers.google.com/chat/images/quickstart-app-avatar.png' ).

" Create sections
ls_section-header = 'Purchase Order Details'.

ls_widget-decorated_text = VALUE zgoog_if_chat_cards_v2=>ty_decorated_text(
                                     icon = VALUE zgoog_if_chat_cards_v2=>ty_icon( known_icon = 'INVITE' )
                                     text = 'Document Date: 2024-10-23' ).
ls_section-widgets = VALUE #( ( ls_widget ) ).
CLEAR ls_widget.

ls_widget-decorated_text = VALUE zgoog_if_chat_cards_v2=>ty_decorated_text(
    icon = VALUE zgoog_if_chat_cards_v2=>ty_icon(
                     material_icon = VALUE zgoog_if_chat_cards_v2=>ty_material_icon( name = 'category' ) )
    text = 'Document Type: Standard PO'  ).

ls_section-widgets = VALUE #( BASE ls_section-widgets
                              ( ls_widget ) ).
CLEAR ls_widget.

ls_widget-decorated_text = VALUE zgoog_if_chat_cards_v2=>ty_decorated_text(
    icon = VALUE zgoog_if_chat_cards_v2=>ty_icon(
                     material_icon = VALUE zgoog_if_chat_cards_v2=>ty_material_icon( name = 'conveyor_belt' ) )
    text = 'Supplier: 5300000061 - Cymbal Industries'  ).

ls_section-widgets = VALUE #( BASE ls_section-widgets
                              ( ls_widget ) ).
CLEAR ls_widget.

ls_widget-decorated_text = VALUE zgoog_if_chat_cards_v2=>ty_decorated_text(
                                     icon = VALUE zgoog_if_chat_cards_v2=>ty_icon( known_icon = 'TRAIN' )
                                     text = 'Shipping Type: RAIL' ).
ls_section-widgets = VALUE #( BASE ls_section-widgets
                              ( ls_widget ) ).
CLEAR ls_widget.

ls_widget-decorated_text = VALUE zgoog_if_chat_cards_v2=>ty_decorated_text( text = 'Approved By: User-Name' ).
ls_section-widgets = VALUE #( BASE ls_section-widgets
                              ( ls_widget ) ).
CLEAR ls_widget.

ls_section-widgets = VALUE #( BASE ls_section-widgets
                              ( ls_widget ) ).

ls_card_v2-sections = VALUE #( ( ls_section ) ).

ls_card-card = REF #( ls_card_v2 ).
ls_input-cards_v2 = VALUE #( (  ls_card ) ).

TRY.
    lo_chat->create_messages( EXPORTING iv_p_spaces_id = lv_space_id
                                        is_input       = ls_input
                              IMPORTING es_output      = DATA(ls_output)
                                        ev_ret_code    = DATA(lv_ret_code)
                                        ev_err_text    = DATA(lv_err_text)
                                        es_err_resp    = DATA(ls_err_resp) ).
  CATCH /goog/cx_sdk INTO lo_excp.
    " Handle exception here
ENDTRY.

IF lo_chat->is_error( lv_ret_code ).
  " Handle error here
ELSE.
  " Handle success here
ENDIF.
