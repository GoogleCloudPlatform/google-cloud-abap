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
INTERFACE zgoog_if_chat_cards_v2
  PUBLIC.

  TYPES ty_t_string TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_action_parameters,
           key   TYPE string,
           value TYPE string,
         END OF ty_action_parameters.
  TYPES ty_t_action_parameters TYPE STANDARD TABLE OF ty_action_parameters WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_header,
           title          TYPE string,
           subtitle       TYPE string,
           image_type     TYPE string, " SQUARE or CIRCLE
           image_url      TYPE string,
           image_alt_text TYPE string,
         END OF ty_header.

  TYPES: BEGIN OF ty_text_paragraph,
           text      TYPE string,
           max_lines TYPE int4,
         END OF ty_text_paragraph.

  TYPES: BEGIN OF ty_material_icon,
           name   TYPE string,
           fill   TYPE boolean,
           weight TYPE i,
           grade  TYPE i,
         END OF ty_material_icon.

  TYPES: BEGIN OF ty_icon,
           alt_text      TYPE string,
           image_type    TYPE string,
           known_icon    TYPE string,
           icon_url      TYPE string,
           material_icon TYPE ty_material_icon,
         END OF ty_icon.

  TYPES: BEGIN OF ty_action,
           function                 TYPE string,
           parameters               TYPE ty_t_action_parameters,
           load_indicator           TYPE string,                 " SPINNER or NONE
           persist_values           TYPE boolean,
           interatcion              TYPE string,                 " INTERACTION_UNSPECIFIED or OPEN_DIALOG
           required_widgets         TYPE ty_t_string,
           all_widgets_are_required TYPE boolean,
         END OF ty_action.

  TYPES: BEGIN OF ty_open_link,
           url      TYPE string,
           open_as  TYPE string, " FULL_SIZE or OVERLAY
           on_close TYPE string, " NOTHING or RELOAD
         END OF ty_open_link.

  TYPES: BEGIN OF ty_overflow_menu_item,
           start_icon TYPE ty_icon,
           text       TYPE string,
           on_click   TYPE REF TO data,
           disabled   TYPE boolean,
         END OF ty_overflow_menu_item,
         ty_overflow_menu_items TYPE STANDARD TABLE OF ty_overflow_menu_item WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_overflow_menu,
           items TYPE ty_overflow_menu_items,
         END OF ty_overflow_menu.

  TYPES: BEGIN OF ty_on_click,
           action                   TYPE ty_action,
           open_link                TYPE ty_open_link,
           open_dynamic_link_action TYPE ty_action,
           card                     TYPE REF TO data,
           overflow_menu            TYPE ty_overflow_menu,
         END OF ty_on_click.

  TYPES: BEGIN OF ty_color,
           red   TYPE string,
           green TYPE string,
           blue  TYPE string,
           alpha TYPE string,
         END OF ty_color.

  TYPES: BEGIN OF ty_button,
           text     TYPE string,
           icon     TYPE ty_icon,
           color    TYPE ty_color,
           on_click TYPE ty_on_click,
           disabled TYPE boolean,
           alt_text TYPE string,
           "@type type string,
         END OF ty_button.

  TYPES: BEGIN OF ty_switch_control,
           name             TYPE string,
           value            TYPE string,
           selected         TYPE boolean,
           on_change_action TYPE action,
           control_type     TYPE string,
         END OF ty_switch_control.

  TYPES: BEGIN OF ty_decorated_text,
           icon           TYPE ty_icon,
           start_icon     TYPE ty_icon,
           top_label      TYPE string,
           text           TYPE string,
           wrap_text      TYPE string,
           botton_label   TYPE string,
           on_click       TYPE ty_on_click,
           button         TYPE ty_button,
           switch_control TYPE ty_switch_control,
           end_icon       TYPE ty_icon,
         END OF ty_decorated_text.

  TYPES ty_t_buttons TYPE STANDARD TABLE OF ty_button WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_button_list,
           buttons TYPE ty_t_buttons,
         END OF ty_button_list.

  TYPES: BEGIN OF ty_suggestion_item,
           text TYPE string,
         END OF ty_suggestion_item,
         ty_t_suggestion_items TYPE STANDARD TABLE OF ty_suggestion_item WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_suggestions,
           items TYPE ty_t_suggestion_items,
         END OF ty_suggestions.

  TYPES: BEGIN OF ty_validation,
           character_limit TYPE int4,
           input_type      TYPE string,
         END OF ty_validation.

  TYPES: BEGIN OF ty_text_input,
           name                 TYPE string,
           label                TYPE string,
           hinttext             TYPE string,
           value                TYPE string,
           type                 TYPE string,
           on_change_action     TYPE ty_action,
           initial_suggestions  TYPE ty_suggestions,
           auto_complete_action TYPE ty_action,
           validation           TYPE ty_validation,
           placeholdertext      TYPE string,
         END OF ty_text_input.

  TYPES: BEGIN OF ty_selection_item,
           bottom_text    TYPE string,
           selected       TYPE abap_bool,
           start_icon_uri TYPE string,
           text           TYPE string,
           value          TYPE string,
         END OF ty_selection_item.
  TYPES ty_t_selection_item TYPE TABLE OF ty_selection_item WITH NON-UNIQUE DEFAULT KEY.

  TYPES:
    BEGIN OF ty_selection_items,
      items TYPE ty_t_selection_item,
    END OF ty_selection_items.

  TYPES: BEGIN OF ty_platform_data_source,
           common_data_source TYPE string,
           " host_App_Data_Source TYPE ty_Host_App_DSP,
         END OF ty_platform_data_source.

  TYPES: BEGIN OF ty_selection_input,
           name               TYPE string,
           label              TYPE string,
           type               TYPE string,
           items              TYPE ty_t_selection_item,
           on_change_action   TYPE ty_action,
           " multi_Select_Max_Selected_Items TYPE int4,
           " multi_Select_Min_Query_Length TYPE int4,
           validation         TYPE ty_validation,
           externaldatasource TYPE ty_action,
           platformdatasource TYPE ty_platform_data_source,
         END OF ty_selection_input.

  TYPES: BEGIN OF ty_date_time_picker,
           name                 TYPE string,
           label                TYPE string,
           type                 TYPE string,
           value_msepoch        TYPE string,
           timezone_offset_date TYPE int4,
           on_changeaction      TYPE ty_action,
           validation           TYPE ty_validation,
         END OF ty_date_time_picker.

  TYPES: BEGIN OF ty_divider,
           to_b4 TYPE int2,
         END OF ty_divider.

  TYPES: BEGIN OF ty_grid,
           to_be5 TYPE int2,
         END OF ty_grid.

  TYPES: BEGIN OF ty_columns,
           to_be6 TYPE int2,
         END OF ty_columns.

  TYPES: BEGIN OF ty_chip_list,
           to_be7 TYPE int2,
         END OF ty_chip_list.

  TYPES: BEGIN OF ty_widget,
           horizontal_alignment TYPE string,
           " Union field data can be only one of the following:
           text_paragraph       TYPE ty_text_paragraph,
           " image                TYPE Image,
           decorated_text       TYPE ty_decorated_text,
           button_list          TYPE ty_button_list,
           text_input           TYPE ty_text_input,
           selection_input      TYPE ty_selection_input,
           date_time_picker     TYPE ty_date_time_picker,
           divider              TYPE ty_t_string,
           grid                 TYPE ty_grid,
           columns              TYPE ty_columns,
           chip_list            TYPE ty_chip_list,
         END OF ty_widget.

  TYPES ty_t_widget TYPE STANDARD TABLE OF ty_widget WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_sections,
           header                      TYPE string,
           widgets                     TYPE ty_t_widget,
           collapsible                 TYPE boolean,
           uncollapsible_widgets_count TYPE int2,
           " collapse_Control            TYPE ty_collapse_control,
         END OF ty_sections.

  TYPES ty_t_sections TYPE STANDARD TABLE OF ty_sections WITH NON-UNIQUE DEFAULT KEY.

  TYPES: BEGIN OF ty_cards_v2,
           header                TYPE ty_header,
           sections              TYPE ty_t_sections,
           section_divider_style TYPE string,
           " card_actions          TYPE ty_t_card_actions,
           name                  TYPE string,
           " fixed_footer          TYPE ty_card_fixed_footer,
           display_style_name    TYPE string,
           peek_card_header      TYPE ty_header,
         END OF ty_cards_v2.

ENDINTERFACE.
