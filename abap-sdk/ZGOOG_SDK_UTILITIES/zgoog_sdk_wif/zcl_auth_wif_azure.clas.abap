class ZCL_AUTH_WIF_AZURE definition
  public
  inheriting from /GOOG/CL_AUTH_WIF_BASE
  final
  create public .

public section.
protected section.

  methods GET_EXT_IDP_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_AUTH_WIF_AZURE IMPLEMENTATION.


  METHOD GET_EXT_IDP_TOKEN.
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

    TYPES:
      BEGIN OF t_azure_resp,
        access_token TYPE string,
      END OF t_azure_resp.

    DATA: lo_client TYPE REF TO if_http_client.

    DATA: lv_url type string.
    lv_url = 'http://169.254.169.254/metadata/identity/oauth2/token?resource=<APP_ID_URI>&api-version=2018-02-01'.
    "Replace <APP_ID_URI> with the value of Application ID URI of the application that you've configured for workload identity federation.

    cl_http_client=>create_by_url(
       EXPORTING
         url                        = lv_url
       IMPORTING
         client                     = lo_client
       EXCEPTIONS
         argument_not_found         = 1
         plugin_not_active          = 2
         internal_error             = 3
         pse_not_found              = 4
         pse_not_distrib            = 5
         pse_errors                 = 6
         oa2c_set_token_error       = 7
         oa2c_missing_authorization = 8
         oa2c_invalid_config        = 9
         oa2c_invalid_parameters    = 10
         oa2c_invalid_scope         = 11
         oa2c_invalid_grant         = 12
         OTHERS                     = 13 ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_client->request->set_method( 'GET' ).
    lo_client->request->set_header_field( name = 'Metadata' value = 'true' ).

    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    lo_client->propertytype_logon_popup = lo_client->co_disabled.

    lo_client->receive(
      EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

    DATA: lv_json TYPE string.

    lv_json = lo_client->response->get_cdata( ).

    DATA: ls_azure_resp TYPE t_azure_resp.

    /goog/cl_json=>deserialize(
      EXPORTING
        json             = lv_json
      CHANGING
        data             = ls_azure_resp ).
    cv_token = ls_azure_resp-access_token.
    cv_token_type = 'urn:ietf:params:oauth:token-type:jwt'.

  ENDMETHOD.
ENDCLASS.
