class ZCL_AUTH_WIF_AWS definition
  public
  inheriting from /GOOG/CL_AUTH_WIF_BASE
  final
  create public .

public section.

  types:
    BEGIN OF t_header_field,
      key type string,
      value TYPE string,
    END OF t_header_field .
  types:
    tt_header_field type STANDARD TABLE OF t_header_field WITH DEFAULT KEY .
  types:
    BEGIN OF t_token_request,
     url type string,
     method type string,
     headers type tt_header_field,
   END OF t_token_request ,

   BEGIN OF t_aws_creds_fetch,
      access_key_id TYPE string,
      secret_access_key TYPE string,
   END OF t_aws_creds_fetch.
protected section.

  methods GET_EXT_IDP_TOKEN
    redefinition .
private section.

  class-methods FETCH_DYNAMIC_AWS_CREDS
    changing
      !PC_AWS_CREDS type T_AWS_CREDS_FETCH
    returning
      value(RV_SUCCESS) type FLAG .
ENDCLASS.



CLASS ZCL_AUTH_WIF_AWS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_AUTH_WIF_AWS->GET_EXT_IDP_TOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SERVICE_ACCOUNT             TYPE        /GOOG/SERVICE_ACCOUNT_NAME
* | [--->] IV_KEYNAME                     TYPE        /GOOG/KEYNAME
* | [--->] IT_QUERY_PARAMS                TYPE        /GOOG/T_HTTP_KEYS
* | [<---] EV_RET_CODE                    TYPE        /GOOG/RETCO
* | [<---] EV_ERR_TEXT                    TYPE        STRING
* | [<---] EV_ERR_RESP                    TYPE        /GOOG/ERR_RESP
* | [<---] EV_LOGTIME                     TYPE        TIMESTAMP
* | [<-->] CV_TOKEN                       TYPE        /GOOG/REFRESH_TOKEN
* | [<-->] CV_TOKEN_TYPE                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_ext_idp_token.
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

  DATA: ls_key       TYPE /goog/client_key.

  /goog/cl_utility=>get_client_key( EXPORTING iv_keyname    = iv_keyname
                                        IMPORTING es_client_key = ls_key ).


  DATA: lv_awsdate TYPE string.

  DATA: lv_date         TYPE dats,
        lv_time         TYPE tims,
        lv_timestamp    TYPE timestampl,
        lv_tz_utc       TYPE timezone VALUE 'UTC',
        lv_awsts        TYPE string,
        lv_timechar(32) TYPE c.

  GET TIME STAMP FIELD lv_timestamp.

  CONVERT TIME STAMP lv_timestamp TIME ZONE lv_tz_utc INTO DATE lv_date TIME lv_time.
  MOVE lv_timestamp TO lv_timechar.
  CONDENSE lv_timechar.

  lv_awsdate = lv_date(4) &&
               lv_date+4(2) &&
               lv_date+6(2) &&
               'T' &&
               lv_time(2) &&
               lv_time+2(2) &&
               lv_time+4(2) &&
               'Z'.

  TRANSLATE lv_awsdate TO UPPER CASE.

  DATA: lv_lf TYPE string.
  DATA: lv_secret_key TYPE string.
  DATA: lv_accesskey TYPE string.
  DATA: lv_datepart TYPE string.
  DATA: lv_service TYPE string.
  DATA: lv_method TYPE string.

  lv_lf = cl_abap_char_utilities=>newline.

  DATA: ls_aws_creds TYPE t_aws_creds_fetch.

  fetch_dynamic_aws_creds(
    CHANGING
      pc_aws_creds = ls_aws_creds ).

  lv_accesskey =  ls_aws_creds-access_key_id.
  lv_secret_key = ls_aws_creds-secret_access_key.
  lv_datepart = lv_awsdate(8).
  lv_service = 'sts'.
  lv_method = 'GET'.


  DATA: lv_canonical_query_params TYPE string.
  DATA: lv_host TYPE string.
  DATA: lv_region TYPE string.
  DATA: lv_canonical_resource_path TYPE string.

  lv_canonical_query_params = 'Action=GetCallerIdentity&Version=2011-06-15'.
  lv_host = 'sts.amazonaws.com'.
  lv_region = '<Populate your AWS Region>'.   "Example: 'us-east-1'
  lv_canonical_resource_path = '/'.

  DATA: lv_canonical_header_names TYPE string.
  DATA: lv_canonical_headers TYPE string.

  lv_canonical_header_names = 'host;x-amz-date'.
  lv_canonical_headers = 'host:' && lv_host && lv_lf && 'x-amz-date:' && lv_awsdate && lv_lf.

  DATA: lv_canonical_request TYPE string.

  CONCATENATE lv_method lv_lf
              lv_canonical_resource_path lv_lf
              lv_canonical_query_params lv_lf
              lv_canonical_headers lv_lf
              lv_canonical_header_names
              INTO lv_canonical_request.

  DATA: lv_canonical_request_hash TYPE string.

  TRY.
      cl_abap_message_digest=>calculate_hash_for_char(
       EXPORTING
         if_algorithm = 'SHA-256'
         if_data = lv_canonical_request
       IMPORTING
         ef_hashstring = lv_canonical_request_hash ).
    CATCH cx_abap_message_digest.
      "Handle error
      RETURN.
  ENDTRY.

  TRANSLATE lv_canonical_request_hash TO LOWER CASE.

  DATA: lv_algorithm TYPE string.

  lv_algorithm = 'AWS4-HMAC-SHA256'.

  DATA: lv_credential_scope TYPE string.

  CONCATENATE lv_datepart '/' lv_region '/' lv_service '/' 'aws4_request' INTO lv_credential_scope.

  DATA: lv_string_to_sign TYPE string.

  CONCATENATE lv_algorithm lv_lf
              lv_awsdate lv_lf
              lv_credential_scope lv_lf
              lv_canonical_request_hash
              INTO lv_string_to_sign.

  DATA: lv_awskey TYPE string.

  CONCATENATE 'AWS4' lv_secret_key INTO lv_awskey.

  DATA: lv_ksecret TYPE xstring.

  TRY.
      lv_ksecret = cl_abap_hmac=>string_to_xstring( lv_awskey ).
    CATCH cx_abap_message_digest .
      "Handle error
      RETURN.
  ENDTRY.

  DATA: lv_kdate  TYPE xstring.
  TRY.
      cl_abap_hmac=>calculate_hmac_for_char(
        EXPORTING
           if_algorithm = 'SHA256'
           if_key = lv_ksecret
           if_data = lv_datepart
        IMPORTING
           ef_hmacxstring = lv_kdate ).
    CATCH cx_abap_message_digest. "
      "Handle error
      RETURN.
  ENDTRY.

  DATA: lv_kregion TYPE xstring.
  TRY.
      cl_abap_hmac=>calculate_hmac_for_char(
        EXPORTING
           if_algorithm = 'SHA256'
           if_key = lv_kdate
           if_data = lv_region
        IMPORTING
             ef_hmacxstring = lv_kregion ).
    CATCH cx_abap_message_digest.
      "Handle error
      RETURN.
  ENDTRY.

  DATA: lv_kservice TYPE xstring.
  TRY.
      cl_abap_hmac=>calculate_hmac_for_char(
         EXPORTING
           if_algorithm = 'SHA256'
           if_key = lv_kregion
           if_data = lv_service
           IMPORTING
             ef_hmacxstring = lv_kservice ).
    CATCH cx_abap_message_digest.
      "Handle error
      RETURN.
  ENDTRY.

  DATA: lv_ksigningkey TYPE xstring.
  TRY.
      cl_abap_hmac=>calculate_hmac_for_char(
         EXPORTING
           if_algorithm = 'SHA256'
           if_key = lv_kservice
           if_data = 'aws4_request'
         IMPORTING
             ef_hmacxstring = lv_ksigningkey ).
    CATCH cx_abap_message_digest.
      "Handle error
      RETURN.
  ENDTRY.

  DATA: lv_stringtosign TYPE string.

  lv_stringtosign = 'AWS4-HMAC-SHA256' && lv_lf &&
                   lv_awsdate && lv_lf &&
                   lv_datepart && '/' &&
                   lv_region && '/' &&
                   lv_service && '/aws4_request' && lv_lf &&
                   lv_canonical_request_hash.

  DATA: lv_ssignature TYPE string.

  TRY.
      cl_abap_hmac=>calculate_hmac_for_char(
         EXPORTING
           if_algorithm = 'SHA256'
           if_key = lv_ksigningkey
           if_data = lv_stringtosign
         IMPORTING
           ef_hmacstring = lv_ssignature ).
    CATCH cx_abap_message_digest.
      "Handle error
      RETURN.
  ENDTRY.

  TRANSLATE lv_ssignature TO LOWER CASE.

  DATA: lv_authorization_header TYPE string.

  lv_authorization_header = 'AWS4-HMAC-SHA256 Credential=' &&
                            lv_accesskey && '/' &&
                            lv_credential_scope &&
                            ', SignedHeaders=' &&
                            lv_canonical_header_names &&
                            ', Signature=' &&
                            lv_ssignature.

  DATA: ls_token_request TYPE t_token_request.

  ls_token_request-url = 'https://sts.amazonaws.com?Action=GetCallerIdentity&Version=2011-06-15'.
  ls_token_request-method = 'POST'.

  DATA: ls_header_field TYPE t_header_field.
  ls_header_field-key = 'Authorization'.
  ls_header_field-value = lv_authorization_header.
  APPEND ls_header_field TO ls_token_request-headers.

  CLEAR: ls_header_field.
  ls_header_field-key = 'host'.
  ls_header_field-value = 'sts.amazonaws.com'.
  APPEND ls_header_field TO ls_token_request-headers.

  CLEAR: ls_header_field.
  ls_header_field-key = 'x-amz-date'.
  ls_header_field-value = lv_awsdate.
  APPEND ls_header_field TO ls_token_request-headers.

  CLEAR: ls_header_field.
  ls_header_field-key = 'x-goog-cloud-target-resource'.
  ls_header_field-value = '//iam.googleapis.com/projects/' &&
                               ls_key-project_id &&
                               '/locations/global/workloadIdentityPools/' &&
                               ls_key-auth_param1 &&
                               '/providers/' &&
                               ls_key-auth_param2.
  APPEND ls_header_field TO ls_token_request-headers.

  cv_token = /ui2/cl_json=>serialize(  ls_token_request ).
  cv_token_type = 'urn:ietf:params:aws:token-type:aws4_request'.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_AUTH_WIF_AWS=>FETCH_DYNAMIC_AWS_CREDS
* +-------------------------------------------------------------------------------------------------+
* | [<-->] PC_AWS_CREDS                   TYPE        T_AWS_CREDS_FETCH
* | [<-()] RV_SUCCESS                     TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fetch_dynamic_aws_creds.
    DATA: lo_http       TYPE REF TO if_http_client,
          lv_imds_token TYPE string,
          lv_role       TYPE string.

    " Phase 1: Get IMDSv2 Session Token (Security Requirement)
    cl_http_client=>create_by_url( EXPORTING url = 'http://169.254.169.254/latest/api/token'
                                   IMPORTING client = lo_http ).
    lo_http->request->set_method( 'PUT' ).
    lo_http->request->set_header_field( name = 'X-aws-ec2-metadata-token-ttl-seconds' value = '21600' ).
    lo_http->send( ). lo_http->receive( ).
    lv_imds_token = lo_http->response->get_cdata( ). lo_http->close( ).

    " Phase 2: Get IAM Role name
    cl_http_client=>create_by_url( EXPORTING url = 'http://169.254.169.254/latest/meta-data/iam/security-credentials/'
                                   IMPORTING client = lo_http ).
    lo_http->request->set_header_field( name = 'X-aws-ec2-metadata-token' value = lv_imds_token ).
    lo_http->send( ). lo_http->receive( ).
    lv_role = lo_http->response->get_cdata( ). lo_http->close( ).

    " Phase 3: Retrieve actual credentials into me->gs_aws_creds
    cl_http_client=>create_by_url( EXPORTING url = |http://169.254.169.254/latest/meta-data/iam/security-credentials/{ lv_role }|
                                   IMPORTING client = lo_http ).
    lo_http->request->set_header_field( name = 'X-aws-ec2-metadata-token' value = lv_imds_token ).
    lo_http->send( ). lo_http->receive( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lo_http->response->get_cdata( )
                               CHANGING data = pc_aws_creds ).
    lo_http->close( ).
    rv_success = abap_true.

  ENDMETHOD.
ENDCLASS.
