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
   END OF t_token_request .
protected section.

  methods GET_EXT_IDP_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_AUTH_WIF_AWS IMPLEMENTATION.


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
  lv_accesskey = '<Populate AWS Access Key>'.
  lv_secret_key = '<Populate AWS Secret Access Key>'.
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
ENDCLASS.
