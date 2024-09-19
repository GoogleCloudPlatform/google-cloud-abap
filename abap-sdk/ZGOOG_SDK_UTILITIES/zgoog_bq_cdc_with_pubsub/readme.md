Pub/Sub to BigQuery with Change Data Capture (CDC) Utility
----------------------------------------
This sample utility can be used to publish data in an internal table as messages to a configured Pub/Sub topic. 
The utility provides options to add [Change Data Capture (CDC)](https://cloud.google.com/pubsub/docs/bigquery) fields '_CHANGE_TYPE' and '_CHANGE_SEQUENCE_NUMBER' such that existing records in BigQuery can be updated or deleted. 

### Prerequisite Configurations

Before you implement and use this utility, make sure that you or your administrators have completed the following prerequisites:

-   You have a Google Cloud account and project. Please keep the Project Id with you which is available in Google Cloud [Dashboard](https://console.cloud.google.com/home/dashboard).

-   Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).

-   Pub/Sub API is enabled for your GCP project.

-   Follow the [Authentication guide](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/authentication) to set up a service account (say abap-sdk-qs@PROJECT_ID.iam.gserviceaccount.com) and grant appropriate permissions for Pub/Sub and set up ABAP SDK Client Key. 

-   You have created a BigQuery Table, Pub/Sub Avro schema, Topic and a corresponding subscription to BigQuery table. These are explained with an example in this [Medium Blog](https://medium.com/@uajith/bigquery-change-data-capture-cdc-using-pub-sub-b7881075acb8).

*  Validate the configuration 'ABAP_SDK_DEV' using SPRO > ABAP SDK for Google Cloud > Utilities > Validate Authentication Configuration.

### Configure Pub/Sub Topic to publish messages

Use Tcode SM30 and maintain entries in table ZGOOG_PUBSUB_BQ
| Field Name                             | Value         |
| ------------------------------------   | ------------- |
| **Data Source:**                       | Name of the data source such as Table or View |
| **Key Name:**                          | [Google Cloud Key Name](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/install-config#client_key_config)   |
| **Pub/Sub Topic:**                     | Pub/Sub topic name |

### Use the utility 
The utility class method ZGOOG_CL_PUBSUB_TO_BQ=>SEND_TO_PUBSUB can now be used from application programs to publish messages to the configured Pub/Sub topic.

| Method Parameter                       | Value         |
| ------------------------------------   | ------------- |
| **IT_DATA**                            | Internal table with data to publish to Pub/Sub |
| **IV_DATASOURCE**                      | Name of the data source configured in ZGOOG_PUBSUB_BQ |
| **IV_SET_CDC_FIELDS**                  | Optional. Flag to set CDC fields in Pub/Sub message |
| **IV_CDC_FIELD**                       | Optional. The CDC field name in IT_DATA, which can be used to identify UPSERT Vs DELETE |
| **IV_CDC_DEL_VALUE**                   | Optional. The value of CDC field for DELETE
| **IV_CHANGE_SEQUENCE_NUMBER_FLD**      | Optional. An integer field in IT_DATA that can be used as _CHANGE_SEQUENCE_NUMBER
| **Custom Entity**                      | Name of the custom entity to be generated 
| **EV_RET_CODE**        | If message is successfully published, return code will be 200. 
| **EV_ERR_TEXT**               | Error text in case of failures
| **EV_ERR_RESP**               | Error Response in case of failures

### Demo Program
Use the demo program ZR_DEMO_PUBSUB_TO_BQ to test the utility for the sample scenario documented in the [blog](https://medium.com/@uajith/bigquery-change-data-capture-cdc-using-pub-sub-b7881075acb8)
