# ABAP SDK for Google Cloud code samples

## Pub/Sub
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Publish topics](zgoog_sdk_cs_pubsub_v1/zr_publish_topics.prog.abap)      | This code sample can be used to publish a "Hello World!" message to a Pub/Sub Topic. |
| [Pull subscriptions](zgoog_sdk_cs_pubsub_v1/zr_pull_subscriptions.prog.abap)      | This code sample can be used to pull messages that were published to a Pub/Sub Topic using a subscription.      |
| [Create topic](zgoog_sdk_cs_pubsub_v1/zr_create_topics.prog.abap) | This code sample can be used to create a topic in Pub/Sub.     |
| [Create subscription](zgoog_sdk_cs_pubsub_v1/zr_create_subscriptions.prog.abap) | This code sample can be used to create a subscription and attach the same to a Pub/Sub Topic. |
| [List subscriptions of a topic](zgoog_sdk_cs_pubsub_v1/zr_list_subscriptions1.prog.abap) | This code sample can be used to list all the subscriptions attached to a Pub/Sub Topic. |


## Cloud Storage
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Create bucket](zgoog_sdk_cs_storage_v1/zr_insert_buckets.prog.abap) | This code sample can be used to create a bucket in Cloud Storage. |
| [List buckets](zgoog_sdk_cs_storage_v1/zr_list_buckets.prog.abap) | This code sample can be used to list all the buckets in your Cloud project. |
| [Upload file to a bucket](zgoog_sdk_cs_storage_v1/zr_upload_file_bucket.prog.abap) | This code sample can be used to upload an object to a Cloud Storage bucket. |
| [Retrieve metadata of a file](zgoog_sdk_cs_storage_v1/zr_retrieve_file_metadata.prog.abap) | This code sample can be used to retrieve the metadata of an object from a Cloud Storage bucket. |
| [Download file from a bucket](zgoog_sdk_cs_storage_v1/zr_get_objects.prog.abap) | This code sample can be used to download the object data from a Cloud Storage bucket. |
| [Upload file with chunking and customer supplied encryption key](zgoog_sdk_cs_storage_v1/zr_upload_object_rescskey.prog.abap) | This code sample can be used to upload a large object to a Cloud Storage bucket using chunking and providing customer-supplied encryption keys to encrypt the object. |


## Document AI
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Process a single document](zgoog_sdk_cs_documentai_v1/zr_process_processors.prog.abap) | This code sample can be used to process a single document. The default processor version is used to process the document. | 
| [Send a document for human review](zgoog_sdk_cs_documentai_v1/zr_human_review.prog.abap) | This code sample can be used to process a single document and send the processed data for human review. | 
| [Batch process multiple documents](zgoog_sdk_cs_documentai_v1/zr_batch_process_processors.prog.abap) | This code sample can be used to process multiple documents placed in a Cloud Storage bucket with the default processor version. <br /> Documents placed in a source Cloud Storage bucket are processed in a long-running operation and the processed files are placed in a target Cloud Storage bucket in JSON format. | 
| [Process a single document with a processor version](zgoog_sdk_cs_documentai_v1/zr_process_processor_versions.prog.abap) | This code sample can be used to process a single document using a specified processor version. | 
| [Batch process multiple documents with a processor version](zgoog_sdk_cs_documentai_v1/zr_batch_process_processor_ver.prog.abap) | This code sample can be used to process multiple documents placed in a Cloud Storage bucket using a specific processor version.<br /> Documents placed in a source Cloud Storage bucket are processed in a long-running operation and the processed files are placed in a target Cloud Storage bucket in JSON format. | 
| [Get the latest state of a long-running operation](zgoog_sdk_cs_documentai_v1/zr_get_operations1.prog.abap) | This code sample can be used to get the latest state of a long-running operation. | 
| [Start asynchronous cancellation on a long-running operation](zgoog_sdk_cs_documentai_v1/zr_cancel_operations.prog.abap) | This code sample can be used to start asynchronous cancellation on a long-running operation. | 

## Secret Manager
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Create secret](zgoog_sdk_cs_secretmanager_v1/zr_create_secret.prog.abap) | This code sample can be used to create a new secret.  | 
| [Add secret version](zgoog_sdk_cs_secretmanager_v1/zr_add_secret_version.prog.abap ) | This code sample can be used to add a new version to the secret. | 
| [Access secret version](zgoog_sdk_cs_secretmanager_v1/zr_access_secret_version.prog.abap) | This code sample can be used to access a secret version and display the secret. | 
| [Patch secret](zgoog_sdk_cs_secretmanager_v1/zr_patch_secrets.prog.abap) | This code sample can be used to patch a secret by modifying the label information. | 
| [Destroy secret version](zgoog_sdk_cs_secretmanager_v1/zr_destroy_secret_version.prog.abap) | This code sample can be used to destroys a secret version. | 
| [Delete secret](zgoog_sdk_cs_secretmanager_v1/zr_delete_secrets.prog.abap) | This code sample can be used to deletes a secret. | 

## Cloud Translation
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Create glossary](zgoog_sdk_cs_translate_v3/zr_create_glossaries.prog.abap) | This code sample can be used to create a glossary by using Google Cloud Translation API (v3).<br /> A glossary is a custom dictionary the Cloud Translation API (v3) uses to consistently translate business's domain specific terminology.<br /> Make sure a glossary file is prepared and uploaded to a Cloud Storage bucket in the same Google Cloud project. For more information about preparing a glossary file, see [Creating and using glossaries (Advanced).](https://cloud.google.com/translate/docs/advanced/glossary) | 
| [Translate with glossary](zgoog_sdk_cs_translate_v3/zr_translate_wth_glossary.prog.abap) | This code sample can be used to translate glossary texts for an already configured glossary id.<br /> Make sure a glossary id is created for the resource locations. For more information about preparing a glossary file, see [Creating and using glossaries (Advanced).](https://cloud.google.com/translate/docs/advanced/glossary) | <br /> 
| [Translate text in batch mode](zgoog_sdk_cs_translate_v3/zr_batch_translate_text_locati.prog.abap) | This code sample can be used to translate text in batch mode. Make sure that you have completed the following steps:<br /> 1. Store your text file in a source Cloud Storage bucket.<br /> 2. Create a target Cloud Storage bucket to store the translated text files.<br /> 3. Grant the service accounts access to Cloud Storage buckets, in addition to Cloud Translation permissions. | 
| [Translate document in batch mode](zgoog_sdk_cs_translate_v3/zr_batch_translate_document_lo.prog.abap) | This code sample can be used to translate one or more files of supported types in a Cloud Storage bucket and store the translated file into a target Cloud Storage bucket. For batch translations, the service account must have additional permissions to access Cloud Storage buckets along with the permissions to perform translation. For more information, see [Translate documents](https://cloud.google.com/translate/docs/advanced/translate-documents). | 

## Google Sheets
| <div style="width:220px">Example</div>       | Description   | 
| ------------- |---------------|
| [Create Spreadsheet](zgoog_sdk_cs_sheets_v4/zr_create_spreadsheet.prog.abap) | This code sample can be used to create a spreadsheet by using Google Cloud Sheets API (v4).<br /> | 
| [Append values in a spreadsheet](zgoog_sdk_cs_sheets_v4/zr_append_val_spreadsheet.prog.abap) | This code sample can be used to append values in a spreadsheet using Google Cloud Sheets API (v4). | 
| [Update values in a spreadsheet](zgoog_sdk_cs_sheets_v4/zr_update_value_spreadsheet.prog.abap) | This code sample can be used to update values in a spreadsheet using Google Cloud Sheets API (v4). | 