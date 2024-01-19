Resumable Upload to Google Cloud Storage Bucket
-----------------------------------------------
When you need to upload a large file to Google Cloud storage, you can use resumable upload option as described in code [sample](https://cloud.google.com/solutions/sap/docs/abap-sdk/samples/gcs-upload-file-chunking-customer-supplied-key). In this example, ABAP Client Library class /GOOG/CL_STORAGE_V1 handles breaking the large input file into smaller chunks and upload these chunks one after the other.

However, in some cases, the file can be larger than what an ABAP variable can handle. In such cases, you may want to handle the chunking logic within your code. You can achieve such functionality by extending class /GOOG/CL_STORAGE_V1 and writing logic to allow the calling program to do the chunking. This is demonstrated in this sample class [class](zcl_storage_v1_resumable.clas.abap) and an example implementation is given in [this program](zr_storage_resumable_upload.prog.abap)



