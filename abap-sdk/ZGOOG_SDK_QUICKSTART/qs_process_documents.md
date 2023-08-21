# Process Documents

This quickstart shows you how to process documents (invoices) from a source bucket and store the processed document (JSON file) in a target bucket by using the batch processing capability of [Document AI API](https://cloud.google.com/document-ai/docs/reference/rest).

---

## Before you begin

Before you run this quickstart, make sure that you or your administrators have completed the following prerequisites:

* You have a Google Cloud account and project.
* Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).
* The ABAP SDK for Google Cloud is installed and configured. [See how to install and configure the ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config).
* Authentication to access Google Cloud APIs is set up. [See how to set up authentication](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/authentication).
* Make sure the Document AI API in enabled in your Google Cloud project.
* [Go to API library](https://console.cloud.google.com/project/_/apis/library/documentai.googleapis.com?_ga=2.206668307.1647484855.1692595078-539814502.1692344606&_gac=1.128498046.1692595451.Cj0KCQjwrfymBhCTARIsADXTabkYt1JrKGWwFS3LWRs7hwDEuej5fiIH68_Z1QJXoaYOJHZy3QNvL5caAnesEALw_wcB)
* In the Document AI Workbench, create a processor with type INVOICE_PROCESSOR. For more information, see [Creating and managing processors](https://cloud.google.com/document-ai/docs/create-processor).
* In Cloud Storage, create a source bucket to store the invoices for processing and place the invoices in this bucket. For more information, see [Create buckets](https://cloud.google.com/storage/docs/creating-buckets).
* In Cloud Storage, create a target bucket to store the processed files.

---

## Create a program to process documents
1. In the SAP system, create an executable program in your custom namespace (for example, Z or Y) by using transaction **SE38**.
    1. In the SAP GUI, enter transaction code **SE38**.
    2. In the Program field, enter a name of your program, for example, **ZDEMO_DOCUMENT_AI**.
    3. Click **Create**.
    4. Specify the program attributes:
       1. In the **Title** field, enter a title of your program, for example, **Process invoices**.
       2. In the **Type** field, choose **Executable Program**.
    7. Click Save.
    8. Save the program as a Local Object.
    9. In the ABAP Editor, add the [linked code](zr_qs_process_documents.prog.abap):
    10. Replace the following:
        * DEMO_DOC_PROCESSING: the client key name.
        * PROJECT_ID: the ID of the Google Cloud project.
        * LOCATION_ID: the processor's location.
        * PROCESSOR_ID: the ID of the processor.
        * SOURCE_BUCKET_URI: the URI of the Cloud Storage bucket folder where source documents are kept for processing.
        * TARGET_BUCKET_URI: the URI of the Cloud Storage bucket where the processed document (JSON file) would be stored.

2. Run your application in SE38. If successful, the following output displays:
3. To validate the results, follow these steps:
    * In the Google Cloud console, go to Cloud Storage **Buckets** page.
    * Open the target bucket. The processed document is stored in the form of a JSON file.  