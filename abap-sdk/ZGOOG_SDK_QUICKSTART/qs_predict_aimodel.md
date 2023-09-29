# Predict using Vertex AI Model

This quickstart shows you how you can perform an online prediction for a text prompt using [Vertex AI API v1](https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.publishers.models/predict). This quickstart uses the pre-trained text-bison model to extract function call input parameters from an email.   

---

## Before you begin

Before you run this quickstart, make sure that you or your administrators have completed the following prerequisites:

* You have a Google Cloud account and project.
* Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).
* The ABAP SDK for Google Cloud is installed and configured. [See how to install and configure the ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config).
* Authentication to access Google Cloud APIs is set up. [See how to set up authentication](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/authentication).
* Make sure the Vertex AI API in enabled in your Google Cloud project.
* [Go to API library](https://console.cloud.google.com/project/_/apis/library/aiplatform.googleapis.com)
* Set up one of the [regional endpoints](https://cloud.google.com/vertex-ai/docs/reference/rest#service-endpoint) as an [RFC destination](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config#create_rfc_destinations) in ABAP SDK Configuration. This is because Vertex AI currently does not support global endpoint.  

---

## Create a program to perform prediction using Vertex AI Model
-   Create a program in SE38 and paste the [linked](zr_qs_predict_aimodel.prog.abap) code, which extracts function call input parameters from an email. 

-   Note: The Client key used in the below program is `DEMO_AIPLATFORM` which will be used by the SDK to connect to the API.

-   Execute the program. If successful, you should see the below output
 
```
'json#{#  "function": "Z_ORDER_DEMO",#  "parameters": {#    "IVendor": "Pepsi",#    "IItem": "Pepsi 0 Sugar",#    "IBoxqty": "5"#  }#}#'
```
  
  
