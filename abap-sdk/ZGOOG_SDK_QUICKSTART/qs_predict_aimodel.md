# Predict using Vertex AI Model

This quickstart shows you how to create a program that performs an online prediction for a text prompt using [Vertex AI API v1](https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.publishers.models/predict). This quickstart uses the pre-trained text-bison model published by Google.   

---

## Before you begin

Before you run this quickstart, make sure that you or your administrators have completed the following prerequisites:

* You have a Google Cloud account and project.
* Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).
* The ABAP SDK for Google Cloud is installed and configured. [See how to install and configure the ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config).
* Authentication to access Google Cloud APIs is set up. [See how to set up authentication](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/authentication).
* Make sure the Vertex AI API in enabled in your Google Cloud project.
* [Go to API library](https://console.cloud.google.com/project/_/apis/library/aiplatform.googleapis.com)

---

## Create a program to perform prediction using Vertex AI Model
1. In the SAP system, create an executable program in your custom namespace (for example, Z or Y) by using transaction **SE38**.
    1. In the SAP GUI, enter transaction code **SE38**.
    2. In the Program field, enter a name of your program, for example, **ZR_QS_PREDICT_AIMODEL**.
    3. Click **Create**.
    4. Specify the program attributes:
       1. In the **Title** field, enter a title of your program, for example, **Quickstart: Online prediction using a Google Provided Vertex AI Model**.
       2. In the **Type** field, choose **Executable Program**.
    7. Click Save.
    8. Save the program as a Local Object.
    9. In the ABAP Editor, add the [linked code](zr_qs_predict_aimodel.prog.abap):
    10. Replace **DEMO_AIPLATFORM** with the client key name.

2. Run your application in SE38. If successful, the following output displays:

```
' ABAP (Advanced Business Application Programming) is a high-level programming language that is used to develop business applications for SAP's ERP.....'
```
  
  
