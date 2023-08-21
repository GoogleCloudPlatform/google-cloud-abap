# Translate texts

This quickstart shows you how to create a program that translates texts from English to German by using the [Cloud Translation API v2](https://cloud.google.com/translate/docs/basic/translating-text).

---

## Before you begin

Before you run this quickstart, make sure that you or your administrators have completed the following prerequisites:

* You have a Google Cloud account and project.
* Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).
* The ABAP SDK for Google Cloud is installed and configured. [See how to install and configure the ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config).
* Authentication to access Google Cloud APIs is set up. [See how to set up authentication](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/authentication).
* Make sure the Cloud Translation API in enabled in your Google Cloud project.
* [Go to API library](https://console.cloud.google.com/project/_/apis/library/translate.googleapis.com?_ga=2.269142897.1647484855.1692595078-539814502.1692344606&_gac=1.153951562.1692595451.Cj0KCQjwrfymBhCTARIsADXTabkYt1JrKGWwFS3LWRs7hwDEuej5fiIH68_Z1QJXoaYOJHZy3QNvL5caAnesEALw_wcB)

---

## Create a program to translate texts
1. In the SAP system, create an executable program in your custom namespace (for example, Z or Y) by using transaction **SE38**.
    1. In the SAP GUI, enter transaction code **SE38**.
    2. In the Program field, enter a name of your program, for example, **ZDEMO_TRANSLATE**.
    3. Click **Create**.
    4. Specify the program attributes:
       1. In the **Title** field, enter a title of your program, for example, **Translate from English to German**.
       2. In the **Type** field, choose **Executable Program**.
    7. Click Save.
    8. Save the program as a Local Object.
    9. In the ABAP Editor, add the [linked code](zr_qs_translate_texts.prog.abap):
    10. Replace **DEMO_TRANSLATE** with the client key name.

2. Run your application in SE38. If successful, the following output displays:

```
'Translation Successful'
'Translated Text is: Die Erde ist der dritte Planet von der Sonne'
```
  
  