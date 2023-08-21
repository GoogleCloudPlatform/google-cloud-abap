# Publish Messages to Google Cloud Pub/Sub

This quickstart shows you how to create a program that publishes a "Hello World!" message to a Pub/Sub topic by using the [Pub/Sub API](https://cloud.google.com/pubsub/docs/reference/service_apis_overview).

---

## Before you begin

Before you run this quickstart, make sure that you or your administrators have completed the following prerequisites:

* You have a Google Cloud account and project.
* Billing is enabled for your project. [See how to confirm that billing is enabled for your project](https://cloud.google.com/billing/docs/how-to/verify-billing-enabled).
* The ABAP SDK for Google Cloud is installed and configured. [See how to install and configure the ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/install-config).
* Authentication to access Google Cloud APIs is set up. [See how to set up authentication](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/authentication).
* Grant the service account the IAM role **roles/pubsub.publisher**.
* Make sure the Pub/Sub API in enabled in your Google Cloud project.
* [Go to API library](https://console.cloud.google.com/project/_/apis/library/pubsub.googleapis.com?_ga=2.34774433.1647484855.1692595078-539814502.1692344606&_gac=1.161871054.1692595451.Cj0KCQjwrfymBhCTARIsADXTabkYt1JrKGWwFS3LWRs7hwDEuej5fiIH68_Z1QJXoaYOJHZy3QNvL5caAnesEALw_wcB)
* Create a Pub/Sub topic **SAMPLE_TOPIC_01** and add a pull subscription **SAMPLE_SUB_TOPIC_01** to the same. For more information, see [Create a topic](https://cloud.google.com/pubsub/docs/create-topic#create_a_topic) and [Create a subscription](https://cloud.google.com/pubsub/docs/create-subscription#create_subscriptions).

---

## Create a program to publish messages to Google Cloud
1. In the SAP system, create an executable program in your custom namespace (for example, Z or Y) by using transaction **SE38**.
    1. In the SAP GUI, enter transaction code **SE38**.
    2. In the Program field, enter a name of your program, for example, **ZDEMO_PUBSUB**.
    3. Click **Create**.
    4. Specify the program attributes:
       1. In the **Title** field, enter a title of your program, for example, **Publish messages to a Pub/Sub topic**.
       2. In the **Type** field, choose **Executable Program**.
    7. Click Save.
    8. Save the program as a Local Object.
    9. In the ABAP Editor, add the [linked code](zr_qs_pubish_messages.prog.abap):
    10. Replace **CLIENT_KEY** with the client key name.

2. Run your application in SE38. If successful.
3. To validate the results, follow these steps:
    a. In the Google Cloud console, go to **Pub/Sub**.
    b. Select the subscription **SAMPLE_SUB_TOPIC_01** and go to the Messages tab.
    c. Use the PULL feature to check whether the **"Hello World!"** message has been published to the topic.
  
  