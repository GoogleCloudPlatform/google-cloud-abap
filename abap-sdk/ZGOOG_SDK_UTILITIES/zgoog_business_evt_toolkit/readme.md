# Business Eventing Toolkit for SAP Utilities

## Business Eventing Toolkit

This toolkit facilitates the integration between SAP business events and Google Cloud, enabling seamless event publishing from SAP systems.

### Overview

The primary goal of this toolkit is to simplify the process of capturing business events triggered within the SAP landscape and publishing them reliably to Google Cloud. By doing so, it enables:

* **Real-time Integration:** React to business moments in SAP almost instantly within Google Cloud services.
* **Decoupling:** Loosely couple SAP processes from downstream consumers, enhancing system resilience and flexibility.
* **Extensibility:** Build event-driven architectures where various Google Cloud services (like Cloud Functions, Cloud Run, Pub/Sub, FCM and Integration Connectors) or third-party applications can subscribe to and react to SAP events.

The toolkit ensures that events are published in a standardized format, adhering to the **CloudEvents specification (v1.0)**. This promotes interoperability and simplifies event consumption across different platforms and services.


### RAP Event Handler Generator

A key component of this toolkit is the **RAP Event Handler Generator** (Program: `ZGOOG_R_GEN_RAP_EVTHANDLER`).

#### Purpose

This tool automates the creation of the necessary ABAP OO Class required to handle specific RAP business entity events (e.g., `CREATED`, `UPDATED`, `DELETED`).

#### How to Use

1.  **Run the Generator Program:** Execute the ABAP program `ZGOOG_R_GEN_RAP_EVTHANDLER`.
2.  **Provide Details:**
    * Specify the target **RAP Entity Name** (e.g., `R_PRODUCTTP`).
    * Enter the specific **Entity Event Name** you want to capture (e.g., `CREATED`).
    * Define an **Event Key** (this key is used by the publisher to identify the event configuration, like the target Pub/Sub topic).
    * Provide necessary **Object Directory Data** (Package, Request/Task).
    * Specify the desired **Generated Class Name**.

![Alt text](images/program_selection_screen_rap.png)

3.  **Execute:** Run the program.

#### Outcome

The generator creates an ABAP class containing:
* A local handler class inheriting from `cl_abap_behavior_event_handler`.
* A method implementation for the specified RAP entity event.
* Pre-written code within the event handler method to:
    * Serialize the event data using `/goog/cl_json=>serialize`.
    * Publish the event to Google Cloud Pub/Sub using `/goog/cl_event_publisher=>publish_event` with the specified `event_key`.

This generated class can then be activated and used within the RAP Business Object's Behavior Definition to automatically publish events when the corresponding actions occur in SAP.
