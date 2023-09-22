# ABAP SDK for Google Cloud 

ABAP SDK for Google Cloud provides client libraries for developers to use ABAP to programmatically access Google Cloud APIs. By encapsulating essential functions as reusable components, these client libraries reduce the amount of code that developers need to write.

The objective of the ABAP SDK for Google Cloud is to make it easier for SAP developers to leverage a consistent development framework and make Google technology stacks more directly available to ABAP coders.  This document outlines the design of a ABAP SDK with these goals in mind: 
* Simplifying the integration between SAP ABAP stack and GCP technologies.
* Increasing the productivity of the SAP developers by encapsulating the complexities of the integration with GCP.
* Formalizing the integration solution between SAP ABAP stack and GCP through the library leveraging agreed blueprinted integration patterns.
* Build the required internal toolsets to enable code generation capabilities for Google Services and establish a robust process for testing the SDK capabilities.

This connector will be applicable for SAP NetWeaver stack >= 7.0 versions.

Here is the [documentation](https://cloud.google.com/solutions/sap/docs/abap-sdk/latest/developer) for reference. 

## About this repository:

This repository contains code samples, art of possible scenarios, demo code, and utilities that can be used to help developers get started with the ABAP SDK for Google Cloud.

To consume the code in the repository, you can use abapGit to pull the code into your SAP environment. When you pull the code, you can choose to pull only the folders that are relevant to you, instead of the entire repository. This is because some of the code samples in the repository are only valid for specific environments (for example, S/4HANA only). The compatibility details are also provided in the corresponding README.md files.

### Repository Structure: 
| Sub-directory             | Description   | 
| ------------------------- |---------------| 
| [ZGOOG_SDK_DOCS_SAMPLES](ZGOOG_SDK_DOCS_SAMPLES) | Demo code to connect Google API’s using the SDK | 
| [ZGOOG_SDK_QUICKSTART](ZGOOG_SDK_QUICKSTART) | Guides intended to get a user rapidly acquainted with using ABAP SDK |
