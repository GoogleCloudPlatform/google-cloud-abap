# Create Table in BigQuery

This sample utility can be used to create the table definition in BigQuery for tables that are configured in BigQuery Connector for SAP. 

---

## Background

Before replicating the data to BigQuery, the connector automatically creates or updates table definition in BigQuery using the definition of the table in source. However, when the table is empty in source system, SAP SLT does not trigger the connector code. Hence, the table definition for empty tables would be missing in BigQuery. This can impact scenarios where you want to join these tables with other tables in BigQuery. 

## Prerequisites 
 -  You have [set up and configured](https://cloud.google.com/solutions/sap/docs/bq-connector/latest/install-config) BigQuery Connector for SAP in your SAP SLT Landscape
 
-   You have maintained relavent tables in /GOOG/BQ_TABLE using Tcode /GOOG/SLT_SETTINGS

## Solution
-   Create a program in SE38 and paste the [linked](zr_util_create_bq_table.prog.abap) code, which calls various methods of BigQuery Connector BadI Implementation Class 

-   Note: Apply additional selection, validation and error handling logic as required

-   Execute the program with suitable input parameters

-   If successful, the table definitions would be created in BigQuery

