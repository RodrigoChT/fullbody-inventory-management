# FullBody Inventory Management System

The **FullBody Inventory Management System** is a Shiny app that was created for the *FullBody Sportswear* textile company to help them manage many aspects of their business. The features included have been specifically designed to fit their needs, but many would be useful to similar companies.

## Setup
The setup entails importing separate databases for:

* Stores
* Clients
* Providers
* Types of products
* Products (along with their price schemes)
* Inventory spanning multiple stores and possibly providers 

After the initialization of the system all of these databases can be modified and new items can be created through the app in a secure manner, meaning that the data types and formats are checked and fixed before modifying the databases.

## Features

### Operations
The app is designed around performing various *operations*. These *operations* have a series of checks and are carried out in such a way that many human errors that are common when managing this type of data on spreadsheets are completely impossible to commit. The operations included in the current version of the app are:

* Selling garments
* Issuing credit notes
* Reserving products for specific clients
* Receiving or returning products to providers
* Moving inventory among stores
* Modifying the stock of products for any other particular reason
* Reverting a previous operation

Each of these *operations* have a set of data fields that the user should fill out by either selecting an option from a dropdown list or by typing the appropiate information. These fields could be the date of the operation, the store involved in it, the client, the reason for returning inventory to the provider, among many others.

### Reporting and visualization
The app also serves as a reporting and visualization tool. All the databases in the **Setup** section as well as a detalided **Inventory** and a **Movements** table that contains all the performed *operations* are visible inside the app. Furthermore, all tables are searchable and can be downloaded as a csv, excel, or pdf file.

Finally, besides being registered in the **Movements** table, all *operations* generate a pdf document with all their relevant details. See the **Reportes** directory for an example of such a document.

---

### Notes on the repository

* The *Demo* directory contains example images of some of the aspects described in this document.
* The files in the **Data** directory have been removed to safeguard the privacy of *FullBody Sportswear*.
