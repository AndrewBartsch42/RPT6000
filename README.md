# RPT6000 – Year-to-Date Sales Report (COBOL)

## Overview

**RPT6000** is a COBOL batch program that generates a **Year-to-Date (YTD) Sales Report** from customer and sales representative master files.

The program reads input data, calculates sales changes, and produces a formatted report including:

* Customer-level sales data
* Sales representative totals
* Branch totals
* Grand totals
* Year-over-year comparisons (This YTD vs Last YTD)

---

## Features

* 📄 Formatted multi-page report output
* 📊 Year-to-date vs last-year comparison
* 📈 Change amount and percentage calculations
* 🧾 Hierarchical grouping:

  * Customer
  * Sales Representative
  * Branch
  * Grand Total
* 🕒 Automatic date/time stamping
* 🔁 Handles pagination and headings

---

## Input Files

The program expects the following input files:

### 1. `CUSTMAST`

Customer master file containing:

* Branch number
* Sales representative number
* Customer number and name
* Sales (This YTD and Last YTD)

> Defined via `COPY CUSTMAST`

---

### 2. `SALESMAS`

Sales representative master file:

* Sales rep number
* Sales rep name

> Loaded into an in-memory table (up to 100 entries)

---

## Output File

### `SALESRPT`

A formatted report containing:

* Page headers with date/time
* Customer sales lines
* Sales rep totals
* Branch totals
* Grand totals

---

## Program Flow

1. **Initialization**

   * Opens files
   * Loads sales rep table into memory

2. **Processing Loop**

   * Reads `CUSTMAST` sequentially
   * Detects control breaks:

     * Sales rep change
     * Branch change

3. **Calculations**

   * Computes:

     * Change amount
     * Percentage change
   * Handles edge cases:

     * Division by zero → `"N/A"`
     * Overflow → `"OVRFLW"`

4. **Control Break Logic**

   * Prints totals when:

     * Sales rep changes
     * Branch changes

5. **Finalization**

   * Prints grand totals
   * Closes all files

---

## Key Concepts Used

* COBOL **sequential file processing**
* **OCCURS tables** (Sales rep lookup)
* **Control break processing**
* **Packed decimal arithmetic**
* **Report formatting with fixed-width records**

---

## Report Structure

```
DATE: MM/DD/YYYY                  YEAR-TO-DATE SALES REPORT        PAGE: n
TIME: HH:MM                                             RPT6000

BRANCH  SALESREP   CUSTOMER        THIS YTD   LAST YTD   CHANGE   %
---------------------------------------------------------------------

 01     10  John D   12345  ABC Co     1000.00   900.00   100.00  11.1
 ...

             SALES TOTAL ...
           BRANCH TOTAL ...
           GRAND TOTAL ...
```

---

## Notable Logic

### Change Calculation

```cobol
CHANGE-AMOUNT = THIS-YTD - LAST-YTD
```

### Percentage Calculation

```cobol
CHANGE * 100 / LAST-YTD
```

Special handling:

* `LAST-YTD = 0` → `"N/A"`
* Overflow → `"OVRFLW"`

---

## File Structure

```
RPT6000/
├── RPT6000.cob       # Main COBOL program
├── CUSTMAST copybook
├── SALESMAS copybook
└── README.md
```
---  

