---
title: COBVALU explanation
---
# introduction

This document explains the design and implementation of a COBOL subprogram that calculates the present value of a series of cash flows. It answers these main questions:

1. What is the purpose of the program and its input/output structure?
2. How does the program parse and convert input data?
3. How are cash flows collected and processed?
4. How is the present value calculated and presented?

# purpose and input/output structure

The program is a simple subprogram named COBVALU that calculates the present value for a series of cash flows. It takes an interest rate, the number of periods, and a list of cash flow amounts as input. The output is the present value of those cash flows formatted for display.

Input and output data are handled through working-storage variables and linkage parameters. The program uses character fields to receive raw input strings and numeric fields for calculations. The output is formatted into a display string and a numeric payment amount.

<SwmSnippet path="/COBOL/COBVALU.cob" line="1">

---

This setup allows the program to be called with parameters and return both a status and the calculated value in a human-readable format.

```
      **********************************************************
      * COBVALU                                                *
      *                                                        *
      * A simple subprogram that calculates present value      *
      * for a series of cash flows.                            *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBVALU.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CHAR-DATA.
           05  INPUT-1           PIC X(10).
           05  PAYMENT-OUT       PIC $$$$,$$$,$$9.99 USAGE DISPLAY.
           05  INTEREST-IN       PIC X(5).
           05  NO-OF-PERIODS-IN  PIC X(3).
           05  INPUT-BUFFER      PIC X(10) VALUE "5069837544".
           05  BUFFER-ARRAY   REDEFINES INPUT-BUFFER
                              OCCURS 5 TIMES
                                 PIC XX.
           05  OUTPUT-LINE       PIC X(79).
       01 NUM-DATA.
           05  PAYMENT           PIC S9(9)V99 USAGE COMP.
           05  INTEREST          PIC S9(3)V99 USAGE COMP.
           05  COUNTER           PIC 99 USAGE COMP.
           05  NO-OF-PERIODS     PIC 99 USAGE COMP.
           05  VALUE-AMOUNT   OCCURS 99 PIC S9(7)V99 COMP.
       LINKAGE SECTION.
       01  PARM-1.
```

---

</SwmSnippet>

# parsing and converting input data

The program starts by initializing a feedback flag to "NO" to indicate processing status. It then moves a hardcoded input string containing the interest rate and number of periods into a buffer.

The input string is parsed using the UNSTRING statement, which splits the string by spaces or commas into separate fields for interest rate and number of periods. These fields are still character strings at this point.

<SwmSnippet path="/COBOL/COBVALU.cob" line="31">

---

Next, the program converts these character fields into numeric values using the NUMVAL function. This conversion is necessary because the calculations require numeric types.

```
           05  CALL-FEEDBACK  PIC XX.
       PROCEDURE DIVISION USING PARM-1.
           MOVE "NO" TO CALL-FEEDBACK.
           MOVE ".12 5 " TO INPUT-1.
           UNSTRING INPUT-1 DELIMITED BY "," OR ALL " "                 
            INTO INTEREST-IN NO-OF-PERIODS-IN.
      * Convert to numeric values
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).            
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Get cash flows
           PERFORM GET-AMOUNTS VARYING COUNTER FROM 1 BY 1 UNTIL
             COUNTER IS GREATER THAN NO-OF-PERIODS.
      * Calculate present value
           COMPUTE PAYMENT =
               FUNCTION PRESENT-VALUE(INTEREST VALUE-AMOUNT(ALL) ).    
      * Make it presentable
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBVALU:_Present_value_for_rate_of_"
                  INTEREST-IN "_given_amounts_"
                  BUFFER-ARRAY (1) ",_"
                  BUFFER-ARRAY (2) ",_"
                  BUFFER-ARRAY (3) ",_"
                  BUFFER-ARRAY (4) ",_"
                  BUFFER-ARRAY (5) "_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.
      *
      * Get cash flows for each period
      *
       GET-AMOUNTS.
           MOVE BUFFER-ARRAY (COUNTER) TO INPUT-1.
           COMPUTE VALUE-AMOUNT (COUNTER) = FUNCTION NUMVAL(INPUT-1).
```

---

</SwmSnippet>

# collecting and processing cash flows

Cash flow amounts are stored in an array of numeric values. The program uses a loop controlled by the number of periods to collect each cash flow.

For each period, it moves a two-character substring from a buffer array into an input field, then converts that substring to a numeric value using NUMVAL. This approach assumes the cash flows are encoded as fixed-length strings in the buffer.

<SwmSnippet path="/COBOL/COBVALU.cob" line="37">

---

This method efficiently extracts and converts multiple cash flow amounts from a single input buffer, preparing them for the present value calculation.

```
      * Convert to numeric values
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).            
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Get cash flows
           PERFORM GET-AMOUNTS VARYING COUNTER FROM 1 BY 1 UNTIL
             COUNTER IS GREATER THAN NO-OF-PERIODS.
      * Calculate present value
           COMPUTE PAYMENT =
               FUNCTION PRESENT-VALUE(INTEREST VALUE-AMOUNT(ALL) ).    
      * Make it presentable
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBVALU:_Present_value_for_rate_of_"
                  INTEREST-IN "_given_amounts_"
                  BUFFER-ARRAY (1) ",_"
                  BUFFER-ARRAY (2) ",_"
                  BUFFER-ARRAY (3) ",_"
                  BUFFER-ARRAY (4) ",_"
                  BUFFER-ARRAY (5) "_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.
      *
      * Get cash flows for each period
      *
       GET-AMOUNTS.
           MOVE BUFFER-ARRAY (COUNTER) TO INPUT-1.
           COMPUTE VALUE-AMOUNT (COUNTER) = FUNCTION NUMVAL(INPUT-1).
```

---

</SwmSnippet>

# calculating and presenting the present value

The core calculation uses a built-in or custom <SwmToken path="/COBOL/COBVALU.cob" pos="45:3:5" line-data="               FUNCTION PRESENT-VALUE(INTEREST VALUE-AMOUNT(ALL) ).    ">`PRESENT-VALUE`</SwmToken> function, which takes the interest rate and the array of cash flows as arguments. This function returns the present value of the cash flows discounted by the interest rate.

The result is stored in a numeric payment field and then moved to a display field formatted with currency symbols and decimal places.

Finally, the program constructs a descriptive output string that includes the interest rate, the individual cash flow amounts, and the calculated present value. It replaces underscores with spaces for readability and displays the output line along with the payment amount.

<SwmSnippet path="/COBOL/COBVALU.cob" line="37">

---

The feedback flag is set to "OK" to indicate successful completion before the program returns control to the caller.

```
      * Convert to numeric values
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).            
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Get cash flows
           PERFORM GET-AMOUNTS VARYING COUNTER FROM 1 BY 1 UNTIL
             COUNTER IS GREATER THAN NO-OF-PERIODS.
      * Calculate present value
           COMPUTE PAYMENT =
               FUNCTION PRESENT-VALUE(INTEREST VALUE-AMOUNT(ALL) ).    
      * Make it presentable
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBVALU:_Present_value_for_rate_of_"
                  INTEREST-IN "_given_amounts_"
                  BUFFER-ARRAY (1) ",_"
                  BUFFER-ARRAY (2) ",_"
                  BUFFER-ARRAY (3) ",_"
                  BUFFER-ARRAY (4) ",_"
                  BUFFER-ARRAY (5) "_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.
      *
      * Get cash flows for each period
      *
       GET-AMOUNTS.
           MOVE BUFFER-ARRAY (COUNTER) TO INPUT-1.
           COMPUTE VALUE-AMOUNT (COUNTER) = FUNCTION NUMVAL(INPUT-1).
```

---

</SwmSnippet>

# summary

This COBOL subprogram is designed to take encoded input strings for interest rate, number of periods, and cash flows, convert them to numeric values, calculate the present value using a dedicated function, and format the results for display. The design choices focus on handling fixed-format input efficiently and producing clear output for integration with other systems.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
