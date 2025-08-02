---
title: Cross-file explanation
---
# introduction

This document explains the design and implementation of a COBOL program that performs financial calculations by calling external subprograms. We will cover:

1. How the main program processes input commands and delegates tasks.
2. Which external programs the main program uses and their purposes.
3. How the subprograms perform their specific financial calculations and communicate results back.

# main program input processing and control flow

<SwmSnippet path="/COBOL/COBCALC.cob" line="28">

---

The main program (<SwmPath>[COBOL/COBCALC.cob](/COBOL/COBCALC.cob)</SwmPath>) initializes a buffer with predefined commands and processes them sequentially. It loops through the buffer, accepting input commands until it encounters the "END" command. Each command triggers a specific action: either calculating a loan payment or calculating a present value. Invalid commands are reported. This design centralizes command handling and allows easy extension by adding new commands and corresponding actions.

```
       PROCEDURE DIVISION.
           DISPLAY "CALC Begins." UPON CONSOLE.
           MOVE 1 TO BUFFER-PTR.
           MOVE SPACES TO INPUT-1.
      * Keep processing data until END requested
           PERFORM ACCEPT-INPUT UNTIL INPUT-1 EQUAL TO "END".
      * END requested
           DISPLAY "CALC Ends." UPON CONSOLE.
           GOBACK.
      * End of program.

      *
      * Accept input data from buffer
      *
       ACCEPT-INPUT.
           MOVE BUFFER-ARRAY (BUFFER-PTR) TO INPUT-1.
           ADD 1 BUFFER-PTR GIVING BUFFER-PTR.
      * Allow input data to be in UPPER or lower case
           EVALUATE FUNCTION UPPER-CASE(INPUT-1)     CALC1              
             WHEN "END"           
               MOVE "END" TO INPUT-1
             WHEN "LOAN"
               PERFORM CALCULATE-LOAN
             WHEN "PVALUE"
               PERFORM CALCULATE-VALUE
             WHEN OTHER
               DISPLAY "Invalid input: " INPUT-1
           END-EVALUATE.
      *
      * Calculate Loan via CALL to subprogram
      *
       CALCULATE-LOAN.
           CALL "COBLOAN" USING CALL-FEEDBACK.
           IF CALL-FEEDBACK IS NOT EQUAL "OK" THEN
             DISPLAY "Call to COBLOAN Unsuccessful.".
      *
      * Calculate Present Value via CALL to subprogram
      *
       CALCULATE-VALUE.
           CALL "COBVALU" USING CALL-FEEDBACK.
           IF CALL-FEEDBACK IS NOT EQUAL "OK" THEN
```

---

</SwmSnippet>

# external programs used and their purposes

The main program calls two external subprograms: COBLOAN and COBVALU.

- COBLOAN calculates the payment amount for a loan based on principal, interest rate, and number of periods.
- COBVALU calculates the present value of a series of cash flows given an interest rate and number of periods.

<SwmSnippet path="/COBOL/COBCALC.cob" line="39">

---

These subprograms encapsulate the financial logic, keeping the main program focused on input handling and delegation. The main program passes a feedback parameter to each call to verify success or failure.

```
      *
      * Accept input data from buffer
      *
       ACCEPT-INPUT.
           MOVE BUFFER-ARRAY (BUFFER-PTR) TO INPUT-1.
           ADD 1 BUFFER-PTR GIVING BUFFER-PTR.
      * Allow input data to be in UPPER or lower case
           EVALUATE FUNCTION UPPER-CASE(INPUT-1)     CALC1              
             WHEN "END"           
               MOVE "END" TO INPUT-1
             WHEN "LOAN"
               PERFORM CALCULATE-LOAN
             WHEN "PVALUE"
               PERFORM CALCULATE-VALUE
             WHEN OTHER
               DISPLAY "Invalid input: " INPUT-1
           END-EVALUATE.
      *
      * Calculate Loan via CALL to subprogram
      *
       CALCULATE-LOAN.
           CALL "COBLOAN" USING CALL-FEEDBACK.
           IF CALL-FEEDBACK IS NOT EQUAL "OK" THEN
             DISPLAY "Call to COBLOAN Unsuccessful.".
      *
      * Calculate Present Value via CALL to subprogram
      *
       CALCULATE-VALUE.
           CALL "COBVALU" USING CALL-FEEDBACK.
           IF CALL-FEEDBACK IS NOT EQUAL "OK" THEN
             DISPLAY "Call to COBVALU Unsuccessful.".
```

---

</SwmSnippet>

# loan calculation subprogram details

<SwmSnippet path="/COBOL/COBLOAN.cob" line="1">

---

COBLOAN receives input parameters as strings, converts them to numeric types, and computes the loan payment using an annuity function. It formats the output into a readable string and displays the result. The subprogram sets a feedback flag to "OK" on success, which the main program checks to confirm the call succeeded.

```
      **********************************************************
      * COBLOAN                                                *
      *                                                        *
      * A simple subprogram that calculates payment amount     *
      * for a loan.                                            *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBLOAN.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELDS.
           05  INPUT-1           PIC X(26).
           05  PAYMENT           PIC S9(9)V99 USAGE COMP.
           05  PAYMENT-OUT       PIC $$$$,$$$,$$9.99 USAGE DISPLAY.
           05  LOAN-AMOUNT       PIC S9(7)V99 USAGE COMP.
           05  LOAN-AMOUNT-IN    PIC X(16).
           05  INTEREST-IN       PIC X(5).
           05  INTEREST          PIC S9(3)V99 USAGE COMP.
           05  NO-OF-PERIODS-IN  PIC X(3).
           05  NO-OF-PERIODS     PIC 99 USAGE COMP.
           05  OUTPUT-LINE       PIC X(79).
       LINKAGE SECTION.
       01  PARM-1.
           05  CALL-FEEDBACK     PIC XX.
       PROCEDURE DIVISION USING PARM-1.
           MOVE "NO" TO CALL-FEEDBACK.
           MOVE "30000 .09 24 " TO INPUT-1.
           UNSTRING INPUT-1 DELIMITED BY ALL " "
             INTO LOAN-AMOUNT-IN INTEREST-IN NO-OF-PERIODS-IN.
      * Convert to numeric values
           COMPUTE LOAN-AMOUNT = FUNCTION NUMVAL(LOAN-AMOUNT-IN).
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Calculate annuity amount required
           COMPUTE PAYMENT = LOAN-AMOUNT *
               FUNCTION ANNUITY((INTEREST / 12 ) NO-OF-PERIODS).
      * Make it presentable
           MOVE SPACES TO OUTPUT-LINE
           MOVE PAYMENT TO PAYMENT-OUT.
           STRING "COBLOAN:_Repayment_amount_for_a_" NO-OF-PERIODS-IN
                   "_month_loan_of_" LOAN-AMOUNT-IN
                   "_at_" INTEREST-IN "_interest_is:_"
               DELIMITED BY SPACES
               INTO OUTPUT-LINE.
           INSPECT OUTPUT-LINE REPLACING ALL "_" BY SPACES.
           DISPLAY OUTPUT-LINE PAYMENT-OUT.
           MOVE "OK" TO CALL-FEEDBACK.
           GOBACK.
```

---

</SwmSnippet>

# present value calculation subprogram details

<SwmSnippet path="/COBOL/COBVALU.cob" line="1">

---

COBVALU similarly receives input parameters, converts them, and retrieves cash flow amounts from a buffer. It calculates the present value using a built-in function, formats the output string, and displays the result. It also sets the feedback flag to "OK" on success. The subprogram includes a loop to process multiple cash flow amounts, demonstrating handling of variable-length input.

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

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
