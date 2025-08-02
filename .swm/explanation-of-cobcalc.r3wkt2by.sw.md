---
title: Explanation of COBCALC
---
# Introduction

This document explains the design and implementation of the COBOL program located at <SwmPath>[COBOL/COBCALC.cob](/COBOL/COBCALC.cob)</SwmPath>. The program performs financial calculations by calling external subprograms. We will cover:

1. How input commands are handled and processed.
2. The mechanism for invoking financial calculation subprograms.
3. The approach to handling case-insensitive input and invalid commands.
4. The program flow from start to finish.

# program structure and input setup

<SwmSnippet path="/COBOL/COBCALC.cob" line="1">

---

The program defines a small set of input commands ("LOAN", "PVALUE", "END") stored in a buffer array. It uses a pointer to iterate through these commands sequentially. This setup simulates user input for testing or batch processing without interactive input.

```
      **********************************************************
      * COBCALC                                                *
      *                                                        *
      * A simple program that allows financial functions to    *
      * be performed using intrinsic functions.                *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBCALC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PARM-1.
           05  CALL-FEEDBACK     PIC XX.
       01  FIELDS.
           05  INPUT-1           PIC X(10).
       01  INPUT-BUFFER-FIELDS.
           05  BUFFER-PTR        PIC 9.
           05  BUFFER-DATA.
               10  FILLER        PIC X(10)  VALUE "LOAN".
               10  FILLER        PIC X(10)  VALUE "PVALUE".
               10  FILLER        PIC X(10)  VALUE "pvalue".
               10  FILLER        PIC X(10)  VALUE "END".
           05  BUFFER-ARRAY    REDEFINES BUFFER-DATA
                               OCCURS 4 TIMES
                                 PIC X(10).
```

---

</SwmSnippet>

<SwmSnippet path="/COBOL/COBCALC.cob" line="28">

---

At the start of the procedure division, the program initializes the pointer and clears the input buffer to prepare for processing.

```
       PROCEDURE DIVISION.
           DISPLAY "CALC Begins." UPON CONSOLE.
           MOVE 1 TO BUFFER-PTR.
           MOVE SPACES TO INPUT-1.
```

---

</SwmSnippet>

# input processing loop

<SwmSnippet path="/COBOL/COBCALC.cob" line="32">

---

The main loop repeatedly calls <SwmToken path="/COBOL/COBCALC.cob" pos="33:3:5" line-data="           PERFORM ACCEPT-INPUT UNTIL INPUT-1 EQUAL TO &quot;END&quot;.">`ACCEPT-INPUT`</SwmToken> until the "END" command is encountered. This loop drives the program's operation by processing each input command in order.

```
      * Keep processing data until END requested
           PERFORM ACCEPT-INPUT UNTIL INPUT-1 EQUAL TO "END".
      * END requested
           DISPLAY "CALC Ends." UPON CONSOLE.
           GOBACK.
      * End of program.
```

---

</SwmSnippet>

# input command evaluation and subprogram calls

The <SwmToken path="/COBOL/COBCALC.cob" pos="33:3:5" line-data="           PERFORM ACCEPT-INPUT UNTIL INPUT-1 EQUAL TO &quot;END&quot;.">`ACCEPT-INPUT`</SwmToken> routine reads the current command from the buffer and increments the pointer. It converts the input to uppercase to allow case-insensitive matching. Based on the command, it either calls the appropriate financial calculation subprogram or displays an error for invalid input.

<SwmSnippet path="/COBOL/COBCALC.cob" line="39">

---

The two financial calculations are handled by separate subprograms: "COBLOAN" for loan calculations and "COBVALU" for present value calculations. Each call passes a feedback parameter to receive the status. If the call fails, an error message is displayed.

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

# summary

This program processes a predefined list of financial commands, normalizes input case, and delegates calculations to specialized subprograms. It handles errors by checking call feedback and reporting failures. The design keeps the main program simple and focused on input handling and dispatching, while the calculation logic resides in external modules.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
