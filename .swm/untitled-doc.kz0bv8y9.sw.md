---
title: Untitled doc
---
# Introduction

This document explains how the COBOL program processes financial function requests using intrinsic functions and subprogram calls. We will cover:

1. How the program initializes and prepares input data.
2. How it processes commands until termination.
3. How it handles different financial calculations via subprogram calls.
4. How it manages input case sensitivity and error handling.

# program initialization and input setup

<SwmSnippet path="/COBOL/COBCALC.cobol" line="28">

---

The program starts by displaying a message indicating it has begun execution. It initializes a pointer to the input buffer and clears the input field to prepare for processing commands. This setup ensures the program is ready to sequentially process predefined inputs stored in a buffer rather than relying on interactive user input.

```
       PROCEDURE DIVISION.
           DISPLAY "CALC Begins." UPON CONSOLE.
           MOVE 1 TO BUFFER-PTR.
           MOVE SPACES TO INPUT-1.
```

---

</SwmSnippet>

# processing commands until termination

<SwmSnippet path="/COBOL/COBCALC.cobol" line="32">

---

The main processing loop repeatedly accepts input from the buffer and processes it until the "END" command is encountered. This loop structure allows the program to handle multiple commands in sequence without manual intervention, terminating cleanly when the "END" keyword is reached.

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

# input acceptance and command evaluation

Input is accepted from a buffer array indexed by a pointer. After each input is read, the pointer increments to the next buffer element. The program normalizes input to uppercase to handle case-insensitive commands. It then evaluates the input against known commands: "END", "LOAN", and "PVALUE". If the input does not match any of these, it displays an invalid input message.

<SwmSnippet path="/COBOL/COBCALC.cobol" line="39">

---

This approach centralizes input handling and command dispatch, making it easy to extend or modify supported commands.

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
```

---

</SwmSnippet>

# invoking financial calculation subprograms

For recognized commands, the program calls corresponding subprograms: "COBLOAN" for loan calculations and "COBVALU" for present value calculations. Each call passes a feedback parameter to receive status. If the subprogram does not return "OK", the program displays an error message indicating the call was unsuccessful.

<SwmSnippet path="/COBOL/COBCALC.cobol" line="39">

---

This design delegates complex financial computations to specialized subprograms, keeping the main program focused on input handling and flow control.

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

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
