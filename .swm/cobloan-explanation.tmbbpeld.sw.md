---
title: COBLOAN explanation
---
# introduction

This document explains the design and implementation of a COBOL subprogram that calculates the payment amount for a loan. It covers:

1. How the program receives and processes input data.
2. The conversion of input strings to numeric values for calculation.
3. The calculation of the loan payment using an annuity function.
4. Formatting and outputting the result in a readable form.

# program overview and input handling

The program is a simple subprogram named COBLOAN designed to compute loan repayment amounts. It accepts input parameters as a single string containing the loan amount, interest rate, and number of periods. This input is then parsed into separate fields for processing.

<SwmSnippet path="/COBOL/COBLOAN.cob" line="31">

---

The input parsing uses the UNSTRING statement to split the input string by spaces into individual components representing the loan amount, interest rate, and number of periods. This approach keeps the input handling straightforward and contained within the program.

```
             INTO LOAN-AMOUNT-IN INTEREST-IN NO-OF-PERIODS-IN.
      * Convert to numeric values
           COMPUTE LOAN-AMOUNT = FUNCTION NUMVAL(LOAN-AMOUNT-IN).
```

---

</SwmSnippet>

# converting input to numeric values

After parsing, the program converts the string representations of the loan amount, interest rate, and number of periods into numeric values using the FUNCTION NUMVAL. This conversion is necessary because the subsequent calculations require numeric operands.

<SwmSnippet path="/COBOL/COBLOAN.cob" line="34">

---

Using NUMVAL ensures that the program can handle inputs with decimal points and convert them accurately into internal numeric formats suitable for arithmetic operations.

```
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Calculate annuity amount required
           COMPUTE PAYMENT = LOAN-AMOUNT *
               FUNCTION ANNUITY((INTEREST / 12 ) NO-OF-PERIODS).
```

---

</SwmSnippet>

# calculating the loan payment

The core calculation uses the annuity formula to determine the periodic payment amount. The interest rate is divided by 12 to convert it to a monthly rate, and the FUNCTION ANNUITY is applied with the monthly interest and number of periods.

<SwmSnippet path="/COBOL/COBLOAN.cob" line="34">

---

This calculation directly implements the financial formula for loan repayments, making the program useful for determining fixed monthly payments on loans.

```
           COMPUTE INTEREST = FUNCTION NUMVAL(INTEREST-IN).
           COMPUTE NO-OF-PERIODS = FUNCTION NUMVAL(NO-OF-PERIODS-IN).
      * Calculate annuity amount required
           COMPUTE PAYMENT = LOAN-AMOUNT *
               FUNCTION ANNUITY((INTEREST / 12 ) NO-OF-PERIODS).
```

---

</SwmSnippet>

# formatting and output

Once the payment amount is calculated, the program formats the output to be human-readable. It moves the numeric payment into a display field with currency formatting, constructs a descriptive output line by concatenating strings and input values, and replaces underscores with spaces for readability.

<SwmSnippet path="/COBOL/COBLOAN.cob" line="39">

---

Finally, it displays the formatted output line along with the payment amount and signals successful completion by setting a feedback flag.

```
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

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
