---
title: ACCEPT-INPUT explanation
---
# Introduction

This document explains the implementation of the ACCEPT-INPUT section in <SwmPath>[COBOL/COBCALC.cob](/COBOL/COBCALC.cob)</SwmPath>. It covers:

1. What ACCEPT-INPUT does with the input buffer.
2. How input case sensitivity is handled.
3. How different commands are processed.

# handling input buffer and pointer increment

The first step in ACCEPT-INPUT is to move the current character from BUFFER-ARRAY at position BUFFER-PTR into the variable INPUT-1. This extracts the next input token for processing. Immediately after, BUFFER-PTR is incremented by 1 to point to the next character for future input reads. This approach allows sequential processing of input characters without losing track of the current position.

# case normalization and command evaluation

To handle inputs regardless of case, the code converts INPUT-1 to uppercase before evaluation. This ensures commands like "end", "END", or "End" are treated identically. The EVALUATE statement then compares the uppercase input against known commands: "END", "LOAN", and "PVALUE". This design avoids duplicating logic for different input cases and simplifies command matching.

# command execution and error handling

When the input matches "END", the code explicitly moves "END" back into INPUT-1, likely to standardize the value for downstream logic. For recognized commands "LOAN" and "PVALUE", the corresponding calculation routines are invoked via PERFORM statements. If the input does not match any known command, an error message is displayed indicating invalid input. This ensures that only valid commands trigger processing, and users get immediate feedback on unrecognized inputs.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
