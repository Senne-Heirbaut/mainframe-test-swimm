---
title: END variable change impact
---
# Introduction

This document explains how changes to the variable "END" affect the COBOL program in <SwmPath>[COBOL/COBCALC.cob](/COBOL/COBCALC.cob)</SwmPath>. We will answer:

1. What is the role of the variable "END" in the program?
2. How is "END" defined and used?
3. Which paragraphs depend on "END" and will be impacted if it changes?

# role and definition of "END"

<SwmSnippet path="/COBOL/COBCALC.cob" line="23">

---

The variable "END" is defined as a 10-character filler with the initial value "END". This means it is a fixed string used as a marker or flag within the program. Since it is a filler, it is not directly referenced by name in the code but its value can be used in comparisons or control flow.

```
               10  FILLER        PIC X(10)  VALUE "END".
```

---

</SwmSnippet>

# paragraphs affected by changes to "END"

Because "END" is a filler with a fixed value, any paragraph that relies on this value for logic or flow control will be affected if "END" changes. For example, if the program uses "END" as a sentinel to terminate loops or conditionally branch, those paragraphs will behave differently.

To identify the exact paragraphs, you need to search the program for references to the string "END" or logic that compares variables to this value. Those paragraphs will be impacted by changing the value or definition of "END".

# summary

- "END" is a fixed string filler with value "END".
- It acts as a marker or flag in the program.
- Paragraphs that check for or rely on this value will be affected if "END" changes.
- To find all affected paragraphs, look for code that compares to or uses the "END" value.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
