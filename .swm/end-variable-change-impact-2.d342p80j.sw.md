---
title: END variable change impact 2
---
# Introduction

This document explains the impact of changing the variable named <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> in the COBOL program located at <SwmPath>[COBOL/COBCALC.cob](/COBOL/COBCALC.cob)</SwmPath>. We will answer these questions:

1. Where is the <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> variable defined?
2. Which parts of the program use or depend on <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken>?
3. How will changing <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> affect those parts?

# variable definition and role

<SwmSnippet path="/COBOL/COBCALC.cob" line="23">

---

The variable <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> is defined as a 10-character filler field with the literal value <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> in the data division of the COBOL program. This means it is a fixed string constant embedded in the program's data layout.

```
               10  FILLER        PIC X(10)  VALUE "END".
```

---

</SwmSnippet>

# impact of changing the variable

Since <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> is a filler field with a fixed value, any code that relies on this exact string value will be affected if the value changes. This includes:

- Any paragraph or procedure that compares data against the string <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken>.
- Any logic that uses this field as a marker or delimiter.

Because the snippet only shows the definition, you need to search the program for references to this filler field or the literal <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> string. Those paragraphs will need to be updated to reflect the new value or handle the change accordingly.

# summary

Changing the <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> variable affects all paragraphs that use it as a string constant or marker. You must identify those paragraphs by searching for references to <SwmToken path="/COBOL/COBCALC.cob" pos="23:15:15" line-data="               10  FILLER        PIC X(10)  VALUE &quot;END&quot;.">`END`</SwmToken> or this filler field in the program and update their logic to match the new value. This ensures the program's behavior remains consistent after the change.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBbWFpbmZyYW1lLXRlc3Qtc3dpbW0lM0ElM0FTZW5uZS1IZWlyYmF1dA==" repo-name="mainframe-test-swimm"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
