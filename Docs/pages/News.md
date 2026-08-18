---
title: News
---

[Back to the main documentation](../index.html)

# News

## July 29, 2026

FTOL now has two versions, as necessary for procedures that need to distinguish between objects declared as TYPE and those declared as CLASS. See the documentation and the tests for examples.

## May 11, 2025

The stringValue() and stringValueForKey() functions now use allocated strings, so the requestedLength argument is no longer necessary. Existing code can continue to use the older versions, but those versions are deprecated and undocumented.