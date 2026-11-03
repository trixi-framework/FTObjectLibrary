---
title: News
---

[Back to the main documentation](../index.html)

# News

## November 03, 2026

Fixed memory leaks and re-entry problems in object destruction.
Dictionaries now release their contents when destroyed, and FTStringSet frees its dictionary.
Destructors, release procedures and removeAll are now RECURSIVE, and no longer use initialized
(implicitly SAVEd) pointer locals. These problems could cause leaks, or crashes with some compilers
such as ifx, when containers held other containers. Subclasses should follow the same pattern:
declare the destructor and release procedures RECURSIVE, bind the destructor with FINAL,
and do not initialize pointer locals in them. See "Subclassing FTObject" in the User's Guide.

## July 29, 2026

FTOL now has two versions, as necessary for procedures that need to distinguish between objects declared as TYPE and those declared as CLASS. See the documentation and the tests for examples.

## May 11, 2025

The stringValue() and stringValueForKey() functions now use allocated strings, so the requestedLength argument is no longer necessary. Existing code can continue to use the older versions, but those versions are deprecated and undocumented.