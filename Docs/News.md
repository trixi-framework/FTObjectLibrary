# News

May 11, 2025

The stringValue() and stringValueForKey() functions now use allocated strings, so the requestedLength argument is no longer necessary. Existing code can continue to use the older versions, but those versions are deprecated and undocumented. 