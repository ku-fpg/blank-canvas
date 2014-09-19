
API changes
 * Using Double rather than Float
 * Generalized saveRestore to be polymorphic

API additions
 * Added sync for forcing synchronization between the server and client
 * Added argument specific variants of drawImage and putImageData.
 * Added ADTs for RepeatDirection, Alignment, Baseline, LineEnds, Corner (previous used Text)
 * Use of the colour(sic) package (Text is still allowed)

Bug fixes:
 * Fixed unicode escapes in strings

Additions:
 * Allow Images to access client-side filesystem
