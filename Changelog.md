### 0.5.0.1
API changes
 * The `(#)` function had its type generalized from `a -> (a -> Canvas b) -> Canvas b` to `a -> (a -> b) -> b`. This allows it to be used with font length units.

API additions
 * A new ADT for `Font`s has been added in `Graphics.Blank.Font` that can be used in place of `Text`. For example, `"30pt Calibri"` is equivalent to `(defFont "Calibri") { fontSize = 30 # pt }`.
 * A generalized `font` function of type `CanvasFont canvasFont => canvasFont -> Canvas ()` was added to `Graphics.Blank.Font` that can accept a `Text` or `Font` argument. The `font` function in `Graphics.Blank` remains of type `Text -> Canvas ()`.

## 0.5

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

