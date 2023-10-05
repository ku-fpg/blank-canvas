### 0.7.4 [2023.10.05]
* Support building with `scotty-0.20`.

### 0.7.3
* Allow building with GHC 9.0.

### 0.7.2
* Render Unicode codepoints beyond `0xFFFF` properly.

### 0.7.1
* Remove the `wiki-suite` test suite from `blank-canvas.cabal`, as it was never
  intended to work as a traditional test suite. The functionality of
  `wiki-suite` has moved to a subdirectory of the upstream `blank-canvas`
  repository.

## 0.7
* Strengthen the `Monad` constraint on `readColourName` to `MonadFail`.

#### 0.6.3
* Use `base-compat-batteries`.

### 0.6.2
Additions
 * Add `Semigroup` instance for `Canvas`

### 0.6.1
* Fix building with `aeson-1.2.2.0`.

### 0.6
API changes
 * The `(#)` function had its type generalized from `a -> (a -> Canvas b) -> Canvas b` to `a -> (a -> b) -> b`. This allows it to be used with font length units.
 * Added more type synonyms (`Interval`, `Degrees`, `Radians`, etc.) to more clearly indicate what functions expect constrained values.
 * `showbJS` (formerly `showJS`) and `jsStyle` now return a text `Builder` instead of a `String`. This change was introduced as part of a larger `blank-canvas` refactoring to increase performance. See the `Data.Text.Lazy.Builder` module from the `text` package for more details on how to use `Builder`s.

API additions
 * A new ADT for `Font`s has been added in `Graphics.Blank.Font` that can be used in place of `Text`. For example, `"30pt Calibri"` is equivalent to `(defFont "Calibri") { fontSize = 30 # pt }`.
 * A generalized `font` function of type `CanvasFont canvasFont => canvasFont -> Canvas ()` was added to `Graphics.Blank.Font` that can accept a `Text` or `Font` argument. The `font` function in `Graphics.Blank` remains of type `Text -> Canvas ()`.
 * Added a `cursor` function to change the browser cursor. Also added the `Graphics.Blank.Cursor` module containing a generalized `cursor` function that uses a `Cursor` ADT instead of `Text`.
 * Added `Bounded`, `Enum`, `Eq`, `Ix`, `Ord`, and `Show` instances for more data types
 * Added support for more MIME types via the `mime-types` library

Additions
 * Allowed building with `base-4.8.0.0`

Other
 * Require `scotty` >= 0.10 and `kansas-comet` >= 0.4

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

