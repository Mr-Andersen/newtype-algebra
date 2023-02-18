## Algebraic structures with newtype

Goal: tie implementations of `(+)`, `(*)` to `(<>)` and `(-)`, `(/)` to combination of `(<>)` and `inverse`.

- `Data.Group` - intermediate module that defines `inverse`.
- `Data.Semiring` - re-uses 2 `Monoid` instances to get `zero`, `(+)`, `one`, `(*)`.
- `Data.Ring` and `Data.Field` - adds `Group` requirements on top of `Semiring` to get `-` and `/`.
