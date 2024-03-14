## Conversions:
* **alpha** - just a renaming of variables (nothing bound can become unbound and vice versa), e.g. `(λx.e) == (λy.e)[y/x]` or `(λt.(x(λx.xt)))[y/x] = (λt.(y(λx.xt)))` (notice that the bound `x` is not substituted)
* **beta** - the only computation possible, e.g. `(λx.e)f == e[f/x]` or `(λx.x)(λz.z) = x[(λz.z)/x]`
* **eta** - simplification of terms without change of their effect, e.g. `(λy.λx.yx) == (λyx.yx) == (λy.y)`

## Bracket Conventions
