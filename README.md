
# Lambdagames

Fun little PureScript library that computes free theorems for a given
Haskell type, per [Theorems for
free!](https://www2.cs.sfu.ca/CourseCentral/831/burton/Notes/July14/free.pdf).

I mainly wrote this in order to learn PureScript.

Run with `npx spago run`. Enter a Haskell type annotation and the free
theorem will print out. This library generally follows Haskell syntax,
so lowercase letters are variables and uppercase letters are type
constructors. Any free variables will be implicitly quantified over
the whole expression, so if you input `a -> a`, the type you get is
`forall a. a -> a`. This library is also Unicode-friendly, so `->` can
be written `→` (U+2192), and `forall` can be written `∀` (U+2200).

## Testing

Run our (currently meager) test suite with `npx spago test`.

## Building as a JS Module

Lambdagames can be exported as a JavaScript module with `npx spago
bundle-module --main 'Lambda.Module'`. Adding the `--minify` flag is
strongly recommended for non-debug builds.
