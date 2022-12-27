# SARIF implementation for Haskell

![GitHub](https://img.shields.io/github/license/mbg/sarif)
![Haskell CI](https://github.com/mbg/sarif/actions/workflows/build.yml/badge.svg?branch=main)
![stackage-nightly](https://github.com/mbg/sarif/actions/workflows/stackage-nightly.yml/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/sarif)](https://hackage.haskell.org/package/sarif)

This Haskell library provides types and JSON instances for the [Static Analysis Results Interchange Format (SARIF)](https://sarifweb.azurewebsites.net). Static analysis tools written in Haskell may use this library to export their results which can then be consumed by e.g. [GitHub Code Scanning](https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning).

## References

-   [SARIF website](https://sarifweb.azurewebsites.net)
-   [SARIF specification](https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.pdf)
-   [SARIF tutorials](https://github.com/microsoft/sarif-tutorials/)
