
<!-- README.md is generated from README.Rmd. Please edit that file -->

# primer 1.2.0

There are a lot of changes from June 2020, and even since October 2020.
See the new draft of the [Primer of Ecology Using
R](https://hankstevens.github.io/Primer-of-Ecology/) for updated text
and code (31 Dec 2020).

## Added

  - Logistic growth ODEs: now there are three more similar functions,
    ‘dlogistic’, ‘clogistic’, and ‘alogistic’ that all use ‘ode()’ to
    solve them (see help pages for details\!). These refer to discrete,
    continuous, and Allee effect (continuous logsitic).
  - ‘bip\_stability()’ calculates a variety of properties of mutualistic
    and antagonistic bipartite networks.
  - New datasets (‘coneflower’, etc.) for use in an IPM (integral
    projection model) in [Chap. 4 (density-independent
    demography)](https://hankstevens.github.io/Primer-of-Ecology/DID.html#integral-projection).
  - New functions related to generating 1/f environmental noise using
    any arbitrary set of numbers via spectral mimicry (‘spec\_mimic()’,
    ‘one\_over\_f()’, ‘plot\_f()’). I have plans to eventually include
    these in the end of chapter three (density-independent growth) in
    stochatic simulations.

## Removed

  - ‘DemoInfo()’ is no longer available. If you would like that code, I
    can send it to you.
