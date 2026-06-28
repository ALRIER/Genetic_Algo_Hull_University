# Auxiliary Monte Carlo Validation Suite

## Purpose

This folder contains an auxiliary validation suite for the Monte Carlo simulation engine used by the original 60-80 discovery code.

It is not part of the active GA discovery launcher. It is retained as a reproducibility and audit utility for checking whether the simulation engine behaves as intended before expensive GA training is run.

## Scope

The validation suite checks four classes of evidence:

1. Moment fidelity: generated samples recover expected distributional moments.
2. Contamination fidelity: injected contamination follows the requested rate and scale.
3. Statistical sanity: known textbook behavior is recovered in simple reference cases.
4. Empirical anchoring: synthetic distributional regimes cover relevant empirical shape profiles.

## How to use

Source the main discovery modules first, then source `11_validation_suite.R` or call `run_validation_suite()` programmatically.

This module may perform external data downloads in its empirical anchoring layer. For a fully offline run, use the function options that skip the real-data layer.

## Relationship to discovery

The discovery pipeline can run without this folder. This suite exists to audit the simulation engine, not to select regimes, train GA candidates, or decide winners.
