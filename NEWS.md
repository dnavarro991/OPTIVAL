# OPTIVAL 1.0.0

## Initial Release (2025-01-08)

This is the first public release of OPTIVAL.

### Features

* **Core Functionality**
  * Model-based optimal test validity analysis
  * Bootstrap procedures for empirical validation
  * Incremental validity analysis
  * Optimal item subset selection using hybrid method
  
* **Statistical Methods**
  * Confirmatory Factor Analysis (CFA) calibration
  * Linear factor-analytic model for continuous items
  * Extended congeneric measurement model
  * Model-predicted validity curves
  
* **User Interface**
  * Interactive Shiny web application
  * File upload for data matrices and precalibrated loadings
  * Real-time progress indicators
  * Three interactive plots (plotly):
    - Item validity scatter plot
    - Incremental validity curve
    - Test validity curves (observed vs expected)
  * Comprehensive summary statistics
  * Downloadable results table
  
* **R Functions**
  * `OPTIVAL()`: Main analysis function
  * `run_optival()`: Launch Shiny application
  * Internal functions for bootstrap and validity calculations
  
* **Documentation**
  * Complete roxygen2 documentation for all functions
  * Comprehensive README with examples
  * Usage vignettes and FAQ

### References

Pending.