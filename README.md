# Clinical Sample Size Planner

A Shiny application for sample size planning in clinical trials. Provides regulator-friendly designs with statistical power calculations for bioequivalence studies and early phase clinical trials (work in progress).

## Features

**Bioequivalence Studies**
- Equivalence testing
- Adaptive designs (Potvin B)
- Non-inferiority testing
- Non-superiority testing

**Early Phase Studies**
- Phase I trial planning
- Phase II trial planning

## Requirements

- R (>= 4.0)
- Required packages: shiny, bslib, bsicons, shinyjs, shinyWidgets, PowerTOST

## Installation

```r
# Install required packages
install.packages(c("shiny", "bslib", "bsicons", "shinyjs", "shinyWidgets", "PowerTOST"))
```

## Usage

```r
# Run the application
shiny::runApp()
```

The application provides an interactive interface for:
- Setting study parameters
- Calculating sample sizes
- Generating statistical summaries
- Exporting results

## License

MIT License. See LICENSE file for details.
