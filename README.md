# Interactive Bayesian Statistics Visualization

An interactive R Shiny web application designed to visualize Bayesian concepts, specifically how the prior and likelihood distributions affect the posterior

## Live Demo
You can interact with the live application here: 
[https://ivana-barnes.shinyapps.io/bayesian-viz/](https://ivana-barnes.shinyapps.io/bayesian-viz/)

## How to Run Locally

1. Clone or download this GitHub repository to your computer.
2. Open the `bayesian-viz.Rproj` file in RStudio (this automatically sets your working directory to the correct folder).
3. Open an R console and execute:

```R
install.packages(c("shiny", "rsconnect"))
shiny::runApp()
```

## Description of the Visualization

A visualization of how the prior and likelihood distributions impact the posterior distribution in a Bayesian context. The app simulates some coin-flip data given the input of number of flips and the true probability of heads, and then for a chosen prior it plots the prior, likelihood, and posterior distributions on the same plot. There are three preset choices of priors. This allows people to visualize the relative influence of the likelihood and the prior depending on what the prior is, how reasonable/correct the prior is, and the amount of data (number of flips).

Note that because these distributions are on vastly different scales, I scaled the densities (y-axis) such that the maximum density = 1 for all distributions. So the plot just represents the shapes of the distributions, not their heights. 

