# ==============================================================================
# OPTIVAL SHINY APPLICATION - MAIN APP FILE
# ==============================================================================
# This file loads the UI and Server components and launches the application
# ==============================================================================

# Load required libraries
library(shiny)
library(waiter)
library(DT)
library(plotly)
library(lavaan)

# Source UI and Server components
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the application
shinyApp(ui = ui, server = server)
