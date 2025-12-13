# renv activation script
# This file is automatically sourced when R starts if .Rprofile exists

local({
  # Check if renv is installed
  if (!requireNamespace("renv", quietly = TRUE)) {
    message("Installing renv...")
    install.packages("renv", repos = "https://cloud.r-project.org")
  }

  # Activate renv project
  source("renv/settings.R")
})
