# =============================================================================
# NFL Prediction Model - Test Setup
# =============================================================================
# This file is automatically sourced by testthat before running tests.
# It loads all R/ module files so individual tests don't need source() calls.
# =============================================================================

# Find project root using DESCRIPTION file as anchor
find_project_root <- function() {
  # Try multiple methods to find project root

  # Method 1: rprojroot if available
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    tryCatch({
      return(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")))
    }, error = function(e) NULL)
  }

 # Method 2: Walk up from test directory
  test_dir <- getwd()
  current <- test_dir

  for (i in 1:5) {
    if (file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }
    current <- dirname(current)
  }

  # Method 3: Try common relative paths
  candidates <- c(
    "../..",           # From tests/testthat/
    "../../..",        # From deeper test dirs
    ".",               # Current directory
    normalizePath(".") # Absolute current
  )

  for (path in candidates) {
    full_path <- normalizePath(file.path(path), mustWork = FALSE)
    if (file.exists(file.path(full_path, "DESCRIPTION"))) {
      return(full_path)
    }
  }

  stop("Could not find project root (no DESCRIPTION file found)")
}

# Get project root
PROJECT_ROOT <- find_project_root()

# Helper to source R/ modules safely
source_module <- function(module_name) {
  path <- file.path(PROJECT_ROOT, "R", paste0(module_name, ".R"))
  if (file.exists(path)) {
    source(path, local = FALSE)
    message(sprintf("  Loaded: R/%s.R", module_name))
  } else {
    warning(sprintf("Module not found: R/%s.R", module_name))
  }
}

# Load config.R first (defines all global parameters)
config_path <- file.path(PROJECT_ROOT, "config.R")
if (file.exists(config_path)) {
  message("Loading config from: ", config_path)
  source(config_path, local = FALSE)
  message("  Loaded: config.R")
} else {
  warning("config.R not found at: ", config_path)
}

# Load all R/ modules in dependency order
message("Loading R modules from: ", PROJECT_ROOT)

# Core utilities first (no dependencies)
source_module("logging")
source_module("utils")

# Modules that depend on utils
source_module("data_validation")

# Modules that may depend on logging/utils
source_module("playoffs")
source_module("date_resolver")

# Optional modules (may not exist in all versions)
tryCatch({
  source_module("sleeper_api")
}, error = function(e) {
  message("  Skipped: R/sleeper_api.R (optional)")
})

# Load player props configuration (v2.9.0)
props_config_path <- file.path(PROJECT_ROOT, "sports", "nfl", "props", "props_config.R")
if (file.exists(props_config_path)) {
  source(props_config_path, local = FALSE)
  message("  Loaded: sports/nfl/props/props_config.R")
} else {
  message("  Skipped: sports/nfl/props/props_config.R (not found)")
}

message("Test setup complete.\n")

# Export project root for tests that need it
.test_project_root <- PROJECT_ROOT
