# Setup file: loads all data objects into .GlobalEnv so package functions
# (which search their namespace → .GlobalEnv) can find environment objects
# like Biomass_coefs, IPCC_residue_coefs, Residue_kgC_kgDM_Wo, etc.
library(afsetools)

.setup_loader <- function() {
  # load_general_data() assigns into parent.frame() = this function's env

  load_general_data()
  # Copy all loaded objects to .GlobalEnv for package function access
  for (nm in ls()) {
    assign(nm, get(nm), envir = .GlobalEnv)
  }
}
.setup_loader()
rm(.setup_loader)
