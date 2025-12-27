# setup_packages.R
# Check, install, and load required packages for Lake Fazilman project

required_packages <- c(
  "ggplot2",
  "ggpp",
  "ggpubr",
  "ggpmisc",
  "ggrepel",
  "RColorBrewer",
  "patchwork",
  "dplyr",
  "maps",
  "mapproj",
  "gstat",
  "reshape2",
  "ggnewscale",
  "readr",
  "ggthemes",
  "tibble",
  "scales",
  "lubridate",
  "raster",
  "rgdal",
  "renv"
)

install_and_load <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste0("Installing missing package: ", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Run it
install_and_load(required_packages)


message("All required packages are installed and loaded.")
