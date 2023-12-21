# MSc_Thesis_UncertaintyViz
Code for MSc Thesis on visualizing uncertainty in climate variable forecasts.

# Running the RShiny app
Please note that the app is dependent on the package Vizumap, which has to be installed from GitHib as follows:
```
remotes::install_github(repo = "lydialucchesi/Vizumap", build_vignettes = TRUE, force = TRUE)
```
This package in turn depends on two retired R packages, `rgeos` and `maptools`, which can be installed from the CRAN archive as follows:
```
devtools::install_github("https://github.com/cran/rgeos/tree/master")
devtools::install_github("https://github.com/cran/maptools/tree/master")
```
Once these packages have been installed (and the others, which can be installed from CRAN in the usual manner), the Shiny app can be run locally by running `app.R` in the folder `R/app`. Either run from the `main` branch, which computes everything on the fly and will take quite a few minutes to load, or run from the `deploy` branch to use the pre-computed results and view the app in a few seconds.
