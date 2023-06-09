NEWS
====

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/scientiafelis/sebmsr/issues)
* [The commit log](https://github.com/scientiafelis/sebmsr/commits/master)

# v 0.9.0

* New command to `sebms_sunhour_data()` that make it possible to assign result data to Global env. This is degfault to `FALSE` but is set to `TRUE` when this function is used in the plot functions. This makes it possible to use this object to make a diff plot after making a sunhour plot without having to download the data again.

* Add the year to the `sebms_ggsave()` to differencciate the temp and precip plots from different years.

* Small fixes to the figures. Add margin between text and panel in weather plots, larger linewidth, and smaller points in sunhourplots.

* Some code cleaning and commenting of code.

# v 0.8.1

* Minor changes in error and warning handling.
* Added some comments to code to explain what it do.

# v 0.8.0

* Add function to make and plot difference in sun-hours
* Add function to calculate min and max sunhours for a given year and what location that is closest
* Minor changes in the plot functions to set limits of colours etc.
* Add possibility to download data per day or per month and then summarise per year.


# v 0.7.0

* New function that check the nearest locale of a set minimum population per coordinate.

# v 0.6.5

* Separate functions that belong together into different script files
* Add function that calculate sun hour diff between given year and mean
* Add function that plot a figure of the sunhour diff

# v 0.6.0

* Add functions to download sunhours
* Add functions to plot figures of sunhours in Sweden
* Add mean sunhours for 2017 to 2021 to internal data

# v 0.5.0

* Separate functions to make station names
* User given station names are resulting in one station with data for the specified year, per user specifies site name.
* Possibility to only view the resulting figures and not ave them as pngs.
* Add posibility to change colours

# v 0.4.1

* Change station from Visby flygplats to Visby
* Change place for `year` variable in `sebms_weather_png()` so it now comes first. Now it is enough to write for example `sebms_weather_png(2021)`

# v 0.4.0

* New function to get station names fro both temperature and precipitation data.
* Make the functions work better together.
* Function `sebms_weather_png()` make pngs for temp and precip figures.
* Possible to chose year

# v 0.3.0

* Recode the plot functions to produce png's and download weather data from SMHI

# v 0.2.0

* Redo the plot functions to conform to the look in the Butterfly report.

## From v 0.2.0 the package is renamed to `sebmsR` and reworked by Georg Andersson and Lars Pettersson.

# v 0.1.7

* Fix issue #16 (use winslashes in path to config file when on "win" platform)


# v 0.1.6

* Minor update of package metadata and documentation (vignette), resolving issue #1

# v 0.1.5

* Activated travis-ci builds with tagged releases pushed to GitHub Releases

* Merged a PR that fixes an issue in the sebms_per_update_modified

# v 0.1.4

* Fixed issue related to https://github.com/rekonstrukt/swedishbutterflies/issues/13 and (on Windows and OSX) this SO post: https://stackoverflow.com/questions/24495487/r-error-thrown-while-using-rgdal-and-raster-packages.

# v 0.1.3

* Fixed issues with path separators not being platform independent therefore causing issues when using Windows 10.

# v 0.1.2

* Fixed default connection template file for config.yml to use "test5" database

* Improved some log messages to make it clear that package installation succeeds even if there is no valid database connection

* Added species histogram plot with original requested style similar to https://user-images.githubusercontent.com/19598308/41712519-02748db0-754b-11e8-8919-5c3d5b64dc66.png

* Removed prefix "v" before week numbers in sebms_species_histo_plot()

* Changed sebms_species_histo_plot_orig() plot for the value scale to achieve the minimal scale 0..10 and to allow tallest bars to roam freely upwards

* Added docs for the new sebms_precip_temp_plot() function and added usage examples to the Vignette to show how to use that function to make a plot with custom data

# v 0.1.1

* Fixed bug in the code that tries to recover a lost db connection

* Fixed bug that under certain conditions could put the config.yml in the wrong location

* Added more documentation to README regarding how to get data from the db for a specific species

* Fixed default connection in config.yml to use "test4" database

* Added stubs for an emryonic web API exposing some of the functions (details in inst/bin/api.R) and also  providing Swagger docs, the server can be started with the exec/serve.R script


# v 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Added spatial data (sunhours, distribution data) and corresponding plots

* Added more content to the Vignette to illustrate using the spatial data and improved the README file with more getting started instructions

* Added a .travis.yml file to the project with the intention to support that Travis CI can build and deploy to GitHub Releases when a tag is pushed using git

* Fixed some bugs from initial testing on Win 7 / R 3.5

* Added several dependencies and moved db connection away from package startup to when first used, added a config for datbaseconnection that can use .Renviron

* Added .travis.yml for future support for continuous integration
