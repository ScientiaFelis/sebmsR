NEWS
====

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/scientiafelis/sebmsr/issues)
* [The commit log](https://github.com/scientiafelis/sebmsr/commits/main)

# v 1.4.8

This version fixes some bugs and small issues

* **New option** to set printing to plot window, disabled (`print = FALSE`) by default.
* Add in zero observation category again in `sebms_species_per_sitetype_plot()`
* Add more species number categories to `sebms_species_per_sitetype_plot()`. Now it goes to '61-65'
* Add species aggregates to `sebms_species_abundance_plot()`
* Fix naming of species aggregates from '/' to '-'
* Fix file name of png so windows also can handle them. Now we separate species in aggregates with '_' and years with '-'


# v 1.4.7

This version contain new max limits and steps for figures to be a bit neater. It also recalculate species number and site per species group in the `sebms_species_per_sitetype_plot()`

* Add new max limits and step function
* Disregard species aggregate in specie richness calculations if one species in the aggregate exists on site.
* Include sites with zero observation in the average species per site calculations
* Now possible to use multiple years in the `sebms_species_per_sitetype_plot()` function.
* Through an error and exit if more than two years are given in `sebms_abundance_year_compare_plot()`


# v 1.4.6

This version fixes some bugs and a number of Build check errors. It also update the documentation.

* Fix various errors from Build check, package dependencies, variable defenitions etc
* Fix issue with month and week labels in `sebms_abundance_year_compare_plot()`
* Fix issue in filtering out 'nullobservation' and combined species
* Fix issue with placement of mean values in `sebms_species_per_sitetype_plot()`

# v 1.4.5

In this version there is a new function `editcred()` that add or edit a .Renviron file to your home directory.
There are also some bug fixes and updated docmentation.

* New function: `editcred()`. This check if you have a `.Renviron` file in the given path (which should be your home directory). It will create/open it and add the Data base variables used by the `config.yml` file to connect to a PostgreSQL database.

* Add a `source` argument in the butterfly plot functions to set the database sources.

* The package should no longer load or connect to a database on load or attachment.

* Fix a bug where the pool where not exported to the query function in the SQL data base functions.

* Fix some issues with figures to make them closer to report.


# v 1.4.0

This version come with a new theme for some of the plot functions.
This theme`theme_sebms_species()` reduce the theme ad on requirements after the 
theme_sebms()`.
There is also a number of detail changes in figure looks which makes it as close to
the report figures as possible.

* New theme `theme_sebms_species()` which takes care of most differences between some of the species plots.
* A progress bar now show the progress for the processes in plot functions when there are many figures to make and save.
* The species species name can now be added as the plot title in `sebms_species_abundance_plot()` by setting `plotname=TRUE`
* The weeks are now set to always be between 14 and 40 for both `sebms_species_abundance_plot()` and `sebms_abundance_year_compare_plot()`.
* Axis text are no longer bold and it is a bit larger.
* Gridlines are a bit thicker and darker.
* Bars in `sebms_species_abundance_plot()` is a bit wider.
* More documentation and comments in scripts.


# v 1.3.7

* Rename functions to a bit more describing names. There is still room for improval.
* Add function list and description to package documentation. 
* Fixed some descriptions of the data retrieval functions.
* Changed some of the SQL query in a couple of data retrieval functions which filtered on some specific counties.

# v 1.3.6

* Complete missing zero data for `species_per_sitetype_plot()` species nr groups for site type
* Add tick marks between groups on x-axis for `species_per_sitetype_plot( )` and between species names on y-axis in `specieslist_cum_plots()`
* Fix small error in date filter in `species_count_histo_plot()`

# v 1.3.5

* Make possibility to chose species via `Art` argument **on species id** in `sebms_species_histo_plot()`
* Possible to chose several species and get one figure per species
* SQL now filter the species, not R
* SQL match on `reg_uid` from a look up table
* Change the steps and limits on *y-axis* and *x-axis* to fit different max counts.

# v 1.3.0

* Add possibility to chose County, Region or Municipality to figures
* Cleaned up code in SQL queries
* Minor fixes to plot margins

# v 1.2.0

* Figures of species numbers now almost the same as in previous reports
* Possible to filter per *year* and *species* in species plots

# v 1.0.1

* Minor bug fix. Bug caused month name in temperature and precipitation plots follow locale locale. No use the Swedish locale names.

# v 1.0.0

* The functions now can also make a figure with sunhours **per month** in addition to per year.
* The plots have a limit for each month with different min and max values that guide which values should be blue and red.
* There is now a posibility to add legends to the figures by setting *legends* to **TRUE**


# v 0.9.2

* Now the `sebms_sunhour_plot()` iterate over years you have sunhour data from, which makes it possible to give a span of years or give a data.frame from `sebms_sunhour_data()` with several years. It will pruduce a figure and png for each year.
* The `sebms_sundiff_plot()` also now iterate over years and produce figure for each year.

# v 0.9.1

* Remove year variable in ggsave function and incorporate it into the naming in plot functions.
* You can now give data.frame from `sebms_sunhour_data()` to `sebms_sundiff_plot()` directly instead of feed it through `sebms_sunhour_diff()`.
* The `sebms_minmax_sunhour()` had an error where it filtered on years 2021 and 2022, in now filter out the **given** years.
* Add variable to chose which factors to extract the min and max from in the `sebms_minmax_sunhour()` function. This enable you to find the places where there were most and least sun but also, if you feed it diff data, where the difference is as greatest in either more or less sun compare to the mean.
* Reduce the width to 6 in to reduxe white space around map.

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
