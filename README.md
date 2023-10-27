[![Build
Status](https://scientiafelis/sebmsr.svg?branch=master)](https://scientiafelis/sebmsr)

<!-- README.md is generated from README.Rmd. Please edit that file -->

`sebmsR` is an R package for SeBMS - the Swedish Butterfly Monitoring
Scheme - offering tools for accessing data, making plots and a Shiny
app.

## Under construction

The package is currently under re-development and some functions do not
work well, e.g. the Shiny app and reading from a Postgres database. The
functions related to weather data, e.g. `sebms_weather_png()`,
`sebms_sunhours_data()`, `sebms_sunhour_plot ()`, and
`sebms_sundiff_plot()` works well.

## Installing from github

If you want to install the latest version:

``` r
# First make sure you have the devtools package
# which simplifies installations from github
# Note: Windows users have to first install Rtools to use devtools

install.packages("devtools") 
library(devtools)
install_github("scientiafelis/sebmsr")

# A specific versioned release can be installed like this:
install_github("scientiafelis/sebmsr@0.4.0")
```

## Quick start

Since the package can read data from a Postgres db with live data from
SeBMS, some initial system configuration may first be needed to set up
the connection.

### Secure shell connection to the database server

If the database connection requires an ssh tunnel to first be
established, and you are using a \*nix system, this configuration can be
achieved by editing `~/.ssh/config`, adding a section such as:

``` bash
Host butterflies
    User my_ssh_user
    HostName my_server_ip_for_the_postgres_db_server
    LocalForward 5432 127.0.0.1:5432
    ServerAliveInterval 30
```

After this, the tunnel can be established with the command
`ssh -N butterflies`. Upon success, the database can then be reached
locally at the postgres db standard port. Similar results can be
achieved using `putty` on Windows platforms.

### Config for db connection

Once the database server is available, the R package needs to be
configured to use the database connection. To achieve this, load the
package in your R environment and create a `config.yml` with the db
connection details and perhaps also a `.Renviron` file, if you have a
config file that references environment variables in R:

``` r

library(sebmsR)
library(rappdirs)

# this is the location for config.yml holding db connection details
app_dir("sebms")$config() 
# the location may vary depending on OS
# on a Linux OS a valid path for the file is $HOME/.config/sebms/config.yml
# on a Windows 8 OS a valid path for the file is C:\Users\IEUser\AppData\Local\sebms\sebms\config.yml

# this is the location for .Renviron
Sys.getenv("R_USER")
# on a Linux OS a valid path for the .Renviron file is $HOME ie ~/.Renviron
# on a Windows 8 OS a valid path for the .Renviron file is C:\Users\IEUser\.Renviron
# on a Windows 10 OS a valid path for the .Renviron file is C:\Users\IEUser\Documents\.Renviron
```

Example content that can be used in the `config.yml`:

``` bash
default:
  sebms:
    driver: !expr RPostgres::Postgres() 
    server: 'localhost'
    uid: 'my_db_username'
    pwd: 'my_db_password'
    port: 5432
    database: 'test4'
```

If you prefer to use environment variables for the credentials and
reference those in the `config.yml`, the file can look like this:

For the above connection to be initiated, you also need to set up your
`.Renviron` with the environment variables containing the credentials.
For that run the function `editcred(homepath = 'user home dir')`:

``` console
DBUSER = my_db_username
DBPASS = my_db_password
DBNAME = PostgreSQL_database_name
```

## Usage

After getting connected to the database, look at usage examples to get
you started.

Please read the Vignette, using either the Help tab in RStudio IDE or
the R prompt command `browseVignettes(package = "sebmsR")`.

### Examples

To make a plot and generate the pngs for the weather data for the
Butterfly yearly report of **2022** you can just do
`sebms_weather_png(2022)`. For the sunhour figure for the same year do
`sebms_sunhour_plot(2022)`.

To make figures with butterfly data use:

`sebms_abundance_per_species_plot(2022)` to make bar plots with
individual numbers for each species, divided on two png figures.
`sebms_abundance_year_compare_plot(2021:2022)` to make bar plots
comparing individuals per week compared between two years.
`sebms_species_abundance_plot(2021, Art = 118)` to make bar plots for
individual species with numbers per week.
`sebms_species_per_sitetype_plot(2022)` to make bar plots to show how
many species sites have and the number distribution of these, compared
between site type. Also the mean number of species per site type.

## Development

To further develop or change the package, please refer to instructions
at <http://r-pkgs.had.co.nz/>, then fork this repo and submit a PR with
the changes.

For a concrete example - to make a change with regards to how the
filtering on species and year dimensions works for the species data,
edit the ‘R/data.R’ file for example by adjusting the query used in the
sebms_species_per_year function, and possibly adding a test in
`test/testthat/test-sebms-various.R` that verifies expected results,
then do the Ctrl+Shift+{D,T,E} steps and then use git to commit and push
the changes.

To change functions that retrieve data from the db, please make changes
primarily in the `R/data.R` file.

The plotting uses ggplot2 and leaflet mostly and functions are in
`R/plot.R` and `R/zgeomaps.R` for the spatial data.

The long-form documentation / Vignette is located in
`vignettes/sebms-intro.Rmd`.

## Credits

The package bundles code and data assembled and curated by Lars
Pettersson at <http://dagfjarilar.lu.se>

## Meta

-   Please [report any issues or
    bugs](https://github.com/rekonstrukt/swedishbutterflies/issues).
-   License: AGPL
