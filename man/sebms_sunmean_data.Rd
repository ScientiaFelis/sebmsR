% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soltimmar.R
\name{sebms_sunmean_data}
\alias{sebms_sunmean_data}
\title{Create a Mean Sun Hour Value over a Five Year Period}
\usage{
sebms_sunmean_data(year = 2018:2022, months = 4:9, per_month = FALSE)
}
\arguments{
\item{year}{the year or years to produce plot for}

\item{months}{numeric value of the months to summarise sun ours over (default
to 4:9)}

\item{per_month}{logical; summarise per month instead of per year}
}
\description{
Creates a five year mean from the SMHI Iradiance data. This data is also
stored internally in the package to avoid to much downloading
}
