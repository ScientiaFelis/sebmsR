% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trimfunctions.R
\name{get_trendIndex}
\alias{get_trendIndex}
\title{Generate Trend Index for Each Species}
\usage{
get_trendIndex(
  trimIndex = NULL,
  years = 2010:(lubridate::year(lubridate::today()) - 1),
  Art = 1:200,
  Län = ".",
  Region = ".",
  Landskap = ".",
  Kommun = ".",
  filepath = getwd(),
  tag = NULL,
  indicators = TRUE,
  verification = c(109, 110, 111),
  source = c(54, 55, 56, 63, 64, 66, 67, 84),
  write = FALSE
)
}
\arguments{
\item{trimIndex}{a trim index object from [get_trimindex()]}

\item{years}{the year span of interest, set as 'firstyear:lastyear'.}

\item{Art}{the species of interest}

\item{Län}{character or regular expression; which county you want the data from}

\item{Region}{character or reg ex; which region do you want. Possible values are:
'SGot', 'OGot', 'VGotSve', 'OSve', 'NSveNor', 'NNor'.}

\item{Landskap}{character or reg ex; which region you want the data from}

\item{Kommun}{character or reg ex; which municipality you want the data from}

\item{indicators}{logical; if TRUE use the indicators as species selection,
this override `Art`}

\item{verification}{a verification code that filter out verified occurenses of species,
default to 109.}

\item{source}{the data sources}

\item{write}{logical; if index should be written to csv}
}
\value{
trendindex per species with the number of sites used
}
\description{
Generate a file with the trend for each species and the significans of model.
Also show the number of sites where species existed.
}
