% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggMap_IndivLocations.R,
%   R/plotMap_IndivLocations.R
\name{ggMap_IndivLocations}
\alias{ggMap_IndivLocations}
\title{Create a ggplot2 object with individual locations from a \pkg{sf} dataframe}
\usage{
ggMap_IndivLocations(
  sf_dfr,
  geomCol = "geometry",
  colorBy = "successful",
  colorLabel = colorBy,
  startZones = c(1:8),
  subtitle = "",
  group = FALSE,
  bmls = NULL,
  nSZpG = 1,
  nPlotCols = 2,
  colours = c("red", "blue", "green", "cyan", "black"),
  verbose = FALSE
)

ggMap_IndivLocations(
  sf_dfr,
  geomCol = "geometry",
  colorBy = "successful",
  colorLabel = colorBy,
  startZones = c(1:8),
  subtitle = "",
  group = FALSE,
  bmls = NULL,
  nSZpG = 1,
  nPlotCols = 2,
  colours = c("red", "blue", "green", "cyan", "black"),
  verbose = FALSE
)
}
\arguments{
\item{sf_dfr}{- \pkg{sf} dataframe with geometries to plot}

\item{colorBy}{- name of column to use as levels to color tracks by}

\item{colorLabel}{- label for colours legend}

\item{startZones}{- vector of starting zone id's (integers) for sf_zones}

\item{subtitle}{- plot subtitle}

\item{group}{- flag to group startZones}

\item{bmls}{- list of \pkg{ggplot2} basemap layers}

\item{nSZpG}{- number of start zones to include in a group for end location maps}

\item{nPlotCols}{- number of columns for end location maps}

\item{colours}{- vector of colours to use to distinguish start zones in an end location map}

\item{verbose}{- flag (T/F) to print processing info}
}
\value{
a ggplot2 object

a ggplot2 object
}
\description{
Function to create a ggplot2 object with individual locations from a \pkg{sf} dataframe.

Function to create a ggplot2 object with individual locations from a \pkg{sf} dataframe.
}
\details{
Creates \pkg{ggplot2}-style maps of the starting and
ending locations of individuals. End zone locations can be colored according
to their start zone.

Creates \pkg{ggplot2}-style maps of the starting and
ending locations of individuals. End zone locations can be colored according
to their start zone.
}
