% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indivsInfo_ExtractTrajectories.R
\name{indivsInfo_ExtractTrajectories}
\alias{indivsInfo_ExtractTrajectories}
\title{Extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries}
\usage{
indivsInfo_ExtractTrajectories(
  sfs_points,
  crs = wtsGIS::get_crs("WGS84"),
  step = 1000,
  colTemperature = "temperature",
  verbose = FALSE
)
}
\arguments{
\item{sfs_points}{- list of sf dataframes by typeName or single sf dataframe (output from \code{\link{indivsInfo_ReorderResults}})}

\item{crs}{- coordinate reference system for trajectories: \pkg{sf} crs object, EPSG code, or character with proj4string}

\item{step}{- step size for processing individuals}

\item{colTemperature}{- column name with temperature data (default="temperature")}

\item{verbose}{- flag to print debugging info}
}
\value{
a \pkg{sf} dataframe with a column ("geom") of class sfc_LINESTRING giving the trajectory of each
original individual by life stage (see Details)
}
\description{
Function to extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries.
}
\details{
Requires packages \code{sf}, \code{wtsGIS}.
}
\note{
The output \pkg{sf} dataframe has columns
\itemize{
\item{typeName}
\item{id}
\item{parentID}
\item{origID}
\item{startTime}
\item{successful - included if present in sfs_points}
\item{additional columns}
\item{geom - column with trajectory as an sfg_LINESTRING object}
}
Additional columns are: max_age, max_ageInStage, max_num, min_num,
max_depth, min_depth, mn_depth, max_temp, min_temp, mn_temp.
}
