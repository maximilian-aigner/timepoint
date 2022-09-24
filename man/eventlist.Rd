\name{eventlist}
\alias{eventlist}
\alias{summary.eventlist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Eventlist objects
}
\description{
An eventlist object represents a dataset of event times and (optionally) marks, in human-readable form and in a numerically convenient internal format.
}
\usage{
eventlist(datetimes, values = NULL, bounds = NULL, units = "auto")
\method{summary}{eventlist}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datetimes}{
  A vector of POSIXct or POSIXt values representing times of events.
%%     ~~Describe \code{datetimes} here~~
}
  \item{values}{
  An optional list or vector of marks.
%%     ~~Describe \code{values} here~~
}
  \item{bounds}{
  The time interval which was observed, including but not limited to the range of \code{datetimes}.
%%     ~~Describe \code{bounds} here~~
}
  \item{units}{
  If desired, a time unit may be specified here. Otherwise, a convenient choice is made (via \code{difftime}).
%%     ~~Describe \code{units} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of time \code{eventlist}, with attributes:

\item{\code{datetimes, bounds, values}}{The original inputs}

\item{\code{num_times, num_bounds, num_units}}{Numerical (working) times and bounds, as well as the units}

\item{\code{marked}}{Whether the eventlist contains values}

}
\author{
Maximilian Aigner \email{maximilian.aigner@unil.ch}
}

\seealso{
\code{\link{summary.eventlist}}
}
\examples{
data(bikes, package = "timepoint")
eventlist(bikes$departure, bikes$origin,
          bounds = lubridate::ymd("2022-07-05", "2022-08-09", tz = "UTC"),
          units = "days")
}
