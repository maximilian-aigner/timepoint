\name{bikes}
\alias{bikes}
\docType{data}
\title{
Bike-sharing dataset %~~
}
\description{
Arrival and departure times of bike trips in a network of 47 stations. % ~~
}
\usage{data("bikes")}
\format{
  A data frame with 25553 observations on the following 6 variables.
  \describe{
    \item{\code{origin}, \code{destination}}{IDs of the stations}
    \item{\code{departure}}{Departure time (POSIXct)}
    \item{\code{arrival}}{Arrival time (POSIXct)}
    \item{\code{travel_time_min}}{elapsed travel time (arrival - departure), in minutes}
    \item{\code{bike_id}}{IDs of the bikes}
  }
}
\details{
The precision of arrival and departure times is once per minute. Note that the population is not closed, in the sense that the bikes identified by \code{bike_id} freely enter and exit the system of 47 stations recorded here.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(bikes)
transfers <- with(bikes, table(origin, destination))
\donttest{
image(sweep(transfers, 1, rowSums(transfers), "/"))
}
}
\keyword{datasets}
