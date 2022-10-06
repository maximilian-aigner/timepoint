\name{sim_hop}
\alias{sim_hop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a homogeneous Poisson process
}
\description{
Simulates a homogeneous Poisson process in two stages,
first the number of points and then their locations.
}
\usage{
sim_hop(bounds = c(0, 100), rate = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bounds}{
Time interval in which to simulate
}
  \item{rate}{
The rate of the process, or the expected number of points per unit time.
}
}
\value{
A numeric vector of event times (numeric form)
}

\seealso{
\code{\link{sim_dsp}}, \code{\link{sim_rsp}}
}
\examples{
sim_hop()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
