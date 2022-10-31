\name{mooc}
\alias{mooc}
\docType{data}
\title{
MOOC dataset
}
\description{
Data of user activities for 10 users on a MOOC website. The data are sourced from a paper of Feng, Tang and Liu. 
}
\usage{data("mooc")}
\format{
  A data frame with 522 observations on the following 4 variables.
  \describe{
    \item{\code{username}}{user id (numeric)}
    \item{\code{course_id}}{course id (numeric)}
    \item{\code{session_id}}{session id (numeric)}
    \item{\code{time}}{time at which session was initiated}
  }
}
\details{
}
\source{
Feng, W., Tang, J., & Liu, T. X. (2019). Understanding Dropouts in MOOCs. Proceedings of the AAAI Conference on Artificial Intelligence, 33(01), 517-524. https://doi.org/10.1609/aaai.v33i01.3301517
}
\references{
http://moocdata.cn/data/user-activity
}
\examples{
data(mooc)
}
\keyword{datasets}
