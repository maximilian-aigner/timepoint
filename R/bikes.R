#' Bike-sharing usage dataset
#' 
#' Data on bike transfers between 47 stations. The bike population
#' is not closed as some bikes transfer outside the 47 stations
#' selected here.
#'
#' @docType data
#'
#' @usage data(bikes)
#' @format An object of class "data.frame"
#' @keywords datasets
#'
#'
#' @examples
#' data(bikes)
#' num_transfers <- with(bikes, table(origin, destination))
#' \donttest{image(num_transfers, col = hcl.colors(10))}