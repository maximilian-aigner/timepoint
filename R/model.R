# tpmodel <- function(formula_time, formula_marks = NULL, eventlist, covariates,
#                             link = "log", do.fit = TRUE) {
#   mt <- terms(formula, specials = c("cox", "ar"))
#   if (attr(mt, "response") != 0) {
#     yname <- mt$variables[attr(mt, "response")]
#     stopifnot(yname %in% names(eventlist$values[1]))
#     y <- lapply(eventlist$values, \(x) `[[`, yname)
#   }
#   
#   stopifnot(marks %in% c("separable", "independent", "joint"))
#   
#   mtl <- mt$term.labels
#   if (any(grepl(":", mtl, fixed = TRUE)))
#     stop("Interaction terms not supported yet")
#   
#   rani <- grepl("|", mtl, fixed = TRUE)
#   ran <- mtl[rani]
#   det <- mtl[!rani]
#   
#   stopifnot(all(mtl %in% names(covariates)))
#   
#   if (do.fit)
#     out <- tpmodel.fit(y = eventlist, x = covariates, marks = marks, link = link)
#   else
#     out <- NULL
#   return(structure(list(
#     formula = formula,
#     fitted_model = out,
#     covariates = covariates,
#     link = link
#   )), class = "tpmodel")
# }
tpmodel <- function(formulas, eventlist,
                    link = "log", do.fit = TRUE, type = "poiss") {
  if (length(formulas) == 1) {
    formula_time <- formulas
    formula_marks <- NULL
    marked <- FALSE
  } else {
    formula_time <- formulas[[1]]
    formula_marks <- formulas[[2]]
    marked <- TRUE
  }
  
  terms_time <- terms(formula_time, specials = c("cox", "ar", "s"))
  # TODO: Implement Cox and AR terms
  if (!is.null(attr(terms_time, "specials"))) stop("not implemented yet")
  if (marked) terms_marks <- terms(formula_marks)
  
  if (marked & attr(terms_marks, "response") == 0) {
    stop("need to specify response in marked model")
  }
  
  tt <- eventlist$num_times
  if (marked) {
    yy <- sapply(eventlist$values, `[[`, attr(terms_marks, "term.labels"))
  }
  
  # TODO: Implement covariates
  xreg_tt <- rep(1, length(tt))
  xreg_yy <- rep(1, length(tt))
  
  if (do.fit) {
    if (marked)
      if (type == "poiss") {
        out <- tpmodel.fit.poisson.marked(tt, xreg_tt, yy, xreg_yy, link = link)
      } else stop("not implemented yet")
    else {
      if (type == "poiss")
        out <- tpmodel.fit.poisson.unmarked(tt, xreg_tt, link = link)
      else stop("not implemented yet")
    }
  } else out <- NULL
  return(structure(list(
    formula = formula,
    fitted_model = out,
    covariates = covariates,
    link = link
  )), class = "tpmodel")
}

tpmodel.fit.poiss.unmarked <- function(tt, xreg_tt, link) {
  # Model is 
  # N(t, A) ~ Poisson(\int_0^t \lambda(s, A) ds)
  # g(\lambda(t)) = x'\beta
  if (marks == "independent") {
    
    lhood <- function(th) lhood1(th) + lhood2(th)
  }
  if (marks == "separable") {
    
  }
}