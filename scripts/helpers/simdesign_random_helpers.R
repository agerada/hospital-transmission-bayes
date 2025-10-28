#' Generate a random sample of input factor sets
#'
#' @description Generate a random sample of input factor sets
#'
#' @param input list with variables and value ranges
#' @param samples number of random samples
#' @param precision number of digits for the decimal fraction of parameter
#' values
#' @aliases util_create_random
#' @rdname util_create_random
#' @keywords internal
util_create_random <- function(input, samples, precision) {

  # create a random sample of input factor sets (independent draws from priors)
  rnd.design <- lapply(seq(1, length(input)), function(i) {
    spec <- input[[i]]
    qfun <- spec[["qfun"]]
    if (is.null(qfun)) qfun <- "qunif"
    fun <- match.fun(qfun)
    args <- spec
    args[["qfun"]] <- NULL
    # Keep only arguments accepted by the chosen quantile function.
    # If the function supports '...', keep all; otherwise, filter.
    formal_names <- tryCatch(names(formals(fun)), error = function(e) NULL)
    if (!is.null(formal_names) && !("..." %in% formal_names)) {
      args <- args[names(args) %in% formal_names]
    }
    probs <- stats::runif(samples)
    do.call(fun, c(list(p = probs), args))
  })
  names(rnd.design) <- names(input)
  rnd.final <- tibble::as_tibble(rnd.design)
  ## Precision:
  rnd.final <- round(rnd.final, digits = precision)

  return(rnd.final)
}

## simdesign_random: like simdesign_lhs but draws iid random samples from priors
## - nl: nlrx nl object with a defined experiment
## - samples: number of random parameter sets
## - nseeds: number of seeds for this simulation design
## - precision: number of digits for the decimal fraction of parameter values
simdesign_random <- function(nl, samples, nseeds, precision) {
  # Evaluate nl object and variables (mirror simdesign_lhs behavior when available)
  nlrx:::util_eval_experiment(nl)
  nlrx:::util_eval_variables(nl)
  nlrx:::util_eval_variables_sa(nl)
  message("Creating random simulation design")

  rnd <- util_create_random(input = nlrx::getexp(nl, "variables"),
                             samples = samples,
                             precision = precision)

  ## Bind constants if any:
  if(length(nlrx::getexp(nl, "constants")) > 0)
  {
    rnd <- cbind(rnd, nlrx::getexp(nl, "constants"), stringsAsFactors=FALSE)
  }

  ## Convert to tibble:
  rnd <- tibble::as_tibble(rnd)

  ## Generate seeds
  seeds <- nlrx:::util_generate_seeds(nseeds = nseeds)

  # Add simdesign to nl
  new_simdesign <- nlrx::simdesign(simmethod="random",
                                   siminput=rnd,
                                   simseeds=seeds)

  return(new_simdesign)
}