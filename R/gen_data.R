#' Generate Data for MCN Model
#'
#' @param n Numeric. Number of participants.
#' @param items Numeric. Number of items.
#' @param theta_vcov Covariance matrix for MRS, ERS, and target trait.
#' @param betas Jx3 matrix with item parameters beta pertaining to the three
#'   dimensions (MRS, ERS, target trait).
#' @param thetas Optional matrix. If provided, this will be used as the person
#'   parameters instead of generating them using \code{theta_vcov}. Must be
#'   either \code{NULL} or an NxP matrix.
#' @return A list containing the generated matrix of responses \code{dat}, the
#'   generated person parameters \code{theta}, and the calculated response
#'   probabilities \code{prob} among other things.
#' @inheritParams MASS::mvrnorm
#' @importFrom stats pnorm rmultinom
#' @examples
#' # generate data
#' n <- 20; i <- 10
#' betas <- cbind(runif(i, 0, 1), runif(i, 0, 1), rnorm(i, 0, 2))
#' gen <- sim_tree_data(n = n, items = i, betas = betas, theta_vcov = diag(3))
#' # barplot(table(factor(gen$dat[, 1], 0:4)))
#' @export
sim_tree_data <- function(n = 200,
                          items = 10,
                          theta_vcov = NULL,
                          betas = NULL,
                          thetas = NULL,
                          empirical = FALSE
                          ) {
    args <- c(as.list(environment()))

    checkmate::assert_matrix(theta_vcov, any.missing = FALSE,
                             min.cols = 3, ncols = nrow(theta_vcov))
    checkmate::assert_matrix(betas, any.missing = FALSE,
                             ncols = 3, nrows = items)

    categ <- 5
    # Function might be generalized to >= 1 MRS/ERS/Trait dimensions and this
    # may then be indicated through the following vectors
    mrs_item   <- rep(1, sum(items))
    trait_item <- rep(1, sum(items))
    ers_item   <- rep(1, sum(items))

    ndim <- max(mrs_item) + max(trait_item) + max(ers_item)

    if (max(trait_item) > 2) {
        stop("'trait_item' must currently not be larger than 2 ",
             "(i.e., (a) the MRS-, (b) the ERS-, and (c) the target trait-process ",
             "may each either be uni- or two-dimensional).")
    }
    if (max(trait_item) > 1 & length(items) == 1) {
        items <- rep(items, max(trait_item))
    }
    it_tot <- sum(items)

    if (length(trait_item) == 1) {
        trait_item <- rep(1:trait_item, items)
    }
    ndim <- max(mrs_item) + max(trait_item) + max(ers_item)

    if (is.null(thetas)) {
        theta_mu <- rep(0, ndim)
        theta <- MASS::mvrnorm(n, theta_mu, theta_vcov, empirical = empirical)
    } else {
        theta <- thetas
        n <- nrow(thetas)
    }


    # decompose to person ability and item difficulty
    p <- dat <- array(NA, c(n, it_tot, 5))
    m <- y <- e <- matrix(NA, n, it_tot)
    for (ii in 1:n) {
        for (jj in 1:it_tot) {

            mjj <- mrs_item[jj]
            ejj <- max(mrs_item) + ers_item[jj]
            tjj <- max(mrs_item) + max(ers_item) + trait_item[jj]
            m[ii, jj] <- pnorm(theta[ii, mjj] - betas[jj, 1])
            e[ii, jj] <- pnorm(theta[ii, ejj] - betas[jj, 2])
            y[ii, jj] <- pnorm(theta[ii, tjj] - betas[jj, 3])

            # response probabilities: MPT model from -2, -1, 0, 1, 2
            p[ii, jj, 5] <- y[ii, jj]*e[ii, jj]
            p[ii, jj, 4] <- (1 - m[ii, jj])*y[ii, jj]*(1 - e[ii, jj])
            p[ii, jj, 3] <- m[ii, jj]*(1 - e[ii, jj])
            p[ii, jj, 2] <- (1 - m[ii, jj])*(1 - y[ii, jj])*(1 - e[ii, jj])
            p[ii, jj, 1] <- (1 - y[ii, jj])*e[ii, jj]

            dat[ii, jj,] <- rmultinom(1, 1, p[ii, jj, 1:5])
        }
    }

    dat_cat <- matrix(NA, n, it_tot)
    for (i in 1:n) {
        for (j in 1:it_tot) {
            dat_cat[i, j] <- (0:(categ - 1))[dat[i, j,] == 1]
        }
    }

    return(list(dat = dat_cat,
                trait_item = trait_item,
                theta = theta,
                betas = betas,
                theta_vcov = theta_vcov,
                probs = p,
                items = items,
                args = args
    ))
}
