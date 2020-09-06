#' Analysis of Jacobian Lotka-Volterra Food Web Matrices
#'
#' Used primarily to repeat simulations and analyses of Pimm and Lawton (1977),
#' given a Jacobian matrix.  Analyses include eigenanalysis, but also measuring
#' average interaction strength (May 1972), average intraspecific negative
#' density dependence, and the strength of the omnivory interaction, if
#' present.
#'
#' This function simulates a constrained randomization of a Jacobian food web
#' matrix.  The matrix it uses \code{mat} is of a special form, which assumes
#' that all non-zero values are drawn from a uniform distribution between zero
#' and a value of some specified magnitude, either positive or negative.
#'
#' @param mat a numerical matrix; the "maximum" Jacobian matrix. See details
#' below.
#' @param N a scalar for the number of randomizations
#' @param omni.i if omnivory is present, the row/col index for the prey.
#' @param omni.j if omnivory is present, the row/col index for the predator.
#' @param omega if not NULL, a scalar $0<x<1$ indicating the relative weight of
#' omniovory (after McCann et al. (1998))
#' @return Returns a data frame, where each row corresponds to a single random
#' Jacobian matrix, with the following columns.  \item{DomEig }{the real part
#' of the dominant eigenvalue } \item{Im }{the imaginary part of the dominant
#' eigenvalue } \item{IntraDD }{average magnitude, over all species, of the
#' intraspecific negative density dependence; the square root of the sum of the
#' squared diagonal elements of the random Jacobian matrix } \item{I }{average
#' interaction strength (after May 1972); the square root of, sum of the
#' squared off diagonal elements divided by the number of off diagonal
#' elements.} \item{I.omni }{average interaction strength for the omnivory
#' interaction; the square root of, sum of the squared omnivory elements effect
#' of predator on prey and prey on predator divided by two.}
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @references May, R.M. (1973) \emph{Stability and Complexity in Model
#' Ecosystems}, volume 6 of \emph{Monographs in Population Biology}. Princeton
#' University Press.
#'
#' McCann, K., Hastings, A. and Huxel, G.R. (1998) Weak trophic interactions
#' and the balance of nature. \emph{Nature}, \bold{395}, 794--798.
#'
#' Pimm, S.L and Lawton, J.H. (1997) Number of trophic levels in ecological
#' communities. \emph{Nature}, \bold{268}, 329--331.
#'
#' Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R! Series.
#' Springer. 2009.
#' @keywords methods
#' @export
#' @examples
#'
#' ### A relevant style of matrix for the first food chain in Pimm and Lawton (1977).
#' ### Note each non-zero element is the appropriate sign, and the maximum magnitude
#' ### specified by Pimm and Lawton (1977).
#'
#' Aq = matrix(c(
#'    -1, -10,   0,   0,
#'   0.1,   0, -10,   0,
#'     0, 0.1,   0, -10,
#'     0,   0,  0.1,  0),
#'   nrow=4, byrow=TRUE)
#'
#' pimmlawton(Aq, N=1)
#'
#' out <- pimmlawton(Aq, N=2000)
#' out <- subset(out, -1/DomEig <150)
#' hist(-1/out$DomEig, main="Frequencies of Return Time")
#'
`pimmlawton` <-
function (mat, N = 1, omni.i = NA, omni.j = NA, omega = NULL)
{
    S <- nrow(mat)
    if (is.na(omni.i)) {
        out <- matrix(NA, nrow = N, ncol = 4)
        colnames(out) <- c("DomEig", "Im", "IntraDD", "I")
        for (n in 1:N) out[n, ] <- {
            M = runif(S^2) * mat
            eigs <- eigen(M)[["values"]]
            mx <- which.max(Re(eigs))
            deM = Re(eigs)[mx]
            deMi = Im(eigs)[mx]
            intraNDD <- sqrt(sum(diag(M)^2)/S)
            diag(M) <- 0
            IS = sqrt(sum(M^2)/(S * (S - 1)))
            c(deM, deMi, intraNDD, IS)
        }
    }
    else {
        out <- matrix(NA, nrow = N, ncol = 5)
        colnames(out) <- c("DomEig", "Im", "IntraDD", "I", "I.omni")
        for (n in 1:N) out[n, ] <- {
            M = runif(S^2) * mat
            if (!is.null(omega)) {
                M[omni.i, omni.j] <- omega * M[omni.i + 1, omni.j]
                M[omni.i + 1, omni.j] <- (1 - omega) * M[omni.i +
                  1, omni.j]
                M[omni.j, omni.i] <- omega * M[omni.j, omni.i +
                  1]
                M[omni.j, omni.i + 1] <- (1 - omega) * M[omni.j,
                  omni.i + 1]
            }
            eigs <- eigen(M)[["values"]]
            mx <- which.max(Re(eigs))
            deM = Re(eigs)[mx]
            deMi = Im(eigs)[mx]
            intraNDD <- sqrt(sum(diag(M)^2)/S)
            diag(M) <- 0
            IS = sqrt(sum(M^2)/(S * (S - 1)))
            omnivory <- sqrt(mean(c(M[omni.i, omni.j], M[omni.j,
                omni.i])^2))
            c(deM, deMi, intraNDD, IS, omnivory)
        }
    }
    return(as.data.frame(out))
}
