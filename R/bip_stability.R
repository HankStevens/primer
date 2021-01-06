#' Bipartite network properties
#'
#' A function to calculate the several properties of randomly constructed bipartite networks. This function installation of  packages bipartite, igraph, and rARPACK.
#'
#' The interaction strengths are exponentially distributed, with mean of IS (rate = 1/IS). Nestedness is WNODF (Almeida-Neto et al. 2011). It can calculate modularity in two ways, using either the binary adjacency matrix (ones and zeroes, Clauset, Newman and Moore 2004) or a quantitative matrix (Beckett 2016); the latter takes much moore time to calculate, so it uses the binary matrix by default. It also tallies diversity as the total number of species and the connectance as the total number of links divided by the number of possible links.
#'
#' To calculate resilience, the P by A bipartite matrix is expanded to a (P+A) x (P+A) symmetric matrix so the upper triangular portion are the effects of the animals on the plants, and the lower triangular portion is the effects of the plants on the animals.
#'
#' The diagonal represents intraspecific density dependence; it is filled with the negative of the largest summed row of interaction strengths in the symmetric matrix. The symmatric matrix is interpreted as a Jacobian matrix. Therefore, resilience is the negative value of the real part of the dominant eigen value of the expanded symmetric matrix.
#'
#'
#' @param Spp.a numeric vector from which to draw a random number of animal species
#' @param Spp.p numeric vector from which to draw a random number of animal species
#' @param C numeric two element vector for minimum and maximum possible connectances.
#' @param IS mean interaction strength. If NULL, it is calculated as the inverse of the matrix dimension, so that the expected sum of interaction strengths equals one.
#' @param quant Logical indicating whether the estimate of modularity should be based on the binary adjacency matrix (FALSE, default), or a quantitative matrix (TRUE).
#' @param ndd scalar (1 or 2) determining whether the intraspecific negative density dependence is the same in both networks (1), or is scaled to the interaction sizes in each matrix (2).
#' @param reps number of random matrices.
#' @return Returns of list of a matrix with columns for species richness, connectance, nestedness, modularity, and resilience.
#'
#' @author Hank Stevens <HankStevens@@miamioh.edu>
#' @references
#' M. Almeida-Neto and W. Ulrich. A straightforward computational approach for measuring nestedness using quantitative matrices. Environmental Modelling and Software, 26(2):173–178, 2011.
#'
#' S. J. Beckett. Improved community detection in weighted bipartite networks. Royal Society Open Science, 3:140536, 2016.
#'
#' A. Clauset, M. E. J. Newman, and M. Cristopher. Finding community structrue in very large networks. Physical Review E, 70:066111, 2004.
#'
#' @keywords methods
#' @export
#' @examples
#'
#' # b <- bip_stability(Spp.a = 16:60, Spp.p = 8:30, C=c(.05, 0.5), reps=10, quant=FALSE)
#' # pairs(b)
#'
`bip_stability` <- function( Spp.a = 10:50, Spp.p = 5:25, C=c(.05,.3), IS=NULL, quant=FALSE, ndd=1, reps=10){

    out.m <- data.frame(S=numeric(reps), C = numeric(reps),
                        modularity = numeric(reps),
                        nestedness = numeric(reps),
                        resilience.m=numeric(reps),
                        resilience.h=numeric(reps)
    )

    for(i in 1:reps) {
        Sp <- sample(Spp.p, size=1)
        Sa <- sample(Spp.a, size=1)
        S = Sp + Sa

        #random draw for C
        contc <- runif(1, min=C[1], max=C[2])

        # random draw for bipartite interactions, each = C
        interactions <- rbinom( n = Sa * Sp, size=1, prob=contc)

        # Bipartite Network
        bn <- matrix(interactions, nrow=Sp, ncol=Sa)
        rownames(bn) <- paste("plant", LETTERS[1:Sp], sep="")
        colnames(bn) <- paste("bug", LETTERS[1:Sa], sep="")

        # determine final web, based on "observed" interactions
        # remove rows and cols with no interactions
        rs <- rowSums(bn)
        cs <- colSums(bn)
        bn <- bn[rs>0, cs>0]

        # Find "observed" S.p and S.a
        Sp <- dim(bn)[1]
        Sa <- dim(bn)[2]
        S <- Sa+Sp

        # exponentially distributed interactions
        # what is average interaction strength (IS)?
        # expected IS is 1/rate
        if(is.null(IS)) IS <- 1/(Sa*Sp)
        xp <- rexp(Sa*Sp, rate=1/IS)

        # Bipartite Network with Exponentially distributed IS
        bne <- bn*xp
        n <- matrix(0, nrow=S, ncol=S)

        n[1:Sp, (Sp+1):S] <- bne
        n[(Sp+1):S, 1:Sp] <- t(bne)
        n2 <- n
        n2[1:Sp, (Sp+1):S] <- bne * -1

        # create negative density dep. for all species
        n.s <- list(n,n2)
        diag(n) <- -1*max(rowSums(n))
        # using ndd argument to select the magnitude of the NDD
        diag(n2) <- -1*max(rowSums(n.s[[ndd]]))

        # quant. version of NODF  Almeida-Neto et al. (2008, 2011)
        WNODF <- bipartite::networklevel(bne,
                         index=c("weighted NODF"))

        #connectance
        L <- sum( abs(bne) > 0 )
        connectance <- L/prod(dim(bne))

        # modularity: quantitative or based on adjacency matrix
        # The latter is much faster and is the default.
        if(!(quant)){
            m <- (n > 0) + 1 - 1
            g <- igraph::graph_from_adjacency_matrix(m)
            wtc <- igraph::cluster_walktrap(g)
            mod <- igraph::modularity(wtc)
        } else {
            mod <- bipartite::computeModules(bne, method="Beckett") @ likelihood
        }

        # resilience
        l1 <- rARPACK::eigs(n, k=2)$values[1]
        l1n <- rARPACK::eigs(n2, k=2)$values[1]

        # store measures
        out.m[i,] <- c(S, connectance, mod,
                       WNODF, -Re(l1), -Re(l1n) )

    }

    return( out.m )
}
