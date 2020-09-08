

#' Secondary succession data
#'
#' Functional group abundances (herbaceous annual, herbaceous perennial, woody)
#' from one of the fields in the Buell-Small long term succession study
#' (http://www.ecostudies.org/bss/).  Data are based on visual estimates of
#' percent cover, using annual means of each species, which are then summed for
#' each functional group.
#'
#'
#' @name BSsucc
#' @docType data
#' @format A data frame with 147 observations on the following 3 variables.
#' \describe{\item{AGE}{a numeric vector; indicates the age of
#' succession since abandonment from agriculture.}
#' \item{variable}{plant functional type; a factor with levels
#' \code{Annual}, \code{Perennial}, \code{Woody}} \item{value}{a
#' numeric vector} }
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @source Data graciously provided by Scott Meiners (Eastern Illinois
#' University, http://www.ecostudies.org/bss/).
#' @keywords datasets
#' @examples
#'
#' data(BSsucc)
#' #lattice::xyplot(value~ AGE, groups=variable, data=BSsucc,
#'              #type='smooth', span=.3, ylab="Percent Cover",
#'              #xlab="Age Since Abandonment (y)",
#'              #auto.key=list(columns=3, space="top", lines=TRUE,
#'              #points=FALSE))
#'
NULL





#' Data drawn approximately from Collins and Glenn (1991)
#'
#' Numbers of species which were observed in 1--19 sites.
#'
#'
#' @name CandG
#' @docType data
#' @format The format is: num [1:19] 32 16 10 9 8 7 8 6 4 5 ...
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @source Data are approximations of histogram values for a single year of
#' data in:
#'
#' Collins, S.L. and Glenn, S.M. (1991) Importance of spatial and temporal
#' dynamics in species regional abundance and distribution. \emph{Ecology},
#' \bold{72}, 654--664.
#' @keywords datasets
#' @examples
#'
#' data(CandG)
#' barplot(CandG, names=1:19)
#'
NULL





#' Closterium Population Data
#'
#' Data set from an experiment testing whether trophic heterogeneity influences
#' long term population sizes.
#'
#'
#' @name ClostExp
#' @docType data
#' @format A data frame with 144 observations on the following 5 variables.
#' \describe{ \item{Nutrients}{a factor with levels \code{high}
#' \code{low}} \item{No.per.ml}{a numeric vector} \item{Day}{a
#' numeric vector} \item{rep}{a factor with levels \code{a} \code{b}
#' \code{c} \code{d}} \item{ID}{an ordered factor with levels
#' \code{a.low} < \code{d.low} < \code{c.low} < \code{b.low} < \code{c.high} <
#' \code{a.high} < \code{d.high} < \code{b.high}} }
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @source Stevens, M. H. H. and Steiner, C. E. (2006) Effects of predation and
#' nutrient enrichment on a food web with edible and inedible prey.
#' \emph{Freshwater Biology}, \bold{51}, 666--671.
#' @keywords datasets
#' @examples
#'
#' data(ClostExp)
#' #lattice::xyplot(log10(No.per.ml) ~ Day|Nutrients, data=ClostExp, groups=rep, type='l')
#'
NULL





#' Moth Species Richness
#'
#' Data set of moth species richness from Ohio and Indiana, USA.
#'
#'
#' @name moths
#' @docType data
#' @format A data frame with 21 observations on the following 6 variables.
#' \describe{ \item{region}{a factor with levels \code{NCT} \code{WAP}
#' for the two collection regions.} \item{site}{a factor with levels
#' associated with county names.} \item{area}{a numeric vector for the
#' area of each forest fragment} \item{spp}{a numeric vector, the
#' number of moth species.} \item{lat}{a numeric vector; latitude}
#' \item{long}{a numeric vector; longitude} }
#' @references K. S. Summerville and T. O. Crist. 2003. Determinants of
#' lepidopteran community composition and species diversity in eastern
#' deciduous forests: roles of season, ecoregion and patch size. \emph{Oikos},
#' 100:134-148.
#'
#' K. S. Summerville and T. O. Crist. 2004. Contrasting effects of habitat
#' quantity and quality on moth communities in fragmented landscapes.
#' \emph{Ecography}, 27:3-12.
#' @source Data proided graciously by Keith Summerville and Thomas Crist; see
#' "A Primer of Ecology with R" for specific references.
#' @keywords datasets
#' @examples
#'
#' data(moths)
#' plot(spp ~ area, data=moths)
#'
NULL





#' Functions and data for "A Primer of Ecology with R"
#'
#' Functions are primarily functions for systems of ordinary differential
#' equations, difference equations, and eigenanalysis and projection of
#' demographic matrices; data are for examples.
#'
#' \tabular{ll}{ Package: \tab primer\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2012-05-16\cr License: \tab GPL version 2 or later\cr
#' LazyLoad: \tab yes\cr }
#'
#' @name primer-package
#' @aliases primer-package primer
#' @docType package
#' @author Hank Stevens <HStevens@@muohio.edu>
#' @seealso \code{\link[primer]{lvcompg}}, \code{\link[deSolve]{ode}}
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}
#' Springer, 2nd printing.
#' @keywords package
NULL





#' Weekly deaths from bubonic plague in Bombay in 1905--06
#'
#' Kermack and McCormick (1927) provided data on the number of plague deaths
#' per week in Bombay in 1905--06 (Bombay is the former name for the Indian
#' coastal city Mumbai. It is the capital of Maharashtra and is one of the
#' largest cities in the world).
#'
#'
#' @name ross
#' @docType data
#' @format A data frame with 32 observations on the following 2 variables.
#' \describe{ \item{Week}{a numeric vector}
#' \item{CumulativeDeaths}{a numeric vector} }
#' @references Kermack, W.O. and McCormick, W.G. (1927) A contribution to the
#' mathematical theory of epidemics. \emph{Proceedings of the Royal Society,
#' Series A}, \bold{115}, 700--721.
#' @source Data provided kindly by S.P. Ellner (Cornell University)
#' @keywords datasets
#' @examples
#'
#' data(ross)
#' str(ross) ; plot(CumulativeDeaths ~ Week, data=ross, type='b')
#'
NULL










#' Song Sparrow Data Set
#'
#' Song Sparrow (\emph{Melospiza melodia}) counts in Darrtown, OH, USA.  From
#' Sauer, J. R., J.E. Hines, and J. Fallon. 2005. The North American Breeding
#' Bird Survey, Results and Analysis 1966--2004. Version 2005.2. USGS Patuxent
#' Wildlife Research Center, Laurel, MD.
#'
#'
#' @name sparrows
#' @docType data
#' @format A data frame with 36 observations on the following 3 variables.
#' \describe{ \item{Year}{a numeric vector} \item{Count}{a
#' numeric vector} \item{ObserverNumber}{a numeric vector} }
#' @seealso \code{\link{PopSim}}
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @source http://www.pwrc.usgs.gov/BBS/
#' @keywords datasets
#' @examples
#'
#' data(sparrows)
#' ## maybe
#' str(sparrows)
#' plot(Count ~ Year, sparrows)
#'
NULL






#' Percent cover of six perennial herbaceous plants
#'
#' Percent cover of six of the most common herbaceous perennial species from
#' the Buell-Small long term succession study (http://www.ecostudies.org/bss/).
#' Data are plot-level visual estimates of percent cover.
#'
#'
#' @name weeds
#' @docType data
#' @format A data frame with 15140 observations on the following 8 variables.
#' \describe{ \item{X}{a numeric vector} \item{Genus}{a factor
#' with levels \code{Aster}, \code{Euthamia}, \code{Solidago}}
#' \item{Epithet}{a factor with levels \code{canadensis},
#' \code{gigantea}, \code{graminifolia}, \code{novae-angliae}, \code{pilosus},
#' \code{rugosa}} \item{FieldName}{a factor with levels \code{C3,}
#' \code{C4}, \code{C5}, \code{C6}, \code{C7}, \code{D1}, \code{D2}, \code{D3},
#' \code{E1}, \code{E2}} \item{Age}{a numeric vector indicating the
#' number of years of succession, since abandonment from agriculture.}
#' \item{PlotId}{a numeric vector} \item{Cover}{a numeric
#' vector; percent cover, estimated visually.} \item{Species}{a factor
#' with levels \code{A.novae-angliae}, \code{A.pilosus}, \code{E.graminifolia},
#' \code{S.canadensis}, \code{S.gigantea}, \code{S.rugosa}} }
#' @references Stevens, M.H.H. (2009) \emph{A Primer of Ecology with R}. Use R!
#' Series. Springer.
#' @source Data graciously provided by Scott Meiners (Eastern Illinois
#' University, http://www.ecostudies.org/bss/).
#' @keywords datasets
#' @examples
#'
#' data(weeds)
#' str(weeds);
#' # lattice::xyplot(Cover ~ Age, data=weeds, groups=Species,
#' # type=c("a"), auto.key=list(lines=TRUE, points=FALSE, columns=3),
#' # ylim=c(-1,20))
#'
NULL



