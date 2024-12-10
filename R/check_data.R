#' Read ADMB .rep file and return an R object of type 'list'
#'
#' Code modified from original function provided by Steve Martell, D'Arcy N. Webber

#' called by \code{\link{read_admb_re}}
#'
#' @param fn full path and name of ADMB output file to be read
#'
#' @return object of type "list" with ADMB outputs therein
#' @export
#'
#' @examples
#' \dontrun{
#' read_rep(fn = 'inst/example_data/goasr.rep')
#' }
check_data <- function(fn) {
 ## ifelse for sex and colheads
  ## check dims < 10, 5 for both
  ## throw plots if true
}
