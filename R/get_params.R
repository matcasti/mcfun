#' @title Get outermost symbol from input parameters inside functions
#'
#' @param x Input argument to be substituted by its symbol
#' @param env Top environment to stop searching for `x`
#'
#' @return Character vector
#' @export
get_params <- function(x, env) {
  .call <- quote(substitute(x))
  .name <- eval(.call)
  .envs <- rev(x = sys.frames())
  if (!missing(env) && is.environment(env)) {
    to_enclos <- vapply(.envs, identical, env, FUN.VALUE = NA)
    if (length(to_enclos) > 1L) {
      to_enclos <- seq_len(length.out = which(to_enclos))
      .envs <- .envs[to_enclos]
    }
  }
  for (i in .envs) {
    .call[[2L]] <- .name
    .name <- eval(.call, i)
  }
  if (is.null(.name)) {
    return(NULL)
  }
  .name <- as.list(.name)
  if (length(.name) > 1L) {
    .name <- .name[-1L]
  }
  .name <- lapply(.name, as.character)
  .name <- as.character(.name)
  return(.name)
}
