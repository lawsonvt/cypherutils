#' SQL quote transformer
#' 
#' Code copied from glue package to mimic `glue_sql` but for cypher.
#' Not meant to be called by itself
#' 
#' @param connection A dummy DBI connection
#' @param .na The string to be evaluated
#' 
#' @import DBI

sql_quote_transformer <- function(connection, .na) {
  if (is.null(.na)) {
    .na <- DBI::SQL(NA)
  }
  
  function(text, envir) {
    should_collapse <- grepl("[*]$", text)
    if (should_collapse) {
      text <- sub("[*]$", "", text)
    }
    m <- gregexpr("^`|`$", text)
    is_quoted <- any(m[[1]] != -1)
    if (is_quoted) {
      regmatches(text, m) <- ""
      res <- eval(parse(text = text, keep.source = FALSE), envir)
      
      if (length(res) == 1) {
        res <- DBI::dbQuoteIdentifier(conn = connection, res)
      } else {
        
        # Support lists as well
        res[] <- lapply(res, DBI::dbQuoteIdentifier, conn = connection)
      }
    } else {
      res <- eval(parse(text = text, keep.source = FALSE), envir)
      if (inherits(res, "SQL")) {
        if (should_collapse) {
          res <- glue_collapse(res, ", ")
        }
        return(res)
      }
      
      # convert objects to characters
      is_object <- is.object(res)
      if (is_object) {
        res <- as.character(res)
      }
      
      is_na <- is.na(res)
      if (any(is_na)) {
        res[is_na] <- rep(list(.na), sum(is_na))
      }
      
      is_char <- vapply(res, function(x) !is.na(x) && is.character(x), logical(1))
      res[is_char] <- lapply(res[is_char], function(x) DBI::dbQuoteLiteral(conn = connection, x))
      res[!is_char] <- lapply(res[!is_char], function(x) DBI::SQL(conn = connection, x))
    }
    if (should_collapse) {
      res <- glue_collapse(res, ", ")
    }
    if (length(res) == 0L) {
      res <- DBI::SQL("NULL")
    }
    res
  }
}

#' Using glue to make a cypher query
#' 
#' In the same vein as `glue_sql`, create a cypher query
#' using the start `<<` and end `>>` symbols to reference
#' variables. Will also use the `*` symbol to insert a list of
#' values.
#' 
#' @param ... One or more character strings
#' 
#' @return A string in which the values of variables have been dereferenced
#' 
#' @import glue
#' @import DBI
#' 
#' @export

glue_cypher <- function(...) {
  
  e <- parent.frame()
  
  glue(..., .open="<<", .close=">>", .envir = e,
       .transformer = sql_quote_transformer(connection = ANSI(), .na=DBI::SQL("NULL")))
}
