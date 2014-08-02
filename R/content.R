#' Summarize All Variables in a Data Frame
#'
#' This function prints an easy-to-read summary of a data frame.  Each
#' variable takes up only one line of output, allowing much information
#' to be quickly gleaned.  This is similar to \code{\link[utils]{str}}.
#'
#' The output starts by listing the number of variables and observations. It then
#' displays a table with one row per variable. The 1st column is the variable name,
#' optionally followed by the class, and then an abbreviated list of values.
#'
#' @param data a data frame to reorder
#' @param sep a character string to separate printed values for a variable
#' @param class logical: whether to display the class of each variable
#' @param print.width an integer indicating the max number of characters to
#'   print on a line
#' @param print logical: whether to print anything
#'
#' @return
#' Invisibly returns a data frame with the summary.  This has special
#' attributes "nrow", "ncol", "nrow.miss", and "ncol.miss", containing the
#' number of rows, columns, rows with missing values, and columns with
#' missing values in \code{data}, respectively.
#'
#' @seealso \code{\link[utils]{str}}, \code{\link{summary}}
#' @keywords utilities print documentation
#' @author Daniel Muenz
#' @export
#' @examples
#' content(ChickWeight)

content <-
function(data, sep=",", class=TRUE, print.width=getOption("width"), print=TRUE) {
  #require(Rcpp)
  #sourceCpp("~/Documents/Code/R/lib/findSeqs.cpp")
	
  data <- as.data.frame(data)
  r <- nrow(data)
  c <- ncol(data)

  ## number of rows and cols with missing data
  r.miss <- r - nrow(na.omit(data))
  c.miss <- ifelse(r.miss == 0, 0, sum(sapply(data, function(x) any(is.na(x)))))
  
  if (print) {
    if (r.miss == 0) {
      cat("Observations:", r, "(no missing values)\n")
      cat("Variables:", c, "\n\n")
    } else {
      cat("Observations: ", r, " (", r.miss, " with missing values)\n", sep="")
      cat("Variables: ", c, " (", c.miss, " with missing values)\n\n", sep="")
    }
  }

  ## variable names
  var.names <- colnames(data)

  ## count widths needed to display variable numbers and names
  num.len <- ceiling(log10(c))
  name.len <- max(nchar(var.names))

  ## calculate the horizontal screen space available for printing values
  max.width <- print.width - num.len - name.len - 6

  ## get the class of each variable
  classes <- sapply(data, function(x) paste(class(x),collapse="-"))
  names(classes) <- NULL

  ## abbreviate the class names
  class.full <- c("factor","numeric","integer","character","ordered")
  class.abbr <- c("fac",   "num",    "int",    "char",     "o")
  for (i in 1:length(class.full))
    classes <- gsub(class.full[i], class.abbr[i], classes)

  ## figure out how much space we need for printing the class
  ## and adjust the space left for printing values
  if (class) {
    class.len <- max(nchar(classes))
    max.width <- max.width - class.len - 1
  }

  sep.len <- nchar(sep)

  values <- rep("", c)

  ## for each  variable, print the #, name, class, and values
  for (i in 1:c) {
    ## sort variable values and remove duplicates
    uniq <- sort(unique(data[,i]))
    
    ## try to abbreviate integer variables by finding arithmetic
    ## subsequences with constant difference 1.  For example, if
    ## the variable has values 1,3,4,5,6,8,9, we abbreviate this
    ## to 1,3:6,8:9.
    if (classes[i] == "int" || (classes[i] == "num" && all(uniq == as.integer(uniq))))
      vals <- findSeqs(uniq)
    else
      vals <- as.character(uniq)
    
    if (any(is.na(data[,i]))) vals <- c(vals, "NA")

    ## put quotes around values that contain sep
    if (classes[i] %in% c("fac","o-fac","char") & any(has.sep <- grepl(sep,vals)))
      vals[has.sep] <- paste('"', vals[has.sep], '"', sep="")

    valsx <- paste(vals, collapse=sep)
    if (nchar(valsx) <= max.width)
      values[i] <- valsx
    else {
      ## print only as many values as will fit on a line
      l.vals <- c()
      r.vals <- c()
      curr.len <- 0
      done <- F
      while (!done) {
        done <- T

        new.l <- vals[1]
        new.l.len <- nchar(new.l)
        if (curr.len + 2*sep.len + new.l.len + 3 <= max.width) {
          l.vals <- c(l.vals, new.l)
          vals <- vals[-1]
          curr.len <- curr.len + sep.len + new.l.len
          done <- F
        }
      
        new.r <- vals[n.remaining <- length(vals)]
        new.r.len <- nchar(new.r)
        if (curr.len + 2*sep.len + new.r.len + 3 <= max.width) {
          r.vals <- c(new.r, r.vals)
          vals <- vals[-n.remaining]
          curr.len <- curr.len + sep.len + new.r.len
          done <- F
        }
      }
      values[i] <- paste(paste(l.vals,collapse=sep),"...",paste(r.vals,collapse=sep),sep=sep)
    }

  	if (print) {
      cat(format(i, width=num.len, justify="right"), "")
      cat(format(var.names[i],width=name.len), "")
      if (class) cat(format(classes[i], width=class.len), "")
      cat(values[i],"\n",sep="")
    }
  }
  
  ret <- data.frame(name=var.names, class=classes, values=values)
  attr(ret, "nrow") <- r
  attr(ret, "ncol") <- c
  attr(ret, "nrow.miss") <- r.miss
  attr(ret, "ncol.miss") <- c.miss
  invisible(ret)
}
