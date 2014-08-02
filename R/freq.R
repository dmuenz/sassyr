## Created by Daniel Muenz on Dec 9, 2013
## TODO:
## * let order param take list of integer vectors, manually controlling
##   sort order for each variable
## * add parameter to let percents be calculated within subgroups


#' Frequency and Percent Tabulations for Data Frames and Vectors
#'
#' \code{freq} performs (cross-)tabulations for all the data frames
#' and vectors passed to \dots.  By default, each variable is tabulated
#' separately; use \code{cross=TRUE} for cross-tabulations.  Unlike with
#' \code{table}, \code{freq} returns frequencies, percents, and cumulative
#' frequencies and percents (similar to PROC FREQ in SAS).
#'
#' \code{freq} is basically a wrapper around \code{table}. It was inspired by PROC
#' FREQ in SAS, and similarly displays frequences, percents, and their cumulative
#' cousins.  It's useful for quickly assessing the content of a data frame or a set
#' of variables.
#'
#' @aliases freq print.freq as.data.frame.freq dim.freq as.data.frame.freq
#'
#' @param \dots variables to tabulate; can contain data.frames and vectors.
#'   If \code{cross==TRUE}, then variables must all have the same length.
#'   (For S3 methods, \dots is unused.)
#' @param cross logical: whether to cross-tabulate variables. Default is FALSE.
#' @param useNA TRUE/FALSE/NULL: how to include NA's in table. FALSE (default) says
#'   display NA's but ignore them in calculating percents; TRUE says use
#'   them in calculating percents; and NULL says ignore them entirely
#' @param order "value"/"freq": controls order of freq tables. "value" (default)
#'   sorts by variable values; "freq" sorts by descending frequency.
#'   You can specify just the initial letter.
#' @param x an object of class "freq", as returned by \code{freq}.
#' @param decimals integer indicating the number of decimal places to be used when
#'   printing percentages.
#' @param big.mark character: if not empty, used as mark between every 3 digits
#'   before the decimal point, when printing frequencies.
#'
#' @return
#' \code{freq} returns an object of class "freq". If \code{cross=TRUE}, the
#' object is essentially a data frame. It will contain a column for each variable
#' being cross-tabulated and four additional columns for the tabulations.  If
#' \code{cross=FALSE}, the returned object will contain a list of data frames,
#' one per variable.
#'
#' \code{as.data.frame} returns a single data frame. If \code{cross=FALSE} was
#' used, so that the "freq" object contains a list of data frames, then they are
#' vertically concatenated.
#'
#' @seealso \code{\link{table}}, \code{\link{summary}}
#' @keywords utilities print documentation
#' @author Daniel Muenz
#' @export
#' @name freq
#' @examples
#' # sample from 1:10 and look at the distribution
#' freq(x=sample(1:10,200,replace=TRUE))
#'
#' # make a data frame and vector for use below
#' n <- 150
#' d <- data.frame(x=sample(2,n,TRUE), y=sample(letters[1:3],n,TRUE))
#' is.na(d$x[sample(nrow(d),20)]) <- TRUE
#' is.na(d$y[sample(nrow(d),10)]) <- TRUE
#' z <- sample(c("i","ii","iii","iv"),n,TRUE)
#'
#' freq(d) # gets freqs for all vars in d
#' freq(d, z) # all vars in d and z
#'
#' freq(d, cross=TRUE, useNA=NULL) # coss-tab the vars in d, ignoring NAs
#'
#' as.data.frame(freq(d)) # combine output from univariate freq into 1 data frame
NULL

#' @rdname freq
#' @export
freq <- function(..., cross=FALSE, useNA=FALSE, order=c("value","freq")) {

  lengths <- unlist(sapply(list(...), function(x) {
    if (is.data.frame(x))
      rep(nrow(x), ncol(x))
    else if (is.atomic(x))
      length(x)
    else
      stop("... must contain only data.frames and vectors")
  }))
  if (cross && any(lengths != lengths[1]))
    stop("When cross==TRUE, variables in ... must all have same length")
    
  order <- match.arg(order)

  do.the.freq <- function(df) {
    df <- data.frame(df)
    df <- df[do.call(base::order,df),,drop=FALSE]
    x <- data.frame(table(rev(df), useNA="ifany"))
    x <- x[x$Freq != 0,,drop=FALSE]
    x <- data.frame(unique(df), Freq=x$Freq)

    if (length(order)==1 && is.character(order) && order=="freq")
      x <- x[order(-x$Freq),]

    n <- sum(x$Freq)
    n.complete <- sum(na.omit(x)$Freq)
    n.miss <- n - n.complete

    if (is.null(useNA)) {
      x <- na.omit(x)
      x$Percent <- x$Freq / n.complete * 100
      x$cumFreq <- cumsum(x$Freq)
      x$cumPercent <- x$cumFreq / n.complete * 100
    } else if (!useNA) {
      cc.ids <- complete.cases(x)
      x$cumPercent <- x$cumFreq <- x$Percent <- NA
      x$Percent[cc.ids] <- x$Freq[cc.ids] / n.complete * 100
      x$cumFreq[cc.ids] <- cumsum(x$Freq[cc.ids])
      x$cumPercent[cc.ids] <- x$cumFreq[cc.ids] / n.complete * 100
    } else {
      x$Percent <- x$Freq / n * 100
      x$cumFreq <- cumsum(x$Freq)
      x$cumPercent <- x$cumFreq / n * 100
    }
    
    row.names(x) <- NULL
    attr(x, "n") <- n
    attr(x, "n.miss") <- n.miss
    x
  }

  df <- data.frame(...)

  if (cross)
    x <- do.the.freq(df)
  else {
    df <- mapply(function(var,len) var[1:len], df, lengths, SIMPLIFY=F)
    x <- lapply(df, do.the.freq)
    for (name in names(x))
      names(x[[name]])[1] <- name
  }

  class(x) <- "freq"
  attr(x, "cross") <- cross
  x
}

#' @rdname freq
#' @export
as.data.frame.freq <- function(x, ...) {
  if (attr(x, "cross"))
    as.data.frame(unclass(x))
  else {
    l <- lapply(x, as.data.frame)
    l.vars <- lapply(l, function(y) y[,1])
    l.stats <- lapply(l, function(y) y[,-1])

    n.rows <- sapply(l,nrow)
    n <- sum(n.rows)

    Var <- rep(names(l), times=n.rows)
    Value <- unlist(lapply(l.vars, as.character))
    Value[is.na(Value)] <- "NA"
    df.vars <- data.frame(lapply(l.vars, rep, length=n))
    df.stats <- do.call("rbind", l.stats)

    df <- data.frame(Var,Value,df.vars,df.stats, stringsAsFactors=F)
    row.names(df) <- NULL
    
    for (name in names(x)) {
      df[Var==name,name] <- l.vars[[name]]
      df[Var!=name,name] <- NA
    }

    df
  }
}

#' @rdname freq
#' @export
dim.freq <- function(x) {
  if (attr(x, "cross"))
    dim(as.data.frame(x))
  else
    lapply(x, function(y) dim(as.data.frame(y)))
}

#' @rdname freq
#' @export
print.freq <- function(x, decimals=2, big.mark=",", ...) {
  cross <- attr(x, "cross")
  first <- TRUE

  do.the.print <- function(x) {
    n <- attr(x, "n")
    n.miss <- attr(x, "n.miss")

    x <- as.data.frame(x)

    x$Freq <- prettyNum(x$Freq, big.mark=big.mark)
    x$cumFreq <- prettyNum(x$cumFreq, big.mark=big.mark)
    x$Percent <- round(x$Percent, decimals)
    x$cumPercent <- round(x$cumPercent, decimals)

    if (!first)
      cat(rep("-",60),"\n",sep="")
    first <<- FALSE

    if (cross)
      cat("Freq table for",paste(names(x)[1:(ncol(x)-4)],collapse=" * "),"\n\n")
    else
      cat("Freq table for",names(x)[1],"\n\n")

    print(x)
    cat("\nThere are",n.miss,"obs. with missing values out of",n,"\n")
  }
  
  if (cross)
    do.the.print(x)
  else
    lapply(x, do.the.print)
}
