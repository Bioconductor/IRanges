\name{RangedData-class}
\docType{class}

\alias{class:RangedData}
\alias{RangedData-class}

% Accessors:
\alias{nrow,RangedData-method}
\alias{ncol,RangedData-method}
\alias{rownames,RangedData-method}
\alias{colnames,RangedData-method}
\alias{rownames<-,RangedData-method}
\alias{colnames<-,RangedData-method}
\alias{elementLengths,RangedData-method}
\alias{end,RangedData-method}
\alias{end<-,RangedData-method}
\alias{length,RangedData-method}
\alias{names,RangedData-method}
\alias{names<-,RangedData-method}
\alias{ranges,RangedData-method}
\alias{ranges<-,RangedData-method}
\alias{start,RangedData-method}
\alias{start<-,RangedData-method}
\alias{values,RangedData-method}
\alias{values<-,RangedData-method}
\alias{width,RangedData-method}
\alias{width<-,RangedData-method}
\alias{space,RangedData-method}
\alias{universe,RangedData-method}
\alias{universe<-,RangedData-method}
\alias{score,RangedData-method}
\alias{score<-,RangedData-method}
\alias{columnMetadata,RangedData-method}
\alias{columnMetadata<-,RangedData-method}

% Constructor:
\alias{RangedData}

% Coercion:
\alias{as.data.frame,RangedData-method}
\alias{coerce,RangedData,DataFrame-method}
\alias{coerce,Rle,RangedData-method}
\alias{coerce,RleList,RangedData-method}
\alias{coerce,RleViewsList,RangedData-method}
\alias{coerce,Ranges,RangedData-method}
\alias{coerce,RangesList,RangedData-method}
\alias{coerce,RangedData,CompressedIRangesList-method}
\alias{coerce,RangedData,IRangesList-method}
\alias{coerce,RangedData,RangesList-method}
\alias{as.env,RangedData-method}
\alias{coerce,data.frame,RangedData-method}
\alias{coerce,DataTable,RangedData-method}

% Combining and splitting
\alias{c,RangedData-method}
\alias{rbind,RangedData-method}
\alias{split,RangedData-method}

% Subsetting:
\alias{[,RangedData-method}
\alias{[<-,RangedData-method}
\alias{[[,RangedData-method}
\alias{[[<-,RangedData-method}
\alias{$<-,RangedData-method}
\alias{seqselect,RangedData-method}
\alias{seqselect<-,RangedData-method}
\alias{window<-,RangedData-method}

% Utilities:
\alias{within,RangedData-method}

% Applying:
\alias{endoapply,RangedData-method}
\alias{lapply,RangedData-method}

% Show:
\alias{show,RangedData-method}

\title{Data on ranges}
\description{
  \code{RangedData} supports storing data, i.e. a set of variables, on a
  set of ranges spanning multiple spaces (e.g. chromosomes). Although
  the data is split across spaces, it can still be treated as one
  cohesive dataset when desired and extends \code{\linkS4class{DataTable}}. In
  order to handle large datasets, the data values are stored externally
  to avoid copying, and the \code{\link{rdapply}} function facilitates
  the processing of each space separately (divide and conquer).
}

\details{
  A \code{RangedData} object consists of two primary components:
  a \code{\linkS4class{RangesList}} holding the ranges over multiple
  spaces and a parallel \code{\linkS4class{SplitDataFrameList}},
  holding the split data. There is also an \code{universe} slot
  for denoting the source (e.g. the genome) of the ranges and/or
  data.

  There are two different modes of interacting with a
  \code{RangedData}. The first mode treats the object as a contiguous
  "data frame" annotated with range information. The accessors
  \code{start}, \code{end}, and \code{width} get the corresponding
  fields in the ranges as atomic integer vectors, undoing the division
  over the spaces. The \code{[[} and matrix-style \code{[,} extraction
  and subsetting functions unroll the data in the same way. \code{[[<-}
  does the inverse. The number
  of rows is defined as the total number of ranges and the number of
  columns is the number of variables in the data. It is often convenient
  and natural to treat the data this way, at least when the data is
  small and there is no need to distinguish the ranges by their space.

  The other mode is to treat the \code{RangedData} as a list, with an
  element (a virtual \code{\linkS4class{Ranges}}/\code{\linkS4class{DataFrame}}
  pair) for each space. The length of the object is defined as the number
  of spaces and the value returned by the \code{names} accessor gives the
  names of the spaces. The list-style \code{[} subset function behaves
  analogously. The \code{rdapply} function provides a convenient and
  formal means of applying an operation over the spaces separately. This
  mode is helpful when ranges from different spaces must be treated
  separately or when the data is too large to process over all spaces at
  once.
}

\section{Accessor methods}{
  In the code snippets below, \code{x} is a \code{RangedData} object.

  The following accessors treat the data as a contiguous dataset,
  ignoring the division into spaces:
  \describe{
    \item{}{Array accessors:
      \describe{
        \item{}{
          \code{nrow(x)}: The number of ranges in \code{x}.
        }
        \item{}{
          \code{ncol(x)}: The number of data variables in \code{x}.
        }
        \item{}{
          \code{dim(x)}: An integer vector of length two, essentially
          \code{c(nrow(x), ncol(x))}.
        }
        \item{}{
          \code{rownames(x)}, \code{rownames(x) <- value}: Gets or sets
          the names of the ranges in \code{x}.
        }
        \item{}{
          \code{colnames(x)}, \code{colnames(x) <- value}: Gets the
          names of the variables in \code{x}.
        }
        \item{}{
          \code{dimnames(x)}: A list with two elements, essentially
          \code{list(rownames(x), colnames(x))}.
        }
        \item{}{
          \code{dimnames(x) <- value}: Sets the row and column names,
          where value is a list as described above.
        }
        \item{}{\code{columnMetadata(x)}: Get the \code{DataFrame} of
          metadata along the value columns, i.e., where each column in
          \code{x} is represented by a row in the metadata. Note that
          calling \code{mcols(x)} returns the metadata on each
          space in \code{x}.
        }
        \item{}{\code{columnMetadata(x) <- value}: Set the \code{DataFrame}
          of metadata for the columns.
        }
        \item{}{\code{within(data, expr, ...)}: Evaluates \code{expr}
          within \code{data}, a \code{RangedData}. Any values assigned
          in \code{expr} will be stored as value columns in \code{data},
          unless they match one of the reserved names: \code{ranges},
          \code{start}, \code{end}, \code{width} and
          \code{space}. Behavior is undefined if any of the range
          symbols are modified inconsistently. Modifications
          to \code{space} are ignored.
        }
      }
    }
    \item{}{Range accessors. The type of the return value depends on
      the type of \code{\linkS4class{Ranges}}. For \code{\linkS4class{IRanges}},
      an integer vector. Regardless, the number of elements is always equal to
      \code{nrow(x)}.
      \describe{
        \item{}{
          \code{start(x), start(x) <- value}: Get or set the starts of the
           ranges. When setting the starts, \code{value} can be an integer
           vector of \code{length(sum(elementLengths(ranges(x))))} or an
           IntegerList object of length \code{length(ranges(x))} and names
           \code{names(ranges(x))}.
        }
        \item{}{
          \code{end(x), end(x) <- value}: Get or set the ends of the
           ranges. When setting the ends, \code{value} can be an integer
           vector of \code{length(sum(elementLengths(ranges(x))))} or an
           IntegerList object of length \code{length(ranges(x))} and names
           \code{names(ranges(x))}.
        }
        \item{}{
          \code{width(x), width(x) <- value}: Get or set the widths of the
           ranges. When setting the widths, \code{value} can be an integer
           vector of \code{length(sum(elementLengths(ranges(x))))} or an
           IntegerList object of length \code{length(ranges(x))} and names
           \code{names(ranges(x))}.
        }
      }
    }
  }

  These accessors make the object seem like a list along the spaces:
  \describe{
    \item{}{
      \code{length(x)}:
      The number of spaces (e.g. chromosomes) in \code{x}.
    }
    \item{}{
      \code{names(x)}, \code{names(x) <- value}: Get or set the names of
      the spaces (e.g. \code{"chr1"}). 
      \code{NULL} or a character vector of the same length as \code{x}.
    }
  }

  Other accessors:
  \describe{
    \item{}{
      \code{universe(x)}, \code{universe(x) <- value}: Get or set the
      scalar string identifying the scope of the data in some way (e.g.
      genome, experimental platform, etc). The universe may be \code{NULL}.
    }
    \item{}{
      \code{ranges(x), ranges(x) <- value}: Gets or sets the ranges in
      \code{x} as a \code{\linkS4class{RangesList}}.
    }
    \item{}{
      \code{space(x)}: Gets the spaces from \code{ranges(x)}.
    }
    \item{}{
      \code{values(x), values(x) <- value}: Gets or sets the data values in
      \code{x} as a \code{\linkS4class{SplitDataFrameList}}.
    }
    \item{}{
      \code{score(x), score(x) <- value}: Gets or sets the column
      representing a "score" in \code{x}, as a vector. This is the column
      named \code{score}, or, if this does not exist, the first column, if it
      is numeric. The get method return \code{NULL} if no suitable score
      column is found. The set method takes a numeric vector as its value.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{RangedData(ranges = IRanges(), ..., space = NULL,
        universe = NULL)}:
      Creates a \code{RangedData} with the ranges in \code{ranges} and
      variables given by the arguments in \code{...}.  See the
      constructor \code{\linkS4class{DataFrame}} for how the \code{...}
      arguments are interpreted.

      If \code{ranges} is a \code{\linkS4class{Ranges}} object, the
      \code{space} argument is used to split of the data into spaces.
      If \code{space} is \code{NULL}, all of the ranges and values are
      placed into the same space, resulting in a single-space (length one)
      \code{RangedData} object. Otherwise, the ranges and values are split
      into spaces according to \code{space}, which is treated as a factor,
      like the \code{f} argument in \code{\link{split}}.

      If \code{ranges} is a \code{\linkS4class{RangesList}} object, then
      the supplied \code{space} argument is ignored and its value is derived
      from \code{ranges}.

      If \code{ranges} is not a \code{\linkS4class{Ranges}} or
      \code{\linkS4class{RangesList}} object, this function calls
      \code{as(ranges, "RangedData")} and returns the result if successful.

      The universe may be specified as a scalar string by the \code{universe}
      argument.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as.data.frame(x, row.names=NULL, optional=FALSE, ...)}:
      Copy the start, end, width of the ranges and all of the variables
      as columns in a \code{data.frame}. This is a bridge to existing
      functionality in R, but of course care must be taken if the data
      is large. Note that \code{optional} and \code{...} are ignored.
    }
    \item{}{
      \code{as(from, "DataFrame")}: Like \code{as.data.frame} above,
      except the result is an \code{\linkS4class{DataFrame}} and it
      probably involves less copying, especially if there is only a
      single space.
    }
    \item{}{
      \code{as(from, "RangedData")}: Coerce \code{from} to
      a \code{RangedData}, according to the type of \code{from}:
      \describe{
        \item{\code{\linkS4class{Rle}}, \code{\linkS4class{RleList}}}{
          Converts each run to a range and stores the run values in a
          column named "score".
        }
        \item{\code{\linkS4class{RleViewsList}}}{
          Creates a \code{RangedData} using the ranges given by the runs
          of \code{subject(from)} in each of the windows, with a value column
          \code{score} taken as the corresponding subject values.
        }
        \item{\code{\linkS4class{Ranges}}}{
          Creates a \code{RangedData} with only the ranges in \code{from};
          no data columns.
        }
        \item{\code{\linkS4class{RangesList}}}{
          Creates a \code{RangedData} with the ranges in \code{from}.
          Also propagates the \emph{inner} metadata columns of the
          \code{RangesList} (accessed with \code{mcols(unlist(from))})
          to the data columns (aka values) of the \code{RangedData}.
          This makes it a \emph{lossless} coercion and the exact reverse of the
          coercion from \code{RangedData} to \code{RangesList}.
        }
        \item{\code{data.frame} or \code{DataTable}}{Constructs a
          \code{RangedData}, using the columns \dQuote{start},
          \dQuote{end}, and, optionally, \dQuote{space} columns in
          \code{from}. The other columns become data columns in the
          result. Any \dQuote{width} column is ignored.
        }
      }
    }
    \item{}{
      \code{as(from, "RangesList")}:
      Creates a \code{CompressedIRangesList} (a subclass of \code{RangesList})
      made of the ranges in \code{from}.
      Also propagates the data columns (aka values) of the \code{RangedData}
      to the inner metadata columns of the \code{RangesList}.
      This makes it a \emph{lossless} coercion and the exact reverse of the
      coercion from \code{RangesList} to \code{RangedData}.
    }
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an \code{environment} with a symbol for each variable in
      the frame, as well as a \code{ranges} symbol for
      the ranges. This is efficient, as no copying is performed.
    }
  }
}

\section{Subsetting and Replacement}{
  In the code snippets below, \code{x} is a \code{RangedData} object.

  \describe{
    \item{}{
      \code{x[i]}:
      Subsets \code{x} by indexing into its spaces, so the
      result is of the same class, with a different set of spaces.
      \code{i} can be numerical, logical, \code{NULL} or missing.
    }
    \item{}{
      \code{x[i,j]}:
      Subsets \code{x} by indexing into its rows and columns. The result
      is of the same class, with a different set of rows and columns.
      The row index \code{i} can either treat \code{x} as a flat table
      by being a character, integer, or logical vector or treat \code{x}
      as a partitioned table by being a \code{\linkS4class{RangesList}},
      \code{\linkS4class{LogicalList}}, or \code{\linkS4class{IntegerList}}
      of the same length as \code{x}.
    }
    \item{}{
      \code{x[[i]]}:
      Extracts a variable from \code{x}, where \code{i} can be
      a character, numeric, or logical scalar that indexes into the
      columns. The variable is unlisted over the spaces.

      For convenience, values of \code{"space"} and \code{"ranges"}
      are equivalent to \code{space(x)} and \code{unlist(ranges(x))}
      respectively. 
    }
    \item{}{
      \code{x$name}: similar to above, where \code{name} is taken
      literally as a column name in the data.
    }
    \item{}{
      \code{x[[i]] <- value}:
      Sets value as column \code{i} in \code{x}, where \code{i} can be
      a character, numeric, or logical scalar that indexes into the
      columns. The length of \code{value} should equal
      \code{nrow(x)}. \code{x[[i]]} should be identical to \code{value}
      after this operation.

      For convenience, \code{i="ranges"} is equivalent to
      \code{ranges(x) <- value}. 
    }
    \item{}{
      \code{x$name <- value}: similar to above, where \code{name} is taken
      literally as a column name in the data.
    }
  }
}

\section{Splitting and Combining}{
  In the code snippets below, \code{x} is a \code{RangedData} object.
  
  \describe{
    \item{}{
      \code{split(x, f, drop = FALSE)}: Split \code{x} according to
      \code{f}, which should be of length equal to \code{nrow(x)}. Note
      that \code{drop} is ignored here. The result is a
      \code{\linkS4class{RangedDataList}} where every element has the same 
      length (number of spaces) but different sets of ranges within each
      space.
    }
    \item{}{
      \code{rbind(...)}: Matches the spaces from
      the \code{RangedData} objects in \code{...} by name and combines
      them row-wise. In a way, this is the reverse of the \code{split}
      operation described above.
    }
    \item{}{
      \code{c(x, ..., recursive = FALSE)}: Combines \code{x} with
      arguments specified in \code{...}, which must all be
      \code{RangedData} objects. This combination acts as if \code{x} is
      a list of spaces, meaning that the result will contain the spaces
      of the first concatenated with the spaces of the second, and so
      on. This function is useful when creating \code{RangedData}
      objects on a space-by-space basis and then needing to
      combine them.
    }
  }
}

\section{Applying}{
  There are two ways explicitly supported ways to apply a function over
  the spaces of a \code{RangedData}. The richest interface is
  \code{\link{rdapply}}, which is described in its own man page. The
  simpler interface is an \code{lapply} method:
  \describe{
    \item{}{\code{lapply(X, FUN, ...)}:
      Applies \code{FUN} to each space in \code{X} with extra parameters
      in \code{...}.
    }
  }
}

\author{ Michael Lawrence }

\seealso{
  \linkS4class{DataTable}, the parent of this class, with more utilities.
  The \code{\link{rdapply}} function for applying a function to each space
  separately.
}

\examples{
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)

  ## constructing RangedData instances

  ## no variables
  rd <- RangedData()
  rd <- RangedData(ranges)
  ranges(rd)
  ## one variable
  rd <- RangedData(ranges, score)
  rd[["score"]]
  ## multiple variables
  rd <- RangedData(ranges, filter, vals = score)
  rd[["vals"]] # same as rd[["score"]] above
  rd$vals
  rd[["filter"]]
  rd <- RangedData(ranges, score + score)
  rd[["score...score"]] # names made valid
  ## use a universe
  rd <- RangedData(ranges, universe = "hg18")
  universe(rd)

  ## split some data over chromosomes

  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  both <- c(ranges, range2)
  score <- c(score, c(0L, 3L, NA, 22L))
  filter <- c(filter, c(0L, 1L, NA, 0L)) 
  chrom <- paste("chr", rep(c(1,2), c(length(ranges), length(range2))), sep="")

  rd <- RangedData(both, score, filter, space = chrom, universe = "hg18")
  rd[["score"]] # identical to score
  rd[1][["score"]] # identical to score[1:3]
  
  ## subsetting

  ## list style: [i]

  rd[numeric()] # these three are all empty
  rd[logical()]
  rd[NULL]
  rd[] # missing, full instance returned
  rd[FALSE] # logical, supports recycling
  rd[c(FALSE, FALSE)] # same as above
  rd[TRUE] # like rd[]
  rd[c(TRUE, FALSE)]
  rd[1] # numeric index
  rd[c(1,2)]
  rd[-2]

  ## matrix style: [i,j]

  rd[,NULL] # no columns
  rd[NULL,] # no rows
  rd[,1]
  rd[,1:2]
  rd[,"filter"]
  rd[1,] # now by the rows
  rd[c(1,3),]
  rd[1:2, 1] # row and column
  rd[c(1:2,1,3),1] ## repeating rows

  ## dimnames

  colnames(rd)[2] <- "foo"
  colnames(rd)
  rownames(rd) <- head(letters, nrow(rd))
  rownames(rd)

  ## space names

  names(rd)
  names(rd)[1] <- "chr1"

  ## variable replacement

  count <- c(1L, 0L, 2L)
  rd <- RangedData(ranges, count, space = c(1, 2, 1))
  ## adding a variable
  score <- c(10L, 2L, NA)
  rd[["score"]] <- score
  rd[["score"]] # same as 'score'
  ## replacing a variable
  count2 <- c(1L, 1L, 0L)
  rd[["count"]] <- count2
  ## numeric index also supported
  rd[[2]] <- score
  rd[[2]] # gets 'score'
  ## removing a variable
  rd[[2]] <- NULL
  ncol(rd) # is only 1
  rd$score2 <- score
  
  ## combining/splitting

  rd <- RangedData(ranges, score, space = c(1, 2, 1))
  c(rd[1], rd[2]) # equal to 'rd'
  rd2 <- RangedData(ranges, score)
  unlist(split(rd2, c(1, 2, 1))) # same as 'rd'

  ## applying

  lapply(rd, `[[`, 1) # get first column in each space
}

\keyword{methods}
\keyword{classes}
