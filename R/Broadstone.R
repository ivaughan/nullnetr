#' Part of the highly-resolved food web from Broadstone Stream, UK
#'
#' Part of the highly-resolved macroinvertebrate food web from Broadstone Stream
#'   in south-east England (see Woodward \emph{et al}., 2005 for full details of
#'   the web). This data frame contains data collected in August 1996, with 19
#'   macroinvertebrate taxa. Predation was determined via visual gut contents
#'   analysis, with identification of hard parts allowing counts of the number of
#'   individual prey organisms each individual predator consumed.
#'   There are three accompanying data sets:
#'   \enumerate{
#'     \item \code{Broadstone.prey} Gives the total abundance of each prey taxon
#'       from 30 Surber samples collected at the same time as the interaction data.
#'     \item \code{Broadstone.fl} Specifies 'forbidden' links which are not
#'       allowed in the null model.
#'     \item \code{Broadstone.nodes} The abundance and mean body mass for each
#'       taxon in the food web in a format for use alongside a
#'       \code{generate_null_model} object with the \code{cheddar} package
#'       (Hudson \emph{et al}., 2013). Body size is taken from cheddar's
#'       'BroadstoneStream' data set.
#'   }
#' @format A data frame with 319 rows and 20 columns. Each row represents the gut
#'   contents of an individual predator. There are 20 columns in total, with the
#'   first column (\code{Predator}) indicating the predator taxon (seven taxa in
#'   total) and the remaining 19 columns indicating the number of individuals eaten
#'   of each of the 19 potential prey taxa.
#'
#' @source Woodward, G., Speirs, D.C. & Hildrew, A.G. (2005) Quantification and
#' resolution of a complex, size-structured food web. \emph{Advances in
#' Ecological Research}, \strong{36}, 84--135.

"Broadstone"
