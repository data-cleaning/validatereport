#' @name STS
#' @aliases STS
#' @title Short Term Statistics (STS) data
#' @description 
#'   Mockup of a STS file in the format it is exchanged between national
#'   statistical institutes and Eurostat. All numbers are figmented and they
#'   do not represent actual statistics. The \code{STS} data is a \code{list}
#'   with two elements:
#'
#' \itemize{
#'   \item{ \code{STS$data}: A number of STS time series, in `long format'.}
#'   \item{ \code{STS$meta}: metadata on the file envelope; a single-row data frame.}
#' }
#'
#' @section STS data:
#' An informal description of the \code{STS$data} columns follows below. For
#' a formal description including code lists refer to the formal
#' data structure definition (DSD) in 
#' \href{https://circabc.europa.eu/sd/a/adfce7f9-49ff-47cf-8a47-4224011a8d48/SDMX\%20for\%20STS_guidelines_170201.pdf}{SDMX for Short-Term Statistics}.
#'
#' \itemize{
#'  \item{ \code{FREQ}: frequency of the time series. }
#'  \item{ \code{REF_AREA}: Area of the European Statistical System to which the data pertains. }
#'  \item{ \code{SEASONAL_ADJUST}: Status code for seasonal adjustment. }
#'  \item{ \code{INDICATOR}: The economic indicator represented in the data. }
#'  \item{ \code{ACTIVITY}: Economic activity code. }
#'  \item{ \code{BASE_PER}: Base year of the indicator (where the indicator equals 100). }
#'  \item{ \code{TIME_PERIOD}: Period described by the data. }
#'  \item{ \code{OBS_VALUE}: The indicator value. }
#'  \item{ \code{OBS_STATUS}: Status of observation, e.g. estimated, normal, or provisional.}
#'  \item{ \code{CONF_STATUS}: Confidentiality status (e.g. under embargo). }
#'  \item{ \code{PRE_BREAK_VALUE}: Value prior to a 'break' introduced in the time series. }
#'  \item{ \code{EMBARGO_TIME}: Date and time when data can be publicly released. }
#'  \item{ \code{COMMENT_OBS}: Optional comment for observation }
#'  \item{ \code{COMMENT_DSET}: Optional comment for the data set. }
#'  \item{ \code{TRANSFORMATION}: Code indicating if and how the data have been transformed. }
#'  \item{ \code{DECIMALS}: Number of decimals in the data. }
#'  \item{ \code{UNIT_MULT}: Value by which the data must be multiplied as a power of 10.}
#'  \item{ \code{UNIT_MEASURE}: National accounts unit of measure. }
#'  \item{ \code{TIME_FORMAT}: Code indicating the time format. }
#'  \item{ \code{COMMENT_TS}: Comment on the time series.}
#'}
#'
#'
#' @section STS meta:
#'   These are elements of the file name used to transfer the data. The column
#'   names are not part of the formal DSD.
#' \itemize{
#'   \item{\code{DOMAIN}: Statistical domain.}
#'   \item{\code{INDICATOR}: Indicator in the file (TOVT total turnover, non-deflated).}
#'   \item{\code{FREQ}: Time series frequency.}
#'   \item{\code{PERIOD}: Last period described by the time series.}
#'   \item{\code{VERSION}: File version.}
#' }
#' @docType data
#' @format A .RData file
NULL

