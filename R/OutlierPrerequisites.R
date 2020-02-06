#' Detects outliers using Inter Quartile Range
#'
#' Takes in a vector and detects outliers using IQR
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage IQRDetect(data_col)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' IQRDetect(a)
IQRDetect <- function(data_col)
{
  data_col <- sort(data_col)
  max_val <- quantile(data_col,0.75)+1.5*IQR(data_col)
  min_val <- quantile(data_col,0.25)-1.5*IQR(data_col)
  outliers_vec <- (data_col > max_val | data_col < min_val)
  fit_outliers <- list(min_IQR = min_val, max_IQR = max_val, IQR_val = IQR(data_col))
  return (list(outliers_vec = outliers_vec, fit_outliers = fit_outliers))
}

#' Detects outliers using mean and standard deviation
#'
#' Takes in a vector and detects outliers using n-Sigma
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage nSigmaDetect(data_col)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' nSigmaDetect(a)
nSigmaDetect <- function(data_col)
{
  mean <- mean(data_col)
  sd <- sd(data_col)
  max_val <- mean+3*sd
  min_val <- mean-3*sd
  outliers_vec <- (data_col > max_val | data_col < min_val)
  fit_outliers <- list(min_IQR = min_val, max_IQR = max_val, mean = mean, sd = sd)
  return (list(outliers_vec = outliers_vec, fit_outliers = fit_outliers))
}

#' Performs Rosner's test for outliers on a vector
#'
#' Perform Rosner's generalized extreme Studentized deviate test for up to k potential outliers in a dataset, assuming the data without any outliers come from a normal (Gaussian) distribution.
#' @param data_col Any numeric vector that needs to be treated for outliers
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise.
#' @export
#' @usage ESD_Detect(data_col)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' ESD_Detect(a)
ESD_Detect <- function(data_col)
{
  # library(EnvStats)
  rt <- EnvStats::rosnerTest(data_col, k = (length(data_col[which(data_col %in% boxplot.stats(data_col)$out)]))+1, warn = F)
  #rt$all.stats[rt$all.stats$Outlier == T,]$Obs.Num
  outlier_vec <- ifelse(data_col %in% rt$all.stats[rt$all.stats$Outlier == T,]$Value, TRUE, FALSE) # return min and max value of outliers detected for fit
  ifelse(length(rt$all.stats[rt$all.stats$Outlier == T,]$Value) > 0, fit_outliers <- list(min_ESD = min(rt$all.stats[rt$all.stats$Outlier == T,]$Value), max_ESD = max(rt$all.stats[rt$all.stats$Outlier == T,]$Value)), fit_outliers <- list(min_ESD = NULL, max_ESD = NULL))
  return (list(outliers_vec = outlier_vec, fit_outliers = fit_outliers))
}

#' Performs Z-Score test for outliers on a vector
#'
#' Perform Z-Score test in a data vector, assuming the data without any outliers come from a normal (Gaussian) distribution.
#' @param data_col Any numeric vector that needs to be treated for outliers
#' @param zCutOff A z-score threshold exceeding which will be detected as an outlier. Defaulted to 1.96
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise.
#' @export
#' @usage outliersZ(data_col, zCutOff)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' outliersZ(a)
outliersZ <- function(data_col, zCutOff = 1.96)
{
  #compute standard deviation (sample version n = n [not n-1])
  #stdev <- sqrt(sum((data_col - mean(data_col, na.rm = T))^2, na.rm = T) / sum(!is.na(data_col)))
  #compute absolute z values for each value
  absZ <- abs(data_col - mean(data_col, na.rm = T)) / sd(data_col)  ## return mean and sd for fit
  fit_outliers <- list(mean_Z = mean(data_col, na.rm = T), sd_Z = sd(data_col), zCutOff = zCutOff)
  return (list(outliers_vec = absZ > zCutOff, fit_outliers = fit_outliers))
}

#' Detects outliers using a box-plot
#'
#' Takes in a data vector and detects the outliers in it using the box-plot statistics
#' @param data_col Any numeric vector that needs to be treated for outliers
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise.
#' @export
#' @usage Boxplot_Detect(data_col)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' Boxplot_Detect(a)
Boxplot_Detect <- function(data_col)
{
  outliers <- data_col[which(data_col %in% boxplot.stats(data_col)$out)]  ## return boxplot.stats(data_col)$stats
  outlier_vec <- ifelse(data_col %in% outliers, TRUE, FALSE)
  return (list(outliers_vec = outlier_vec, fit_outliers = boxplot.stats(data_col)$stats))
}

#' Performs Median Absolute Deviation test for outliers on a vector
#'
#' Perform Median Absolute Deviation test for up to k potential outliers in a dataset, assuming the data without any outliers come from a normal (Gaussian) distribution.
#' @param data_col Any numeric vector that needs to be treated for outliers
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise.
#' @export
#' @usage MAD_Detect(data_col)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' MAD_Detect(a)
MAD_Detect <- function(data_col)
{
  if (mad(data_col, constant=1) == 0)
  {
    cutoff <- median(data_col)  ## return cutoff for fit
    outliers_vec <- ifelse(data_col!=cutoff, TRUE, FALSE)
  } else
  {
    data <- abs(data_col - median(data_col)) / mad(data_col, constant=1) ## Return median and mad for fit
    cutoff <- median(data) * 1.4826  ## return cutoff for fit
    outliers_vec <- ifelse(data>cutoff, TRUE, FALSE)
  }

  return(list(outliers_vec = outliers_vec, fit_outliers = list(median_MAD = median(data_col), mad_MAD = mad(data_col, constant=1), cutoff_MAD = cutoff)))
}
