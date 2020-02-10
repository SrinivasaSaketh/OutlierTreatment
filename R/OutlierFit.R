#' A fit function to detect the outliers using a pre-defined IQR values
#'
#' Detects outliers by taking in a data vector and IQR values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage IQRFit(data_col, fit_outliers)
IQRFit <- function(data_col, fit_outliers)
{
  data_col <- sort(data_col)
  max_val <- quantile(data_col,0.75)+1.5*fit_outliers$IQR_val
  min_val <- quantile(data_col,0.25)-1.5*fit_outliers$IQR_val
  outliers_vec <- (data_col > max_val | data_col < min_val)
  return (outliers_vec)
}

#' A fit function to detect the outliers using a pre-defined mean and standard deviation values
#'
#' Detects outliers by taking in a data vector and fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage nSigmaFit(data_col, fit_outliers)
nSigmaFit <- function(data_col, fit_outliers)
{
  max_val <- fit_outliers$mean+3*fit_outliers$sd
  min_val <- fit_outliers$mean-3*fit_outliers$sd
  outliers_vec <- (data_col > max_val | data_col < min_val)
  return (outliers_vec)
}

#' A fit function to detect the outliers using a pre-defined min and max values derived from ESD test
#'
#' Detects outliers by taking in a data vector and fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage ESD_Fit(data_col, fit_outliers)
ESD_Fit <- function(data_col, fit_outliers)
{
  ifelse(!is.null(fit_outliers$min_ESD), outliers_vec <- (data_col > fit_outliers$max_ESD | data_col < fit_outliers$min_ESD), outliers_vec <- FALSE)
  return (outliers_vec)
}

#' A fit function to detect the outliers using a pre-defined mean and standard deviation values derived from Z-Score test
#'
#' Detects outliers by taking in a data vector and fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage outliersZ_Fit(data_col, fit_outliers)
outliersZ_Fit <- function(data_col, fit_outliers)
{
  absZ <- abs(data_col - fit_outliers$mean_Z) / fit_outliers$sd_Z  ## return mean and sd for fit
  return (absZ > fit_outliers$zCutOff)
}

#' A fit function to detect the outliers using a pre-defined box-plot statistics
#'
#' Detects outliers by taking in a data vector and box-plot fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage Boxplot_Fit(data_col, fit_outliers)
Boxplot_Fit <- function(data_col, fit_outliers)
{
  max_val <- fit_outliers[1]
  min_val <- fit_outliers[5]
  outliers_vec <- (data_col > max_val | data_col < min_val)
  return (outliers_vec)
}

#' A fit function to detect the outliers using a pre-defined Median Absolute Deviation statistics
#'
#' Detects outliers by taking in a data vector and MAD fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns a boolean vector representing TRUE if the data point is an outlier and a FALSE otherwise
#' @export
#' @usage MAD_Fit(data_col, fit_outliers)
MAD_Fit <- function(data_col, fit_outliers)
{
  if (fit_outliers$mad_MAD == 0)
  {
    cutoff <- fit_outliers$median_MAD  ## return cutoff for fit
    outliers_vec <- ifelse(data_col!=fit_outliers$cutoff_MAD, TRUE, FALSE)
  } else
  {
    data <- abs(data_col - fit_outliers$median_MAD) / fit_outliers$cutoff_MAD ## Return median and mad for fit
    cutoff <- fit_outliers$median_MAD * 1.4826  ## return cutoff for fit
    outliers_vec <- ifelse(data>cutoff, TRUE, FALSE)
  }

  return(outliers_vec)
}

#' A fit function to detect and treat the outliers using the pre-defined statistics for different detection and treatment techniques
#'
#' Detects and treats outliers by taking in a data vector and fit values of the train data
#' @param data_col Any numeric vector in which the outliers needs to be detected
#' @param fit_outliers A list returned from "OutlierDetectionAndTreatment" that is used to fit the test data.
#' @return Returns an outlier treated vector
#' @export
#' @usage OutlierDetectionAndTreatment_Fit(data_col, fit_outliers)
OutlierDetectionAndTreatment_Fit  <- function(data_col, fit_outliers)
{
  outliers_IQR <- IQRFit(data_col, fit_outliers[[1]]$ind_fits$IQR_fit)
  outliers_nSigma <- nSigmaFit(data_col, fit_outliers[[1]]$ind_fits$nSigma_fit)
  outliers_Zscore <- outliersZ_Fit(data_col, fit_outliers[[1]]$ind_fits$zscore_fit)
  outliers_ESD <- ESD_Fit(data_col, fit_outliers[[1]]$ind_fits$ESD_fit)
  outliers_boxplot <- Boxplot_Fit(data_col, fit_outliers[[1]]$ind_fits$boxplot_fit)
  outliers_MAD <- MAD_Fit(data_col, fit_outliers[[1]]$ind_fits$MAD_fit)

  outliers <- data.frame(outliers_IQR, outliers_nSigma, outliers_Zscore, outliers_ESD, outliers_boxplot, outliers_MAD)
  outliers$SumDetected <- rowSums(outliers, na.rm = T)
  outliers_vec <- outliers$SumDetected >= fit_outliers[[1]]$select

  data_outlier_detected <- data.frame(data_col, outliers_vec)
  ## MIN MAX CAPPING
  ifelse(length(data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] > fit_outliers[[1]]$optimal_max),]$data_col) > 0, data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] > fit_outliers[[1]]$optimal_max),]$data_col <- fit_outliers[[1]]$optimal_max, data_outlier_detected)

  ifelse(length(data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] < fit_outliers[[1]]$optimal_min),]$data_col) > 0, data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] < fit_outliers[[1]]$optimal_min),]$data_col <- fit_outliers[[1]]$optimal_min, data_outlier_detected)

  return(data_outlier_detected$data_col)
}

#' A fit function to detect and treat the outliers in a dataset using the pre-defined statistics for different detection and treatment techniques
#'
#' Detects and treats outliers by taking in a data set and fit values of the train data. Returns the original data if "fit_outliers" is NULL
#' @param data Any dataset in which the outliers needs to be detected and treated for all continuous fields
#' @param fit_outliers A list returned from "bestOutlierTreat" that is used to fit the test data.
#' @param continuous_cols A list of continuous columns identified in the train data. Returned from "bestOutlierTreat"
#' @return Returns the original dataset if fit_outliers is NULL and outliers fit dataset otherwise
#' @export
#' @usage Outliers_Fit(data, fit_outliers, continuous_cols)
Outliers_Fit <- function(data, fit_outliers, continuous_cols)
{
  data <- data.frame(data)
  if(is.null(fit_outliers))
    return (data[, continuous_cols])
  # library(data.table)
  outliers_treated_fit <- list()
  for (i in 1:length(continuous_cols))
  {
    # print(i)
    outliers_treated_fit[[continuous_cols[i]]] <- OutlierDetectionAndTreatment_Fit(data[, continuous_cols[i]], fit_outliers[i])
  }
  outliers_treated_fit <- data.frame(outliers_treated_fit)
  return(outliers_treated_fit)
}
