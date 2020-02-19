#' Detects outliers in a given data vector
#'
#' Takes in a data vector and detects outliers using multiple outlier-detection techniques:
#' \itemize{
#' \item Z-Score Detection
#' \item Rosner's ESD Test
#' \item Box Plot Detection
#' \item Median Absolute Deviation
#' \item Inter Quantile Range Detection
#' \item n-Sigma Detection
#' }
#' After detecting the outliers using each of these techniques, a data element is detected as outlier only if it detected in more than "select" number of techniques. After detecting the outliers, the data vector is treated for outliers using Min-Max capping.
#' @param data_col Any numeric vector that needs to be treated for outliers
#' @param select A metric to mark a data element as an outlier
#' @return Returns an outlier treated data vector using Min-Max capping
#' @export
#' @usage OutlierDetectionAndTreatment(data_col, select)
#' @example
#' a <- c(12,34,234,23,678, 768, 34, 34 ,78)
#' OutlierDetectionAndTreatment(a,2)
OutlierDetectionAndTreatment  <- function(data_col, select = 2)
{
  outliers_IQR <- IQRDetect(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_IQR)
  # min_IQR <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_IQR <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers_nSigma <- nSigmaDetect(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_nSigma)
  # min_nSigma <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_nSigma <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers_Zscore <- outliersZ(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_Zscore)
  # min_zScore <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_zScore <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers_ESD <- ESD_Detect(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_ESD)
  # min_ESD <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_ESD <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers_boxplot <- Boxplot_Detect(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_boxplot)
  # min_boxplot <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_boxplot <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers_MAD <- MAD_Detect(data_col)
  # data_outlier_detected <- data.frame(data_col, outliers_MAD)
  # min_MAD <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  # max_MAD <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  outliers <- data.frame(outliers_IQR$outliers_vec, outliers_nSigma$outliers_vec, outliers_Zscore$outliers_vec, outliers_ESD$outliers_vec, outliers_boxplot$outliers_vec, outliers_MAD$outliers_vec)
  outliers$SumDetected <- rowSums(outliers)
  outliers_vec <- outliers$SumDetected >= select

  # optimal_min <- min(min_IQR, min_nSigma, min_zScore, min_ESD, min_boxplot, min_MAD)
  # optimal_max <- max(max_IQR, max_nSigma, max_zScore, max_ESD, max_boxplot, max_MAD)

  data_outlier_detected <- data.frame(data_col, outliers_vec)
  optimal_min <- min(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])
  optimal_max <- max(data_outlier_detected[data_outlier_detected[2] == FALSE,][1])

  ## MIN MAX CAPPING
  ifelse(length(data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] > optimal_max),]$data_col) > 0, data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] > optimal_max),]$data_col <- optimal_max, data_outlier_detected)

  ifelse(length(data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] < optimal_min),]$data_col) > 0, data_outlier_detected[(data_outlier_detected[2] == TRUE) & (data_outlier_detected[1] < optimal_min),]$data_col <- optimal_min, data_outlier_detected)

  fit_outliers <- list(optimal_min = optimal_min, optimal_max = optimal_max, select = select, ind_fits = list(IQR_fit = outliers_IQR$fit_outliers, nSigma_fit = outliers_nSigma$fit_outliers, zscore_fit = outliers_Zscore$fit_outliers, ESD_fit = outliers_ESD$fit_outliers, boxplot_fit = outliers_boxplot$fit_outliers, MAD_fit = outliers_MAD$fit_outliers))

  return(list (outlier_treated = data_outlier_detected$data_col, fit_outliers = fit_outliers))
}

#' Treats the outliers present in the given dataset and checks if treating the dataset for outliers is actually necessary
#'
#' Takes in a data set and treats all the outliers using multiple outlier-detection techniques. After treating the entire dataset for any possible outliers, it checks if treating the dataset for outliers is actually necessary by comparing the base model performance for the treated and untreated datasets and returns the best one.
#' @param data Any dataset that needs to be treated for outliers
#' @param dv A string mentioning the column name of the Dv in the given data.
#' @return Returns an outlier treated data vector using Min-Max capping if the performance of the treated dataset is better than the original dataset along with the fit file.
#' @export
#' @usage bestOutlierTreat(data, dv)

bestOutlierTreat <- function(data, dv)
{
  # library(Tranformations)
  data <- data.frame(data)
  dist <- data_distribution(data, dv)
  continuous_cols <- dist[(dist$distribution=="Continous" & dist$is_dv==FALSE),]$names
  dvcol <- data[,dv]
  # library(data.table)
  #qq <- data.table(missing_treated)[,lapply(.SD, OutlierDetectionAndTreatment), .SDcols=continuous_cols]

  outliers_treated <- list()
  outliers_fit <- list()
  # outliers_max <- list()
  for (i in 1:length(continuous_cols))
  {
    qq <- OutlierDetectionAndTreatment(data[, continuous_cols[i]])
    outliers_treated[[continuous_cols[i]]] <- qq$outlier_treated
    outliers_fit[[continuous_cols[i]]] <- qq$fit_outliers
    # outliers_max[[continuous_cols[i]]] <- qq$optimal_max
  }
  outliers_treated <- data.frame(outliers_treated)
  outliers_treated <- cbind(outliers_treated, dvcol)

  original_data <- cbind(data[, continuous_cols], dvcol)

  total_matrix_all <- list("Min-Max Treated" = outliers_treated, "Original" = original_data)
  # library(pbmcapply)
  output<-do.call(rbind,pbmclapply(seq(1:2), model_my_data,
                                   data = total_matrix_all,
                                   mc.cores = 1))
  if (dist[dist$is_dv == T, ]$distribution == "Continous")
  {
    output <- data.frame(output[, c("Rsquared", "MAE", "RMSE")])
    var <- c("Rsquared", "MAE", "RMSE")
  } else
  {
    output <- data.frame(output[, c("Mean_F1", "Mean_Precision", "Mean_Recall")])
    var <- c("Mean_F1", "Mean_Precision", "Mean_Recall")
  }

  # var<-nearZeroVar(output, saveMetrics = TRUE)
  # var<-rownames(var[(var$zeroVar=="FALSE"),])
  # out<-data.frame(output)[,var]
  output$Method <- names(total_matrix_all)
  best_trans_metric <- names(total_matrix_all[which(output[,1] == max(output[,1]))[1]])
  output <- output[,c("Method", var)]
  ifelse(best_trans_metric == "Original", return(list(outliers_treated = original_data, outliers_fit = NULL, continuous_cols = continuous_cols, model_perf_metrics = output)), return(list(outliers_treated = outliers_treated, outliers_fit = outliers_fit, continuous_cols = continuous_cols, model_perf_metrics = output)))
}
