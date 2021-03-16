# Title     : Getting and Cleaning Data Course Project
# Objective : Getting and Cleaning Data Course Project
# Created on: 3/13/21

library(dplyr)

# name columns of the given dataframe with the prefix and column indexes as suffix
rename_dfcols <- function(df, prefix) {
  for (i in 1:ncol(df)) {
    names(df)[i] <- sprintf('%s_%d', prefix, i)
  }
  df
}

# This function collect all datasets in either 'test' or 'train' folder.
# All datasets will be bound based on their columns in the resulting data frame
getDataSubset <- function(dataset) {

  # prepare paths to collect data into columns
  subpath <- 'UCI HAR Dataset/%s/%s/%%s_%s.txt'
  subpath1 <- sprintf(subpath, dataset, '.', dataset)
  subpath2 <- sprintf(subpath, dataset, 'Inertial Signals', dataset)

  # collect data from various files
  subject <- read.delim(sprintf(subpath1, 'subject'), header = FALSE)
  names(subject) <- 'subject'

  y <- read.delim(sprintf(subpath1, 'y'), header = FALSE)
  names(y) <- 'activity'

  x <- read.table(sprintf(subpath1, 'X'), header = FALSE)
  x <- rename_dfcols(x, 'x')

  body_acc_x <- read.table(sprintf(subpath2, 'body_acc_x'), header = FALSE)
  body_acc_x <- rename_dfcols(body_acc_x, 'body_acc_x')

  body_acc_y <- read.table(sprintf(subpath2, 'body_acc_y'), header = FALSE)
  body_acc_y <- rename_dfcols(body_acc_y, 'body_acc_y')

  body_acc_z <- read.table(sprintf(subpath2, 'body_acc_z'), header = FALSE)
  body_acc_z <- rename_dfcols(body_acc_z, 'body_acc_z')

  body_gyro_x <- read.table(sprintf(subpath2, 'body_gyro_x'), header = FALSE)
  body_gyro_x <- rename_dfcols(body_gyro_x, 'body_gyro_x')

  body_gyro_y <- read.table(sprintf(subpath2, 'body_gyro_y'), header = FALSE)
  body_gyro_y <- rename_dfcols(body_gyro_y, 'body_gyro_y')

  body_gyro_z <- read.table(sprintf(subpath2, 'body_gyro_z'), header = FALSE)
  body_gyro_z <- rename_dfcols(body_gyro_z, 'body_gyro_z')

  total_acc_x <- read.table(sprintf(subpath2, 'total_acc_x'), header = FALSE)
  total_acc_x <- rename_dfcols(total_acc_x, 'total_acc_x')

  total_acc_y <- read.table(sprintf(subpath2, 'total_acc_y'), header = FALSE)
  total_acc_y <- rename_dfcols(total_acc_y, 'total_acc_y')

  total_acc_z <- read.table(sprintf(subpath2, 'total_acc_z'), header = FALSE)
  total_acc_z <- rename_dfcols(total_acc_z, 'total_acc_z')

  # bind columns
  cbind(subject, y, x,
        body_acc_x, body_acc_y, body_acc_z,
        body_gyro_x, body_gyro_y, body_gyro_z,
        total_acc_x, total_acc_y, total_acc_z)

}

# This function callects all datasets in the 'test' and 'train' folders.
# The datasets will be merged based on rows in the resulting data frame.
getDataset <- function() {
  print('  collecting test datasets...')
  test <- getDataSubset('test')

  print('  collecting train datasets...')
  train <- getDataSubset('train')

  rbind(test, train)
}

# compute the average of each variable for each activity and each subject
compute_means_of_variables <- function(df) {

  # load the data frame into a 'data frame tbl' or 'tbl_df'
  df3 <- tibble::as_tibble(df)

  df3 <- df3 %>% group_by(subject, activity)

  # In the following for loop, we will compute the avarage of each variable
  # and then build a new dataframe for the computed averages
  first <- TRUE
  res <- data.frame()
  for (col_name in names(df3[, 3:ncol(df3)])) {

    # Note 1:
    #   https://dplyr.tidyverse.org/reference/summarise.html
    #   Refer to column names stored as strings with the `.data` pronoun
    # Note 2:
    #   the option ".groups = 'drop'" is to suppress a warning message
    df4 <- summarize(df3, mean(.data[[col_name]]), .groups = 'drop')

    # rename the newly computed mean column accordingly
    names(df4)[3] <- sprintf('avg_%s', col_name)

    if (first) {
      first <- FALSE
      res <- df4
    } else {
      res <- cbind(res, df4[,3])
    }
  }

  res
}

# This function converts the integers in the 'activity' column to factor.
factor_activity <- function(df) {
  for (i in 1:nrow(df)) {
    cell <- df$activity[i]

    # Coding Note:
    #   The line above should work well.  However, the following lines
    #   will allow the change of the column name when applicable.
    #
    #   col_name <- 'activity' OR function(df, col_name)
    #   cell <- df[[col_name]][i]  # when passed as func arg, must be 'activity' with quotes
    #   cell <- df(substitute(col_name))[i]  # as func arg, could be just activity without quotes

    if (cell == 1) {
      df$activity[i] <- 'WALKING'
    } else if (cell == 2) {
      df$activity[i] <- 'WALKING_UPSTAIRS'
    } else if (cell == 3) {
      df$activity[i] <- 'WALKING_DOWNSTAIRS'
    } else if (cell == 4) {
      df$activity[i] <- 'SITTING'
    } else if (cell == 5) {
      df$activity[i] <- 'STANDING'
    } else if (cell == 6) {
      df$activity[i] <- 'LAYING'
    }
  }

  # let's convert the strings to factor
  df$activity <- as.factor(df$activity)

  df
}

# This function collects the give datasets and then computes the means
# of the variables in the 3rd columns onward.
main <- function() {
  # Note: use 'cat' instead of 'print' to get the new line '\n' into effect.
  cat('\nData Science: Getting and Cleaning Data by Johns Hopkins University via Coursera\n');

  # collect all datasets
  cat('\ncollecting datasets...\n')
  df <- getDataset()


  # compute the means of the 3rd variables onward because the 1st column
  # is the subject and the 2nd column is the activity being measured in
  # the 3rd column onward.
  cat('\ncomputing means of variables...\n')
  res <- sapply(df[,3:ncol(df)], mean)
  # print(res)
  write.csv(res, 'uci_har_mean.csv')
  cat('Please see file uci_har_mean.csv due to limited print options\n')
  rm('res') # remove the former data frame in 'res' to save memory


  # compute the standard deviations of the 3rd variables onward because
  # the 1st column is the subject and the 2nd column is the activity being
  # measured in the 3rd column onward.
  cat('\ncomputing standard deviations of variables...\n')
  res <- sapply(df[,3:ncol(df)], sd)
  # print(res)
  write.csv(res, 'uci_har_sd.csv')
  cat('Please see file uci_har_sd.csv due to limited print options\n')
  rm('res') # remove the former data frame in 'res' to save memory


  cat('\ncomputing means of variables in groups...\n')
  df2 <- compute_means_of_variables(df)
  rm('df') # remove the former data frame in 'df' to save memory

  # convert the numeric values in the activity column to factor
  df2 <- factor_activity(df2)

  # print the computed averages of each variables
  # options('max.print' = nrow(df2))
  # print(df2)
  write.table(df2, 'uci_har_grp_avg.table', sep='\t', row.names = FALSE)
  cat('Please see file uci_har_grp_avg.table due to limited print options\n')
}

# main function to bootstrap the R script
main()
