# Title     : Getting and Cleaning Data Course Project
# Objective : Getting and Cleaning Data Course Project
# Created on: 3/13/21


library(dplyr)


# This function downloads the zipped data file from a particular source
downloadDataFile <- function() {
  filename <- 'getdata_projectfiles_UCI HAR Dataset.zip'
  if (!file.exists(filename)) {
    cat('\nDownloading data file...\n')
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = filename, method = 'curl')

    cat('Unzipping data file...\n')
    unzip(filename)
  }
}


# This function collect all datasets in either 'test' or 'train' folder.
# All datasets will be bound based on their columns in the resulting data frame
getDataSubset <- function(dataset) {

  # prepare paths to collect data into columns
  subpath <- sprintf('UCI HAR Dataset/%s/%%s_%s.txt', dataset, dataset)

  # collect data from various files
  subject <- read.delim(sprintf(subpath, 'subject'), header = FALSE)
  y <- read.delim(sprintf(subpath, 'y'), header = FALSE)
  x <- read.table(sprintf(subpath, 'X'), header = FALSE)

  # bind columns
  cbind(subject, y, x)

}


# This function callects all datasets in the 'test' and 'train' folders.
# The datasets will be merged based on rows in the resulting data frame.
getDataset <- function() {
  print('  collecting test datasets...')
  test <- getDataSubset('test')

  print('  collecting train datasets...')
  train <- getDataSubset('train')

  total <- rbind(test, train)

  features <- read.table('UCI HAR Dataset/features.txt')
  colnames(total) <- c('Subject', 'Activity', features[,2])

  total
}


# This function collects the give datasets and then computes the means
# of the variables in the 3rd columns onward.
main <- function() {
  # Note: use 'cat' instead of 'print' to get the new line '\n' into effect.
  cat('\nData Science: Getting and Cleaning Data by Johns Hopkins University via Coursera\n');

  # download the zipped data file if not existed
  downloadDataFile()

  # collect all datasets
  cat('\ncollecting datasets...\n')
  data <- getDataset()

  # Extract only variables that represent means and standard deviations of measurements
  # Note 1: Columns 1 and 2 are requirements for the Subject and Activity variables
  # Note 2: We need to select columns before converting to tibble due to duplicated
  #         names in the given features.  IOW, we cannot use the 'select' verb of the
  #         dplyr package because we cannot convert the dataset to tibble data format.
  data <- subset(data, select = c(1, 2, grep('mean|std', colnames(data))))

  # Convert the data set to tibble format, select applicable variables,
  # group the data set based on subject and activity before computing
  # the means of each variables in the groups
  data <- data %>% as_tibble %>%
    group_by(Subject, Activity) %>%
    summarize_all(mean)

  # Read the labels for the activities
  activity_label <- read.table('UCI HAR Dataset/activity_labels.txt')

  # Factorize the activities in the data set
  data$Activity <- data$Activity %>%
    factor(levels = activity_label[,1], labels = activity_label[,2])

  cat('\ncomputing means of variables in groups...\n')
  # print the computed averages of each variables
  # print(data)
  write.table(data, 'uci_har_grp_mean.txt', sep='\t', row.names = FALSE)
  cat('Please see file uci_har_grp_mean.txt due to limited print options\n')
}


# main function to bootstrap the R script
main()
