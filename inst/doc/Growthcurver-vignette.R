## ---- echo = FALSE, eval = TRUE------------------------------------------
# First, load the package and the dataset. 
library(growthcurver)

# Load the sample growth curve data provided with the package 
# The first column is the time in hours, and there is one column 
# for each well in a 96-well plate.
d <- growthdata
knitr::kable(d[1:10, 1:8])

## ---- eval = FALSE-------------------------------------------------------
#  # Replace the next line with the location and name of your input data file.
#  file_name <- "the/path/to/my/data/myfilename.txt"
#  d <- read.table(file_name, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  # Here, I assume that your time data are stored in a column called "hours" and
#  # you don't have a column named "time". You can copy the data from the "hours"
#  # column to the "time" column as follows.
#  d$time <- d$hours

## ---- eval = FALSE-------------------------------------------------------
#  # Convert the "time" column from hours to minutes
#  d$time <- d$time * 60
#  
#  # Convert the "time" column from minutes to seconds
#  d$time <- d$time * 60
#  
#  # Convert the "time" column from seconds to hours
#  d$time <- d$time * 60 * 60

## ---- eval = FALSE-------------------------------------------------------
#  # How to do a background correction by averaging several "blank" wells
#  # (for the sake of this example, let's assume that wells B2, D8, and G11
#  # are blanks even though they actually hold growth curve data).
#  # If you want to use this method for background correction,
#  # replace my well names with yours.
#  
#  # First, get the "blank" values over time by averaging the "blank" columns at
#  # each timepoint at which measurements were made.
#  d$blank <- apply(d[, c("B2", "D8", "G11")], 1, mean)
#  # Next, subtract the blank value from the measurements for a column.
#  d$A1 <- d$A1 - d$blank

## ---- eval = TRUE--------------------------------------------------------
# First, load the package and the dataset. 
library(growthcurver)

# Load the sample growth curve data provided in the Growthcurver package.
# The first column is the time in hours, and there is one column 
# for each well in a 96-well plate.
d <- growthdata


## ---- eval = TRUE, fig.height = 3, fig.width = 3-------------------------
# First, we'll do a background correction by subtracting the minimum
# value from the well during the growth curve from all the timepoints.
# You could replace the following two lines with your favorite background 
# correction method (one is given in the "Format of the input data" section).
min_value <- min(d$A1)
d$A1 <- d$A1 - min_value

# Now, we'll use Growthcurver to summarize the growth curve data using the 
# simple background correction method (minimum value correction). 
# This returns an object of type "gcfit" that holds information about
# the best parameters, the model fit, and additional metrics summarizing
# the growth curve.
gc_fit <- SummarizeGrowth(d$time, d$A1)

# It is easy to get the most useful metrics from a gcfit object
gc_fit

# And it is easy to plot the raw data and the best fit logistic curve
plot(gc_fit)

## ---- eval = FALSE-------------------------------------------------------
#  # The gcfit object returned from SummarizeGrowth also contains further metrics
#  # summarizing the growth curve data.
#  gc_fit$vals
#  
#  # look at the structure of the gc_fit object
#  str(gc_fit)

## ---- eval = TRUE--------------------------------------------------------
# To see all the available metrics 
str(gc_fit$vals)

# To access a single metric (for example the residual sum of squares
#                            from the fit of the model to the data)
gc_fit$vals$sigma


## ---- message = FALSE, fig.width = 7-------------------------------------
# As in the simple example, load the package and the data. 
library(growthcurver)
d <- growthdata

# Let's create an output data frame to store the results in. 
# We'll create it so that it is the right size (it's faster this way!), 
# but leave it empty.
num_analyses <- length(names(d)) - 1
d_gc <- data.frame(sample = character(num_analyses),
                   k = numeric(num_analyses),
                   n0  = numeric(num_analyses),
                   r = numeric(num_analyses),
                   t_mid = numeric(num_analyses),
                   t_gen = numeric(num_analyses),
                   auc_l = numeric(num_analyses),
                   auc_e = numeric(num_analyses),
                   sigma = numeric(num_analyses),
                   stringsAsFactors = FALSE)

# Truncate or trim the input data to observations occuring in the first 20 hours.
# Remember that the times in these sample data are reported in hours. To use  
# minutes (or to trim at a different time), change the next line of code. 
# For example, if you still would like to trim at 20 hours, but your time data 
# are reported in minutes use: trim_at_time <- 20 * 60
trim_at_time <- 20   

# Now, loop through all of the columns in the data frame. For each column,
# run Growthcurver, save the most useful metrics in the output data frame,
# and make a plot of all the growth curve data and their best fits.

# First, create a plot for each of the wells in the 96-well plate.
# Uncomment the next line to save the plots from your 96-well plate to a 
# pdf file in the working directory.
# pdf("growthcurver.pdf", height = 8.5, width = 11)
par(mfcol = c(8,12))
par(mar = c(0.25,0.25,0.25,0.25))
y_lim_max <- max(d[,setdiff(names(d), "time")]) - min(d[,setdiff(names(d), "time")])

n <- 1    # keeps track of the current row in the output data frame
for (col_name in names(d)) {
  
  # Don't process the column called "time". It contains time and not OD600 data.
  if (col_name != "time") {

    # Create a temporary data frame that contains just the time and current col
    d_loop <- d[, c("time", col_name)]
    
    # Do the background correction.
    # You could replace the following two lines with your favorite background 
    # correction method (one is given in the "Format of the input data" section).
    min_value <- min(d_loop[, col_name])
    d_loop[, col_name] <- d_loop[, col_name] - min_value

    # Now, call Growthcurver to calculate the metrics using SummarizeGrowth
    gc_fit <- SummarizeGrowth(data_t = d_loop[, "time"], 
                              data_n = d_loop[, col_name],
                              t_trim = trim_at_time)
    
    # Now, add the metrics from this column to the next row (n) in the 
    # output data frame, and increment the row counter (n)
    d_gc$sample[n] <- col_name
    d_gc[n, 2:9] <- c(gc_fit$vals$k,
                      gc_fit$vals$n0,
                      gc_fit$vals$r,
                      gc_fit$vals$t_mid,
                      gc_fit$vals$t_gen,
                      gc_fit$vals$auc_l,
                      gc_fit$vals$auc_e,
                      gc_fit$vals$sigma)
    n <- n + 1
    
    # Finally, plot the raw data and the fitted curve
    # Here, I'll just print some of the data points to keep the file size smaller
    n_obs <- length(gc_fit$data$t)
    idx_to_plot <- 1:20 / 20 * n_obs
    plot(gc_fit$data$t[idx_to_plot], gc_fit$data$N[idx_to_plot], 
         pch = 20, 
         xlim = c(0, trim_at_time), 
         ylim = c(0, y_lim_max),
         cex = 0.6, xaxt = "n", yaxt = "n")
     text(x = trim_at_time / 4, y = y_lim_max, labels = col_name, pos = 1)
     lines(gc_fit$data$t, predict(gc_fit$model), col = "red")
  }
}
# Uncomment the next line to save the plots from your 96-well plate to a file
# dev.off()

## ---- eval = FALSE-------------------------------------------------------
#  # Look at the first few rows (samples) of data in the output data frame.
#  # (I'm only showing the first 4 rows of results, but you may want to see more.
#  #  You can either look at everything using the command "d_gc", or adjust the
#  #  number of rows displayed by changing the 4 to something else,
#  #  e.g., "d_gc[1:15,]").
#  d_gc[1:4, ]

## ---- eval = TRUE, message = FALSE, echo = FALSE-------------------------
library(dplyr)
d_gc[1:4, ] %>% 
    mutate(k = round(k, digits = 5),
         n0 = round(n0, digits = 5), 
         r = round(r, digits = 5),
         t_mid = round(t_mid, digits = 5),
         t_gen = round(t_gen, digits = 5),
         auc_l = round(auc_l, digits = 5),
         auc_e = round(auc_e, digits = 5), 
         sigma = round(sigma, digits = 5))
  

## ---- eval = TRUE, message = FALSE---------------------------------------
# Load dplyr and the sample data
library(dplyr)
d_gc <- as_data_frame(d_gc)

# Plot a histogram of the sigma values in order to check for outliers
hist(d_gc$sigma, main = "Histogram of sigma values", xlab = "sigma")


## ---- eval = FALSE, message = FALSE--------------------------------------
#  # Show the top 5 samples with the largest sigma value
#  # (with the worst model fit to the growth curve data)
#  d_gc %>% top_n(5, sigma) %>% arrange(desc(sigma))

## ---- eval = TRUE, echo = FALSE, message = FALSE-------------------------
d_gc %>%  
  mutate(k = round(k, digits = 5),
         n0 = round(n0, digits = 5), 
         r = round(r, digits = 5),
         t_mid = round(t_mid, digits = 5),
         t_gen = round(t_gen, digits = 5),
         auc_l = round(auc_l, digits = 5),
         auc_e = round(auc_e, digits = 5), 
         sigma = round(sigma, digits = 5)) %>%
  top_n(5, sigma) %>% arrange(desc(sigma))

