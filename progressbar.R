### Simple progress bar
#     Creates progress bar object, and then updates it 1000 times in loop
#
#     By Patrick Rogers, California Research Bureau
#       Mar 2022
#
#     Uses the following Packages
#       {utils}
#
#     Uses the following data
#       n/a

# Progress Bar ####
progress_bar <- txtProgressBar(min=1,max=1000, style=3)
for(i in 1:1000){
  Sys.sleep(1/1000)
  message(i)
  setTxtProgressBar(progress_bar, i)
}

# Time Elapsed ####

start <- Sys.time()
for(i in 1:10){
  cat("Time Elapsed: ", round(Sys.time() - start))
  Sys.sleep(1)
  cat("\r")
}

# END OF FILE ####
