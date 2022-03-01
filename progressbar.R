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
progress_bar <- txtProgressBar(min=1,max=100, style=3)
for(i in 1:1000){
  Sys.sleep(1/100)
  setTxtProgressBar(progress_bar, i/10)
}

# END OF FILE ####