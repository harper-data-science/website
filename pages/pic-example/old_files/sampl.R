
setwd(r'(D:\Dropbox\git-harper-data-science\website\pages\pic-example\old_files)')
names <- list.files()
file.rename(from = names,
            to = paste(names, '.txt', sep=''))

list.files()
names <- list.files()

# sample 70% non-stratified
trainprop <- .7
mytrainN <- round(length(names)*trainprop,0)
myvalN <- length(names)-mytrainN
sample(x = names, size = mytrainN)

# sample 70% stratified
trainprop <- .7

farms <-c('farm1', 'farm2')

my_test_samp <- vector('character')
for(i in 1:length(farms)){
  names_sub <- names[grep(pattern = farms[i], x = names)]
  mytrainN <- round(length(names_sub)*trainprop, 0)
  myvalN <- length(names_sub)-mytrainN
  my_test_samp <- c(my_test_samp, 
                    sample(x = names, size = mytrainN)) # use file.copy() or similar here
}
rm(my_test_samp)







