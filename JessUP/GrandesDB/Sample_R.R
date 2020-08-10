path <- 'C:\\Users\\usuario\\Desktop\\DSenCDMX\\JessUP\\GrandesDB'
setwd(path)
set.seed(0)
indexs <- sample(1:10000, 50 , replace = FALSE)

library(readr)
library(microbenchmark)
res <- microbenchmark(
  read.csv = read.csv("sample_size_speedup50000checkouts-by-title.csv", stringsAsFactors = FALSE),
  fread = data.table::fread("sample_size_speedup50000checkouts-by-title.csv", sep = ",", stringsAsFactors = FALSE),
  readcsv = read_csv("sample_size_speedup50000checkouts-by-title.csv",) ,
  
  times = 100)
res
#          Unit: milliseconds
#     expr        min         lq       mean     median         uq        max
# read.csv 17034.2886 17669.8653 19369.1286 18537.7057 20433.4933 23459.4308
#    fread   287.1108   311.6304   432.8106   356.6992   460.6167   888.6531


library(ggplot2)
autoplot(res)
########################################################################
set.seed(0)
m <- 9604*11
indexs <- sample(1:34892624, m, replace = FALSE)
indexs <- sort(indexs)

#init <- data.table::fread('checkouts-by-title.csv', sep=',', stringsAsFactors = FALSE, nrows = 1, encoding = 'UTF-8' )
#init
#t1 <- Sys.time()
#for(i in 1:length(indexs))
#{
 # temp <- data.table::fread('checkouts-by-title.csv', sep=',', 
#                          stringsAsFactors = FALSE, nrows = 1 , skip = indexs[i], encoding = 'UTF-8', 
 #                         col.names = c("UsageClass", "CheckoutType", "MaterialType", "CheckoutYear", "CheckoutMonth", "Checkouts", "Title", 
  #                                      "Creator", "Subjects", "Publisher", "PublicationYear"))
#  init <- rbind(init, temp)
 # print(i)
#}
#t2 <- Sys.time()
#t2-t1


desesperacion <- function( i )
{
  print(i)
  temp <- ?data.table::fread('checkouts-by-title.csv', sep=',', 
                            stringsAsFactors = FALSE, nrows = 1 , skip = (i-1),
                            encoding = 'UTF-8', 
                            col.names  = c("UsageClass", "CheckoutType", "MaterialType", "CheckoutYear",
                                           "CheckoutMonth", "Checkouts", "Title",
                                            "Creator", "Subjects", "Publisher", "PublicationYear"))
  return(temp)
  
}
t1 <- Sys.time()
lista <- lapply(FUN=desesperacion, X=indexs)
t2 <- Sys.time()
t2-t1
a <- do.call(rbind, lista)
write.csv(a, file=paste0('sample_size_speedup_', m, 'checkouts-by-title.csv'), rrow.names = FALSE)



2.1 inutos para 1015 


a <- data.table::fread('checkouts-by-title.csv', sep=',', 
                       stringsAsFactors = FALSE,
                       encoding = 'UTF-8', 
                       col.names  = c("UsageClass", "CheckoutType", "MaterialType", "CheckoutYear",
                                      "CheckoutMonth", "Checkouts", "Title",
                                      "Creator", "Subjects", "Publisher", "PublicationYear"))
a2 <- a[indexs]
write.csv(a2, file=paste0('sample_size_speedup_', m, 'checkouts-by-title.csv'), row.names = FALSE)
