file <- 'D:/Perforce/razor/analytics/main/algorithms/AlgoRecurrentTransactions/training/Top-100-Users-Trans-v4.csv'
data <- read.csv(file, sep='|', quote="")
cats.disc <- unique(data[, c('TRANSACTION.CATEGORY', 'DISCRETIONARY')])
write.csv(cats.disc, 'categories.txt', row.names = F, quote = F)

sub <- unique(data[, 'SUBSCRIPTION.PATTERN'])
write.csv(sub, 'subscription.txt', row.names = F, quote = F)
