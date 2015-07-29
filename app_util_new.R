
query.rand  <- "select mem_id, description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where
mem_id in (select distinct mem_id from yi_base_views.bank_panel order by random() limit 10) and transaction_date > '2014-09-01' order by 1"

query.fix  <- "select mem_id, transaction_id, description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where
mem_id in (1000164415352201, 1000564012973460, 1000564013171293, 1000564013502219, 1000564014242573, 1000616412894541, 1000564012609530, 1000564013137781, 1000564014123899, 1000564014583825, 1000616410283476) and transaction_date > '2014-09-01' order by 1"

query <- "select description, amount, transaction_category as category, optimized_transaction_date as date from yi_base_views.bank_panel where mem_id=1000616410222094 and transaction_date > '2014-08-01'"

DATE_FORMAT <- "%d/%m/%Y"
xyz <- function(X) {
  print(X[, c('UNIQUE.MEM.ID', 'DESCRIPTION')])
}

find_similar  <- function(X){
  X <- X %>% mutate(flag=0)
  #X$flag  <- rep(0, nrow(X))
  data.sim <- data.frame(matrix(ncol=11, nrow=0))
  names(data.sim) <- c('mem', 'transaction_id', 'similar_transactions', 'desc', 'date_diff', 'date_var', 'amt', 'cat', 'discret', 'sub', 'label')
  for(i in 1:nrow(X)) {
    if(X[i, 'flag'] == 1) next
    cond_match <- sim(X$DESCRIPTION[i], X$DESCRIPTION) <= 2
    X[cond_match, 'flag'] <- 1
    temp <- X[cond_match, ] %>%
      group_by(TRANSACTION.TYPE) %>%
      filter(n() >= 2) %>%
      do(feature_vector(.))
      data.sim <- rbind(data.sim, temp[, -1])        
  }
  return (data.sim)
}

strprocess <- function(x) {
  x <- gsub("[^[:alpha:]]", "", x)
  x <- gsub("X", "", x)
  return (tolower(x))
}

sim <- function(x, y){
  x <- strprocess(x)
  y <- strprocess(y)
  return (adist(x, y))
}

date_diff <- function(x, y){
  return (difftime(strptime(x, format="%d/%m/%Y"), strptime(y, format="%d/%m/%Y"), units="weeks"))
  #  return (difftime(strptime(x, format="%d-%m-%Y"), strptime(y, format="%d-%m-%Y"), units="weeks"))
}

Mode<-function(x){ux<-unique(x); ux[which.max(tabulate(match(x,ux)))]};

load_data <- function(file) {
  x <- read.table(file, numerals="no.loss", sep="|", quote="", comment.char="", header = T, stringsAsFactors=F)
  x$UNIQUE.MEM.ID <- rep(1, nrow(x))
  x$UNIQUE.TRANSACTION.ID <- seq.int(1, nrow(x))
  return(x)
}

filter_similar <- function(x) {
  sim <- do.call("rbind", lapply(split(x, x$UNIQUE.MEM.ID), find_similar))
  sim[is.na(sim)] <- -1
  sim[, 'date_diff'] <- gsub(" weeks", "", sim[, 'date_diff'])
  return(sim)
}

build_model <- function() {
  cats  <<- read.csv('categories.txt', header=T)
  cats.disc <<- cats[cats$DISCRETIONARY==1, 'TRANSACTION.CATEGORY']
  patterns <<- read.csv('subscription.txt', header=F)
  
  data  <- read.csv('app_train1.csv', header=T)
  data[is.na(data)]   <- -1
  m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)
}

feature_vector <- function(x) {
#  x %>% arrange(as.Date(OMPTIMIZED.TRANSACTION.DATE, format=DATE_FORMAT)) %>%
#    mutate(diff.date=date_diff(OMPTIMIZED.TRANSACTION.DATE, lag(OMPTIMIZED.TRANSACTION.DATE))) %>%
#    group_by(UNIQUE.MEM.ID) %>%
#    summarize(similar_transactions=paste(UNIQUE.TRANSACTION.ID, collapse="|"), desc=strprocess(DESCRIPTION[1]), 
 #             date_diff=round(mean(diff.date, na.rm=T), digits = 2), date_var=round(var(diff.date, na.rm=T), digits = 2),
#              amt=round(var(AMOUNT) / mean(AMOUNT), digits = 2), cat=Mode(TRANSACTION.CATEGORY), discret=Mode(TRANSACTION.CATEGORY) %in% cats.disc, 
#              sub=Reduce('|', lapply(patterns$V1, grepl, strprocess(DESCRIPTION[1]))))
  
  order.date      <- order(as.Date(x$OMPTIMIZED.TRANSACTION.DATE, format=DATE_FORMAT))
  x               <- x[order.date, ]
  diff.date       <- date_diff(tail(x$OMPTIMIZED.TRANSACTION.DATE, -1), head(x$OMPTIMIZED.TRANSACTION.DATE, -1))
  diff.date.avg   <- round(mean(diff.date), digits = 2)
  diff.date.var   <- round(var(diff.date), digits = 2)
  amt.var_to_mean <- round(var(x$AMOUNT) / mean(x$AMOUNT), digits = 2)
  trans.cat       <- Mode(x$TRANSACTION.CATEGORY)
  trans.cat.d     <- trans.cat %in% cats.disc
  trans.desc      <- strprocess(x$DESCRIPTION[1])
  trans.sub       <- Reduce('|', lapply(patterns$V1, grepl, trans.desc))
  data.features   <- data.frame(mem=x$UNIQUE.MEM.ID[1], transaction_id=x$UNIQUE.TRANSACTION.ID[1], 
                                similar_transactions=paste(x$UNIQUE.TRANSACTION.ID[-1], collapse="|"), desc=trans.desc, 
                                date_diff=diff.date.avg, date_var=diff.date.var, amt=amt.var_to_mean, cat=trans.cat, 
                                discret=trans.cat.d, sub=trans.sub, label=Mode(x$RECURRING))
  return(data.features)
}

process_write <- function(x, file){
  # data.train <- feature_vector(x)
  write.table(x, file=file, append=T, row.names=F, col.names=F,  sep=",")
}

trans_predict <- function(m, x) {
  x$pred <- predict(m, x)
  x$pred[which(x$discret=='TRUE' | x$cat=='Credit Card Payments')] <- 0
  x$pred[which(x$cat=='Paychecks/Salary' | x$cat=='Other Income' | x$sub=='TRUE')] <- 1
  return (x)
}

build_result <- function(raw, processed) {
  result <- processed[processed$pred=='Valid', c(F, T, T, F, F, F, F, T, F, T, F)]
  result <- merge(result, raw[, c('UNIQUE.TRANSACTION.ID', 'DESCRIPTION', 'AMOUNT')], by='UNIQUE.TRANSACTION.ID')
  #result <- cbind(result, raw[result$UNIQUE.TRANSACTION.ID==raw$UNIQUE.TRANSACTION.ID, c('AMOUNT', 'DESCRIPTION')])
  colnames(result) <- c('Transation ID', 'Similar Transactions', 'TRANSACTION.CATEGORY', 'Subscription', 'DESCRIPTION', 'AMOUNT')
  rownames(result) <- NULL
  return(result)
}