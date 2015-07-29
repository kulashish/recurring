#source('../global/redshift_conn.R')
source('app_util_new.R')
library(rpart)
#------------------------------------------------
#Uncomment the below line and point to local JRE
#------------------------------------------------
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library(RWeka)
library(dplyr)
library(plyr)

cats      <<- read.csv('categories.txt', header=T)
cats.disc <<- cats[cats$discretionary==1, 'category']
patterns  <<- read.csv('subscription.txt', header=F)
#trans <- YodleeInsightsConnector.query(conn, query.rand)
#trans <- YodleeInsightsConnector.query(conn, query.fix)
trans     <- read.csv('Top-100-Users-Trans-v4.csv', head=TRUE, sep='|', colClasses = c(UNIQUE.MEM.ID='character', UNIQUE.TRANSACTION.ID='character'), stringsAsFactors=F)
data.sim_df <- trans %>% group_by(UNIQUE.MEM.ID) %>% do(find_similar(.))
#data.sim_df <- ddply(trans, .(UNIQUE.MEM.ID), find_similar)
process_write(data.sim_df[, -1], 'trans_features_full.csv')

#--------------------------------------------------------------------------
data  <- read.csv('trans_features_full-train-1-of-3.csv', header=T
#                  ,col.names=c('mem', 'transaction_id', 'similar_transactions', 'desc',
#                              'date_diff', 'date_var', 'amt', 'cat', 'discret', 'sub', 'label')
                  #colClasses=c(rep('character',4), rep('numeric',3), 'character', rep('logical',3))
                  )
data$label <- as.factor(data$label)
data[is.na(data)]   <- -1
#model.dt  <- rpart(label ~ date_diff+date_var+amt+cat+discret+sub, data=data, method='class', control = rpart.control(cp = 0, xval=2))
m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)
print(m)
#plot(model.dt)
#text(model.dt)

data.test  <- read.csv('trans_features_full-test-1-of-3.csv', header=T
 #                 ,col.names=c('mem', 'transaction_id', 'similar_transactions', 'desc',
#                              'date_diff', 'date_var', 'amt', 'cat', 'discret', 'sub', 'label')
                  #colClasses=c(rep('character',4), rep('numeric',3), 'character', rep('logical',3))
)
data.test$label <- as.factor(data.test$label)
data.test[is.na(data.test)] <- -1
pred <- trans_predict(m, data.test)

#--------Computation of Accuracy measures----------------------
TP <- pred[(pred$label=='1') & (pred$pred=='1'), ]
TN <- pred[(pred$label=='0') & (pred$pred=='0'), ]
FP <- pred[(pred$label=='0') & (pred$pred=='1'), ]
FN <- pred[(pred$label=='1') & (pred$pred=='0'), ]
Recall <- nrow(TP) / (nrow(TP)+nrow(FN))
Precision <- nrow(TP) / (nrow(TP)+nrow(FP))
F1 <- 2 * Recall * Precision / (Recall + Precision)

#--------------------------------------------------------------------------
data.sample_index <- sample(nrow(data), as.integer(.6*nrow(data)))
m <- PART(label ~ date_diff+date_var+amt+cat+discret+sub, data=data)

