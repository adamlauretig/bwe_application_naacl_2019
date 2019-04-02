# runs the model on the inaugural corpus
library(bwe)

d <- quanteda::data_corpus_inaugural
tmp_data <- data.table::data.table(txts = d$documents$texts, yr = d$documents$Year, 
  president = paste(d$documents$FirstName, d$documents$President))
tmp_data <- as.data.frame(tmp_data)
tmp_data$yr_standardized <- (tmp_data$yr - mean(tmp_data$yr))/sd(tmp_data$yr)

inaug_1_big <- fit_bwe(dat = tmp_data, text_field = "txts", K = 50, maxit = 15000, 
  context_window = 9L, minimum_word_count = 5L, start_type = "svd", thresh = 1e-6,
  cx_0 = 5, dx_0 = 5, cb_0 = 5, db_0 = 5, seed = 216L, prop_ns = 5)
save(inaug_1_big, file = "inaug_1_big.rdata")

