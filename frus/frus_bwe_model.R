# code for the FRUS analysis
library(bwe)
load("frus_subset.rdata")


frus_bwe <- fit_bwe(dat = frus_subset, text_field = "text", K = 50, maxit = 2500, 
  context_window = 9L, minimum_word_count = 40L, start_type = "svd", 
  cx_0 = 10, dx_0 = 10, cb_0 = 10, db_0 = 10, seed = 216L, prop_ns = 5, 
  checkfreq = 5, thresh = 1e-6)
save(frus_bwe, file = "frus_bwe_1964_1966_2.rdata")
