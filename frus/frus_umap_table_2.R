# code to create table 2
rm(list = ls())
options(stringsAsFactors = FALSE)
set.seed(216L)
library(bwe)
library(data.table)
library(ggplot2)
library(quanteda)
library(cowplot)
library(uwot)
library(xtable)
load("~/Dropbox/Dissertation_data/frus_bwe_1964_1966_2.rdata")
load("~/Dropbox/Dissertation_data/frus_subset.rdata")

# two sets of embeddings: vanilla and anchored ----
anchored_frus <- double_anchor_word_embeddings(
  anchoring_word = c("war", "peace", "conflict", "cooperation"), 
  bwe_object = frus_bwe, similarity = "cosine", trim = TRUE)
anchored_x <- anchored_frus$identified_words
x <- frus_bwe$x

# unanchored
umap_x <- umap(X = t(x), n_neighbors = 20, 
  n_components = 12, metric = "cosine", scale = TRUE, min_dist = .8)
plot(umap_x[, 1], umap_x[, 4])
rownames(umap_x) <- colnames(x)

top_words <- rbindlist(lapply(1:ncol(umap_x), function(i){
  
  tmp <- umap_x
  reordered_rows <- tmp[ order(tmp[, i], decreasing = TRUE), ]
  data.table(umap_dim = i, top_words = rownames(reordered_rows)[1:8])
}))

top_umap_words <- matrix(NA_character_, nrow = 8, ncol = ncol(umap_x))
for(i in 1:ncol(umap_x)){
 top_umap_words[,i] <- top_words[umap_dim == i]$top_words
}


# anchored
set.seed(216L)
anchored_umap_x <- umap(X = t(anchored_x), n_neighbors = 50, 
  n_components = 11, metric = "cosine", scale = TRUE, min_dist = .5)
rownames(anchored_umap_x) <- colnames(x)
plot(anchored_umap_x[,1], anchored_umap_x[,4])
anchored_top_words <- rbindlist(lapply(1:ncol(anchored_umap_x), function(i){
  
  tmp <- anchored_umap_x
  reordered_rows <- tmp[ order(tmp[, i], decreasing = TRUE), ]
  data.table(umap_dim = i, top_words = rownames(reordered_rows)[1:8])
}))


top_anchored_umap_words <- matrix(
  NA_character_, nrow = 8, ncol = ncol(anchored_umap_x))
for(i in 1:ncol(anchored_umap_x)){
 top_anchored_umap_words[,i] <- anchored_top_words[umap_dim == i]$top_words
}
subset_of_anchored_words <- top_anchored_umap_words[, c(1:6,9:10)]
subset_of_anchored_words_1 <- rbind(subset_of_anchored_words[, 1:4], subset_of_anchored_words[, 5:8])

print(xtable(subset_of_anchored_words), include.rownames = FALSE)
