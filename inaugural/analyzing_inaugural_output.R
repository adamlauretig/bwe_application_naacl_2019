options(stringsAsFactors = FALSE)
library(bwe)
library(data.table)
library(quanteda)
library(Matrix)
library(ggplot2)
library(ggridges)
library(xtable)
load("inaug_1_big.rdata")

## anchoring words ----
inaug_1_anchors <- double_anchor_word_embeddings(
  anchoring_word = c("international", "domestic"), 
  bwe_object = inaug_1_big, 
  similarity = "cosine", 
  trim = TRUE
  )

inaug_2_anchors <- anchor_word_embeddings(
  anchoring_word = c("america"), 
  bwe_object = inaug_1_big, 
  similarity = "cosine", 
  trim = TRUE
  )
## cosine similarities ----
m1_war <- nearest_words(
  vec1 = inaug_1_anchors$identified_words[, 
    grep("^war$", x = colnames(inaug_1_anchors$identified_words))], 
  mat1 = inaug_1_anchors$identified_words, n_similar = 9)

m2_war <- nearest_words(
  vec1 = inaug_2_anchors$identified_words[, 
    grep("^war$", x = colnames(inaug_2_anchors$identified_words))], 
  mat1 = inaug_2_anchors$identified_words, n_similar = 9)

m3_war <- nearest_words(
  vec1 = inaug_1_big$x[, 
    grep("^war$", x = colnames(inaug_1_big$x))], 
  mat1 = inaug_1_big$x, n_similar = 9)

m1_peace <- nearest_words(
  vec1 = inaug_1_anchors$identified_words[, 
    grep("^peace$", x = colnames(inaug_1_anchors$identified_words))], 
  mat1 = inaug_1_anchors$identified_words, n_similar = 9)

m2_peace <- nearest_words(
  vec1 = inaug_2_anchors$identified_words[, 
    grep("^peace$", x = colnames(inaug_2_anchors$identified_words))], 
  mat1 = inaug_2_anchors$identified_words, n_similar = 9)

m3_peace <- nearest_words(
  vec1 = inaug_1_big$x[, 
    grep("^peace$", x = colnames(inaug_1_big$x))], 
  mat1 = inaug_1_big$x, n_similar = 9)

m1_american <- (nearest_words(
  vec1 = inaug_1_anchors$identified_words[, 
    grep("^american$", x = colnames(inaug_1_anchors$identified_words))], 
  mat1 = inaug_1_anchors$identified_words, n_similar = 9))

m2_american <- nearest_words(
  vec1 = inaug_2_anchors$identified_words[, 
    grep("^american$", x = colnames(inaug_2_anchors$identified_words))], 
  mat1 = inaug_2_anchors$identified_words, n_similar = 9)

m3_american <- nearest_words(
  vec1 = inaug_1_big$x[, 
    grep("^american$", x = colnames(inaug_1_big$x))], 
  mat1 = inaug_1_big$x, n_similar = 9)

m1_international <- nearest_words(
  vec1 = inaug_1_anchors$identified_words[, 
    grep("^international$", x = colnames(inaug_1_anchors$identified_words))], 
  mat1 = inaug_1_anchors$identified_words, n_similar = 9)

m2_international <- nearest_words(
  vec1 = inaug_2_anchors$identified_words[, 
    grep("^international$", x = colnames(inaug_2_anchors$identified_words))], 
  mat1 = inaug_2_anchors$identified_words, n_similar = 9)

m3_international <- nearest_words(
  vec1 = inaug_1_big$x[, 
    grep("^international$", x = colnames(inaug_1_big$x))], 
  mat1 = inaug_1_big$x, n_similar = 9)

m1_national <- nearest_words(
  vec1 = inaug_1_anchors$identified_words[, 
    grep("^national$", x = colnames(inaug_1_anchors$identified_words))], 
  mat1 = inaug_1_anchors$identified_words, n_similar = 9)

m2_national <- nearest_words(
  vec1 = inaug_2_anchors$identified_words[, 
    grep("^national$", x = colnames(inaug_2_anchors$identified_words))], 
  mat1 = inaug_2_anchors$identified_words, n_similar = 9)

m3_national <- nearest_words(
  vec1 = inaug_1_big$x[, 
    grep("^national$", x = colnames(inaug_1_big$x))], 
  mat1 = inaug_1_big$x, n_similar = 9)

words_of_interest <- c("war", "peace", "american", "international", 'national')
## code to make table 1 ----
m1_words <- data.frame(rbind(
names(m1_war),
names(m1_peace),
names(m1_american),
names(m1_international),
names(m1_national)))
colnames(m1_words) <- c("Word of Interest", "", "", "", "", "", "", "", "")
print(xtable(m1_words, align = "ll|llllllll"), include.rownames = FALSE, add.to.row = )


m2_words <- data.frame(rbind(
names(m2_war),
names(m2_peace),
names(m2_american),
names(m2_international),
names(m2_national)))
colnames(m2_words) <- c("Word of Interest", "", "", "", "", "", "", "", "")
print(xtable(m2_words, align = "ll|llllllll"), include.rownames = FALSE)


m3_words <- data.frame(rbind(
names(m3_war),
names(m3_peace),
names(m3_american),
names(m3_international),
names(m3_national)))

colnames(m3_words) <-c("Word of Interest", "", "", "", "", "", "", "", "")
print(xtable(m3_words, align = "ll|llllllll"), include.rownames = FALSE)

words_sim <- rbindlist(l = list(m1_words, m2_words, m3_words))
colnames(words_sim) <- c("Word of Interest", "", "", "", "", "", "", "", "")

addtorow <- list()
addtorow$pos <- list(0, 5, 10)
addtorow$command <- c(paste0(paste0(' \\hline & \\multicolumn{2}{l}{', "Anchor: International, Domestic", '}', collapse=''), '\\\\'), 
  paste0(paste0('\\hline & \\multicolumn{2}{l}{', "Anchor: American", '}', collapse=''), '\\\\'), 
  paste0(paste0('\\hline & \\multicolumn{2}{l}{', "Anchor: None", '}', collapse=''),  '\\\\'))


print(xtable(words_sim, align = "ll|llllllll"), include.rownames = FALSE, hline.after = c(0, 5, 10), add.to.row = addtorow)

# code to make figures ----
# making a dfm from the inaugurals, that uses same words as the corpus ----
d <- quanteda::data_corpus_inaugural
tmp_data <- data.table::data.table(txts = d$documents$texts, yr = d$documents$Year, 
  president = paste(d$documents$FirstName, d$documents$President))
tmp_data <- as.data.table(tmp_data)
tmp_data$yr_standardized <- (tmp_data$yr - mean(tmp_data$yr))/sd(tmp_data$yr)

dat <- tmp_data
text_field <- "txts"
context_window <- 9L
minimum_word_count <- 5L
txtfile <- unlist(dat[, ..text_field])
split_strings <-
  stringr::str_split(txtfile, pattern = "[.] |[?] |[!] ")
split_strings <- unlist(split_strings)
split_strings <- split_strings[nchar(split_strings) > 0]
all_tokens <-
  tokens(char_tolower(split_strings), remove_punct = TRUE)
token_dt <- data.table(unlist(all_tokens))
token_count <- token_dt[, .N, by = V1]
token_count <-
  token_count[order(token_count$N, decreasing = TRUE)]
token_count <- token_count[ N >= minimum_word_count ]
token_count <- token_count[ !(V1 %in%  stopwords("english"))]
inaug_dfm <- dfm(
  x = txtfile, 
  tolower = TRUE, 
  remove_punct = TRUE, 
  select = token_count$V1)
inaug_dfm <- dfm_sort(inaug_dfm, decreasing = TRUE, margin = "features")
inaug_dfm@x <- ifelse(inaug_dfm@x > 1, 1, inaug_dfm@x)

dom_int_scale <- data.table(words = colnames(inaug_1_anchors$identified_words), 
  scale = inaug_1_anchors$identified_words[1, ])
dom_int_scale <- dom_int_scale[ !(words %in%  stopwords("english")) ]

dom_int_inaug <- inaug_dfm %*% dom_int_scale$scale
dom_int_inaug <- as.vector(unname(dom_int_inaug))
tmp_data$dom_int <- dom_int_inaug
tmp_data[, post_1945 := ifelse(yr > 1945, 1, 0)]
tmp_data[, pre_1932 := ifelse(yr < 1932, 1, 0)]

tmp_data[, era := dplyr::case_when(
  yr > 1945 ~ "After 1945",
  yr < 1932 ~ "Before 1932",
  TRUE ~ "FDR"
)]


tmp_data[, post_1945_txt := ifelse(post_1945 == 1, "Post-1945", "Pre-1945")]
with(tmp_data, plot(yr, dom_int ))
summary(lm(dom_int ~ post_1945, data = tmp_data))
summary(lm(dom_int ~ post_1945, data = tmp_data))

ks_result <- with(tmp_data, 
  ks.test(dom_int[post_1945 == 0], y = dom_int[post_1945 == 1], alternative = "less"))

ks_result2 <- with(tmp_data, 
  ks.test(dom_int[era == "pre 1932"], y = dom_int[era == "post 1945"], alternative = "less"))


file_path <- "inaug_shift_plot.pdf"
file_path2 <- "inaug_shift_plot2.pdf"

inaug_shift <- ggplot(tmp_data, aes(x = dom_int, y = factor(post_1945_txt))) + 
  geom_density_ridges(color = "white", fill = "red", lwd = 1) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major.x = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 18),
    text = element_text(family = "Palatino", size = 14)) + 
  labs(title = "Rhetoric is Less Internationalist After 1945", 
    x = "International to Domestic Score \n(Positive is more International)", y = "", 
    subtitle = paste0("Shift is significant, D = ", 
      round(ks_result$statistic, 2), ", p = ", round(ks_result$p.value, 3)))

inaug_shift2 <- ggplot(tmp_data[ era %in% c("Before 1932", "After 1945"), ], 
   aes(x = dom_int, y = factor(era))) + 
  geom_density_ridges(color = "white", fill = "red", lwd = 1) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major.x = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(size = 18),
    text = element_text(family = "Palatino", size = 14)) + 
  labs(title = "Rhetoric is Less Internationalist After 1945", 
    x = "International to Domestic Score \n(Positive is more International)", y = "", 
    subtitle = paste0("Shift is significant, D = ", 
      round(ks_result2$statistic, 2), ", p = ", round(ks_result2$p.value, 3)))

ggsave(filename = file_path, plot = inaug_shift, dpi = 1000, width = 8, height = 8)
ggsave(filename = file_path2, plot = inaug_shift2, dpi = 1000, width = 8, height = 8)
