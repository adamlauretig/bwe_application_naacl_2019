# code to produce FRUS-based figures (2 & 3)
rm(list = ls())
options(stringsAsFactors = FALSE)
library(bwe)
library(data.table)
library(ggplot2)
library(quanteda)
library(lmtest)
library(mgcv)
library(cowplot)
load("frus_bwe_1964_1966_2.rdata")
load("frus_subset.rdata")
load("monthly_usa_event_data.rdata")

## documents by month ----
frus_subset[, doc_date := ifelse(
  nchar(doc_date) < 10, paste0(doc_date, "-01"), doc_date)]

frus_subset[, day_numeric := as.numeric(substr(doc_date, 9, 10))]
frus_subset[, bi_week := ifelse(day_numeric < 15, "01", "28")]
frus_subset[, bi_weekly := paste0(
  substr(as.character(doc_date), 1, 7), "-", bi_week)]

doc_bi <- unique(frus_subset$bi_weekly)
event_bi <- unique(monthly_quad_usa_source$bi_weekly)
event_bi <- event_bi[ event_bi >= as.Date("1964-01-01") & event_bi <  as.Date("1967-01-01")]


## creating dfms ----
dat <- frus_subset
text_field <- "text"
context_window <- 9L
minimum_word_count <- 40L
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
frus_dfm <- dfm(
  x = txtfile, 
  tolower = TRUE, 
  remove_punct = TRUE, 
  select = token_count$V1)
frus_dfm <- dfm_sort(frus_dfm, decreasing = TRUE, margin = "features")
frus_dfm@x <- ifelse(frus_dfm@x > 1, 1, frus_dfm@x)

# anchoring embeddings ----
anchored_frus <- double_anchor_word_embeddings(
  anchoring_word = c("war", "peace", "conflict", "cooperation"), 
  bwe_object = frus_bwe, similarity = "cosine", trim = TRUE)
war_peace_scale <- data.table(words = colnames(anchored_frus$identified_words),
  scale =anchored_frus$identified_words[1, ])
war_peace_scale <- war_peace_scale[ !(words %in%  stopwords("english")) ]
ideology_scale <- data.table(words = colnames(anchored_frus$identified_words),
  scale =anchored_frus$identified_words[2, ])
ideology_scale <- ideology_scale[ !(words %in%  stopwords("english")) ]


anchored_docs <- frus_dfm %*% war_peace_scale$scale
frus_subset$war_peace_scale <- as.vector(anchored_docs)
ideology_docs <- frus_dfm %*% ideology_scale$scale
frus_subset$ideology <- as.vector(ideology_docs)

monthly_conflict <- frus_subset[, lapply(.SD, mean), by = .(bi_weekly), 
  .SDcols = c("war_peace_scale", "ideology")]


with(monthly_conflict, plot(as.Date(bi_weekly), war_peace_scale))
with(monthly_conflict, plot(as.Date(bi_weekly), ideology))
with(monthly_conflict, plot(war_peace_scale, ideology))

# combine event data with documents
months <- data.table(wk_actual = seq.Date(from = as.Date("1964-01-01"), to = as.Date("1966-12-31"), by = "day"))
months[, bi_week := as.numeric(substr(wk_actual, 9, 10))]
months[, bi_weekly := as.character(paste0(substr(wk_actual, 1, 8), ifelse(bi_week < 15, "01", "28")))]
months <- months[,.N, by = .(bi_weekly)]
setkey(months, "bi_weekly")
monthly_quad_usa_source[, bi_weekly := as.character(bi_weekly)]
setkey(monthly_quad_usa_source, "bi_weekly")
monthly_event_counts <- merge(months, monthly_quad_usa_source, by = "bi_weekly", all.x = TRUE)
monthly_event_counts[, source_root := ifelse(is.na(source_root), "USA", source_root)]
setkey(monthly_conflict, "bi_weekly")

monthly_event_counts[, `:=`(
  neutral = ifelse(is.na(neutral), 0, neutral),
  verbal_cooperation = ifelse(is.na(verbal_cooperation), 0, verbal_cooperation),
  material_cooperation = ifelse(is.na(material_cooperation), 0, material_cooperation),
  verbal_conflict = ifelse(is.na(verbal_conflict), 0, verbal_conflict),
  material_conflict = ifelse(is.na(material_conflict), 0, material_conflict)
) ]


war_peace_events <- merge(monthly_event_counts, monthly_conflict, 
  by = "bi_weekly", all.x = TRUE)
war_peace_events[, war_peace_lag := shift(x = war_peace_scale, n = 1, "lag")]
war_peace_events[, ideology_lag := shift(x = ideology, n = 1, "lag")]
war_peace_events[, material_conflict_lag := shift(material_conflict, n = 1, "lag") ]
war_peace_events[, material_cooperation_lag := shift(material_cooperation, n = 1, "lag") ]

war_peace_events[, war_peace_diff := war_peace_scale - war_peace_lag ]
war_peace_events[, ideology_diff := ideology - ideology_lag ]

war_peace_events[, time_trend := 1:.N]
war_peace_events[, hostility := material_conflict- material_cooperation]
with(war_peace_events, plot(war_peace_lag, material_conflict))
with(war_peace_events, plot(ideology_lag, material_conflict))

m1 <- lm(material_conflict ~ war_peace_lag, 
  data = (war_peace_events))
m2 <- glm(material_conflict ~ war_peace_lag, data = (war_peace_events), poisson)

m2_preds <- predict(m2, se.fit = TRUE, type = "response")

new_df <- with(war_peace_events, data.table(
  material_conflict = material_conflict[2:72], 
  war_peace_lag = war_peace_lag[2:72], 
  fit = m2_preds$fit, se = m2_preds$se.fit))

new_df[, `:=`(
  lower_95 = fit - 1.96 * se,
  upper_95 = fit + 1.96 * se
)]
bellicosity_plot <- ggplot(data = new_df) +
  geom_line( aes(x = war_peace_lag, y = fit)) + 
  geom_ribbon(data = new_df, aes(
    x = war_peace_lag, ymin = lower_95, ymax = upper_95), alpha = .25) + 
  geom_point(aes(x = war_peace_lag, y = material_conflict)) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 18),
    strip.background =element_rect(fill="white")
    # text = element_text(family = "Palatino")
    ) +
  labs(x = "Bellicosity\n(Negative is More Peaceful)", y = "U.S.-Initiated Material Conflict Events",
    title = "Material Conflict Events Increase\n in Response to Bellicosity in Foreign Policy")
ggsave(filename = "predicted_bellicosity.pdf", 
  plot = bellicosity_plot, dpi = 1000, width = 8, height = 8)  


ggplot(data = pred_data, aes(x = war_peace_lag, y = fit)) + 
  geom_line() + geom_ribbon(data = pred_data, 
    aes(x = war_peace_lag, ymin = lower_95, ymax = upper_95), alpha = .1) + 
  geom_point(data = war_peace_events, aes(x = war_peace_lag, y = material_conflict))
  
p1 <- ggplot(data = war_peace_events, aes(x = as.Date(bi_weekly), material_conflict)) + 
  geom_point() + 
  # geom_smooth(span = 0.67, color = "red", se = FALSE, lty = 2) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 6),
    strip.background =element_rect(fill="white"),
    text = element_text(size = 12),
    plot.title = element_text(size = 16)) + 
  labs(y = "U.S.-Initiated Hostile Events", x = "", title = "Biweekly Material Conflict\nEvent Counts")
p2 <- ggplot(data = war_peace_events, aes(x = as.Date(bi_weekly), war_peace_lag)) + 
  geom_point() + 
  # geom_smooth(span = 0.5, color = "red", se = FALSE, lty = 2) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 6),
    strip.background =element_rect(fill="white"),
    text = element_text(size = 12),
    plot.title = element_text(size = 16)) + 
  labs(y = "Average Bellicosity Score", x = "", title = "Biweekly Bellicosity Score")

cow_p <- plot_grid(p1, p2, ncol = 1)
labeled_plot <- add_sub(cow_p, "Date, Biweekly, 1964-1966", y  = 0, vjust = -3)
p3 <- ggdraw(labeled_plot)
ggsave(filename = "bellicosity_descriptive.pdf", 
  plot = p3, dpi = 1000, height = 7, width = 5)  

# removing outliers ----

no_outliers <- war_peace_events[-c(4, 21, 62, 71), ]
m2_outliers <- glm(material_conflict ~ war_peace_lag, data = no_outliers, poisson)

m2_preds_outliers <- predict(m2_outliers, se.fit = TRUE, type = "response")

new_df_outlier <- with(no_outliers, data.table(
  material_conflict = material_conflict[2:nrow(no_outliers)], 
  war_peace_lag = war_peace_lag[2:nrow(no_outliers)], 
  fit = m2_preds_outliers$fit, se = m2_preds_outliers$se.fit))

new_df_outlier[, `:=`(
  lower_95 = fit - 1.96 * se,
  upper_95 = fit + 1.96 * se
)]
bellicosity_plot_outlier <- ggplot(data = new_df_outlier) +
  geom_line( aes(x = war_peace_lag, y = fit)) + 
  geom_ribbon(data = new_df_outlier, aes(
    x = war_peace_lag, ymin = lower_95, ymax = upper_95), alpha = .25) + 
  geom_point(aes(x = war_peace_lag, y = material_conflict)) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background =element_rect(fill="white")
    # text = element_text(family = "Palatino")
    ) +
  labs(x = "Bellicosity\n(Negative is More Peaceful)", y = "U.S.-Initiated Material Conflict Events",
    title = "Material Conflict Events Increase\n in Response to Bellicosity in Foreign Policy", 
    subtitle = "(Outliers Removed)")
ggsave(filename = "predicted_bellicosity_outliers.pdf", 
  plot = bellicosity_plot_outlier, dpi = 1000, width = 8, height = 8)  

