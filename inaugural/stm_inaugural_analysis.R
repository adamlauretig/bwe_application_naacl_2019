# inaug analysis with stm
rm(list = ls())
options(stringsAsFactors = FALSE)
library(bwe)
library(data.table)
library(quanteda)
library(stm)
library(ggplot2)
library(xtable)
d <- quanteda::data_corpus_inaugural
tmp_data <- data.table::data.table(txts = d$documents$texts, yr = d$documents$Year, 
  president = paste(d$documents$FirstName, d$documents$President))
tmp_data <- as.data.table(tmp_data)
tmp_data$post_1945 <- ifelse(tmp_data$yr > 1945, 1, 0)



temp <- textProcessor(documents = tmp_data$txts, 
  metadata = data.frame(tmp_data$post_1945), 
  lowercase = TRUE, 
  removestopwords = TRUE, 
  removepunctuation = TRUE, 
  stem = FALSE)
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta, lower.thresh = 5)
mod_out <- stm(documents = out$documents, vocab = out$vocab, K = 50, 
  prevalence= ~ (tmp_data.post_1945), data = out$meta, seed = 216L)
save(mod_out, file = "inaug_analysis_stm.rdata")


frex_only_labels <- function (model, topics = NULL, n = 7, frexweight = 0.5) 
{
    if (n < 1) 
        stop("n must be 1 or greater")
    logbeta <- model$beta$logbeta
    K <- model$settings$dim$K
    vocab <- model$vocab
    if (is.null(topics)) 
        topics <- 1:nrow(logbeta[[1]])
    aspect <- length(logbeta) > 1
    out <- list()
    if (!aspect) {
        #out$prob <- list()
        out$frex <- list()
        #out$lift <- list()
        #out$score <- list()
        logbeta <- logbeta[[1]]
        wordcounts <- model$settings$dim$wcounts$x
        frexlabels <- try(calcfrex(logbeta, frexweight, wordcounts), 
            silent = TRUE)
        #liftlabels <- try(calclift(logbeta, wordcounts), silent = TRUE)
        #scorelabels <- try(calcscore(logbeta), silent = TRUE)
        #problabels <- apply(logbeta, 1, order, decreasing = TRUE)
        for (k in 1:K) {
            #out$prob[[k]] <- vocab[problabels[1:n, k]]
            if (class(frexlabels) == "try-error") {
                out$frex[[k]] <- "FREX encountered an error and failed to run"
            }
            else {
                out$frex[[k]] <- vocab[frexlabels[1:n, k]]
            }
            # if (class(liftlabels) == "try-error") {
            #     out$lift[[k]] <- "Lift encountered an error and failed to run"
            # }
            # else {
            #     out$lift[[k]] <- vocab[liftlabels[1:n, k]]
            # }
            # if (class(scorelabels) == "try-error") {
            #     out$lift[[k]] <- "Score encountered an error and failed to run"
            # }
            # else {
            #     out$score[[k]] <- vocab[scorelabels[1:n, k]]
            # }
        }
        out <- lapply(out, do.call, what = rbind)
    }
    out$topicnums <- topics
#    out <- out$frex
    class(out) <- "labelTopics"
    return(out)
}


frex_topics <- frex_only_labels(mod_out, n = 8)
words <- frex_topics$frex
words[38, ]
words[46, ]
effect <- estimateEffect(c(38, 46) ~ tmp_data.post_1945, 
  stmobj = mod_out, metadata = out$meta, documents = out$documents)
plot_data <- plot(effect, covariate = "tmp_data.post_1945", 
  method = "difference", cov.value1 = 1, cov.value2 = 0)
means <- do.call(rbind, plot_data$means)
cis <- do.call(rbind, plot_data$cis)

to_display <- data.table(topics = c("International Topic", "Domestic Topic"), topic_ind = c(1, 2), 
  means = means, ci_lower = cis[, 1], 
  ci_upper = cis[, 2], words = rbind(paste(words[38, ], collapse = ", "), paste(words[46, ], collapse = ", ")))

p <- ggplot(data = to_display, aes(x = topics, y = means)) + 
  geom_point(size = 2) + 
  geom_linerange(aes(x = topics, ymin = ci_lower, ymax = ci_upper), lwd = .75) + 
  geom_hline(aes(yintercept = 0), lty = 2) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_blank(),
    axis.line = element_line(size = .5),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18),
    strip.background =element_rect(fill="white")
    # text = element_text(family = "Palatino")
    ) + labs(x = "", y = "Change in Topics After 1945", 
      title = "No Shift in Domestic and International Topics\nAfter 1945")
  
ggsave(filename = "stm_inaug_plot.pdf", 
  plot = p, dpi = 1000, width = 6, height = 8)

topics <- cbind(words[38, ], words[46, ])
colnames(topics) <- c("International Topic", "Domestic Topic")
print(xtable(topics), include.rownames = FALSE)
