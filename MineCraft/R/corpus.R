setwd("/Users/louisteitelbaum/R.Projects/MinecraftCorpus/MinecraftCorpusDialogues")

  # FUNCTION: input filename of Minecraft corpus dialogue, outputs df with conversation, session, role, participant, and text for each line
readminecraftconvdf <- function(x) {
  convoname <- substr(x, 1, nchar(x) - 4)
  b <- strsplit(convoname, split = "-")[[1]][1]
  b <- as.numeric(gsub(pattern = "B", replacement = "", b))
  a <- strsplit(convoname, split = "-")[[1]][2]
  a <- as.numeric(gsub(pattern = "A", replacement = "", a))
  convo <- readLines(x, skipNul = TRUE)
    # delete the empty lines
  convo <- convo[nzchar(convo)]
    # label sessions
  sessions <- c()
  for(line in convo) {
    if(substr(line, 1, 1) == "B"){
      session = line
    }else{
      sessions <- append(sessions, session)
    }
  }
    # remove session headings
  convo <- convo[substr(convo, 1, 1) != "B"]
    # make role identifiers one character
  convo <- gsub(pattern = "<Builder>", replacement = "<B>", convo)
  convo <- gsub(pattern = "<Architect>", replacement = "<A>", convo)
    # dataframe with variables by line
  convo <- data.frame(file = x,
                      conversation = convoname,
                      session = sessions,
                      role = substr(convo, 2, 2), 
                      participant = b * (substr(convo, 2, 2) == "B") + a * (substr(convo, 2, 2) == "A"),
                      text = substr(convo, 5, nchar(convo))
                      )
    # remove session headings
  convo <- convo[grepl(pattern = "Mission has started", convo$text) == FALSE, ]
}

  # FUNCTION: if one person takes more than one turn in talk, this collapses them into one
  # takes a convdf (output of readconvdf FUN above)
collapseturns <- function(convdf) {
  trashrows <- c()
  for(row in 2:nrow(convdf)){
    if(convdf[row, "participant"] == convdf[row -1, "participant"] & 
       convdf[row, "conversation"] == convdf[row -1, "conversation"]){
      convdf[row, "text"] <- paste(convdf[row - 1, "text"], convdf[row, "text"])
      trashrows <- append(trashrows, row - 1)
    }
  }
  convdf <- convdf[!(1:nrow(convdf) %in% trashrows), ]
  convdf
}

  # make a big dataframe with all the conversations
filenames <- list.files("/Users/louisteitelbaum/R.Projects/MinecraftCorpus/MinecraftCorpusDialogues")
minecraftcorpusdf <- lapply(filenames, readminecraftconvdf)
minecraftcorpusdf <- do.call(rbind, minecraftcorpusdf)

  # text mining
library(quanteda)
library(quanteda.textstats)
library(hunspell)

# make a quanteda corpus, then a document-feature matrix
minecraftcorpus <- quanteda::corpus(collapseturns(minecraftcorpusdf), text_field = "text")
minecraftcorpus_tokens <- quanteda::tokens(minecraftcorpus, remove_punct = TRUE)
minecraftcorpus_dfm <- quanteda::dfm(minecraftcorpus_tokens)

# make a dictionary that suggests corrections to spelling errors
corrections <- data.frame(word = as.character(minecraftcorpus_tokens), goodspelling = hunspell::hunspell_check(as.character(minecraftcorpus_tokens)))
corrections <- data.frame(word = corrections[corrections$goodspelling == FALSE, 1])
corrections$suggestion <- as.vector(hunspell::hunspell_suggest(corrections$word))
corrections$suggestion <- lapply(corrections$suggestion, '[', 1)
names(corrections) <- c("word", "sentiment")
  # clean up some of the more egregious errors
    # got up to 900
corrections <- corrections[corrections$word %in% c("minecraft", "lol", "3d", "3D", "=", 
                                                   "haha", "xyz", "hola", "okie", "dokie", 
                                                   "lmao", "lmaooo", ">", "yay", "Lol", "centerish") == FALSE, ]
corrections[corrections$word == "rdy", 2] <- "ready"
corrections[corrections$word == "corrent", 2] <- "current"
corrections[corrections$word == "thats", 2] <- "that's"
corrections[corrections$word == "purp", 2] <- "purple"
corrections[corrections$word == "doen", 2] <- "done"
corrections[corrections$word == "theres", 2] <- "there's"
corrections[corrections$word == "ywa", 2] <- "yeah"
corrections[corrections$word == "minecrafting", 2] <- "minecraft"
corrections[corrections$word == "secone", 2] <- "second"
corrections[corrections$word == "didnt", 2] <- "didn't"
corrections[corrections$word == "yello", 2] <- "yellow"
corrections[corrections$word == "diagnol", 2] <- "diagonal"
corrections[corrections$word == "Thats", 2] <- "that's"
corrections[corrections$word == "bluck", 2] <- "black"
corrections[corrections$word == "kk", 2] <- "OK"
corrections[corrections$word == "yas", 2] <- "yes"
corrections[corrections$word == "greeeeeat", 2] <- "great"
corrections[corrections$word == "Yay", 2] <- "yay"
corrections[corrections$word == "thirs", 2] <- "this"
corrections[corrections$word == "squre", 2] <- "square"
corrections[corrections$word == "whold", 2] <- "whole"
corrections[corrections$word == "strdcuture", 2] <- "structure"
corrections[corrections$word == "THATS", 2] <- "that's"
corrections[corrections$word == "yee", 2] <- "yeah"
corrections[corrections$word == "wil", 2] <- "will"
corrections[corrections$word == "ontop", 2] <- "on top"
corrections[corrections$word == "kool", 2] <- "cool"
corrections[corrections$word == "aight", 2] <- "alright"
corrections[corrections$word == "dont", 2] <- "don't"
corrections[corrections$word == "rred", 2] <- "red"
corrections[corrections$word == "arent", 2] <- "aren't"
corrections[corrections$word == "isnt", 2] <- "isn't"
corrections[corrections$word == "colum", 2] <- "column"
corrections[corrections$word == "orang", 2] <- "orange"
corrections[corrections$word == "roate", 2] <- "rotate"
corrections[corrections$word == "im", 2] <- "I'm"
corrections[corrections$word == "imean", 2] <- "I mean"
corrections[corrections$word == "alittle", 2] <- "a little"
corrections[corrections$word == "enterence", 2] <- "entrance"
corrections[corrections$word == "Alrighty", 2] <- "alrighty"
corrections[corrections$word == "lols", 2] <- "lol"
corrections[corrections$word == "arent", 2] <- "aren't"
corrections[corrections$word == "theres", 2] <- "there's"
corrections[corrections$word == "blok", 2] <- "block"
corrections[corrections$word == "Sprry", 2] <- "sorry"
corrections[corrections$word == "litttttttttt", 2] <- "lit"
corrections[corrections$word == "wasnt", 2] <- "wasn't"
corrections[corrections$word == "littttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
", 2] <- "lit"
corrections[corrections$word == "spacs", 2] <- "spaces"
corrections[corrections$word == "sweeeeeeet", 2] <- "sweet"

  #
correctionsdict <- quanteda::as.dictionary(corrections)

# update spelling on dfm
minecraftcorpus_dfm <- quanteda::dfm_lookup(minecraftcorpus_dfm, correctionsdict, exclusive = FALSE, capkeys = FALSE)

# debugging the cosine similarity function
my_textstat_proxy <- function (x, y = NULL, margin = c("documents", "features"), 
                               method = c("cosine", "correlation", "jaccard", "ejaccard", "dice", "edice", 
                                          "hamman", "simple matching", "euclidean", "chisquared", "hamming", 
                                          "kullback", "manhattan", "maximum", "canberra", "minkowski"), 
                               p = 2, min_proxy = NULL, rank = 100, use_na = FALSE) 
{
  x <- as.dfm(x)
  if (is.null(y)) {
    y <- x
  }
  else {
    if (!is.dfm(y)) 
      stop("y must be a dfm")
    y <- as.dfm(y)
  }
  margin <- match.arg(margin)
  method <- match.arg(method)
  if (margin == "documents") {
    f <- union(featnames(x), featnames(y))
    x <- t(pad_dfm(x, f))
    y <- t(pad_dfm(y, f))
  }
  else {
    if (!identical(docnames(x), docnames(y))) 
      stop("x and y must contain the same documents")
  }
  if (method %in% c("cosine", "correlation", "jaccard", "ejaccard", 
                    "dice", "edice", "hamman", "simple matching", "faith")) {
    if (identical(x, y)) {
      result <- simil(x, NULL, 2, method, min_simil = min_proxy, 
                      rank = rank)
    }
    else {
      result <- simil(x, y, 2, method, min_simil = min_proxy, 
                      rank = rank)
    }
  }
  else {
    if (identical(x, y)) {
      result <- dist(x, NULL, 2, method, p = p)
    }
    else {
      result <- dist(x, y, 2, method, p = p)
    }
  }
  dimnames(result) <- list(colnames(x), colnames(y))
  if (use_na) {
    if (method == "correlation") {
      na1 <- colSds(x) == 0
      na2 <- colSds(y) == 0
    }
    else {
      na1 <- colZeros(x) == nrow(x)
      na2 <- colZeros(y) == nrow(y)
    }
    if (any(na1) || any(na2)) 
      result <- result + make_na_matrix(dim(result), which(na1), 
                                        which(na2))
  }
  return(result)
}
environment(my_textstat_proxy) <- asNamespace('quanteda.textstats')
assignInNamespace("textstat_proxy", my_textstat_proxy, ns = "quanteda.textstats")

# TF-IDF vectorization
minecraftcorpus_tfidf <- quanteda::dfm_tfidf(minecraftcorpus_dfm)
minecraftcorpus_tfidf_mat <- as.matrix(minecraftcorpus_tfidf)
minecraftcorpus_tfidf_mat[minecraftcorpus_tfidf_mat == 0] <- NA

# cosine similarity
tfidfsim <- function(dfmsource, x) {
  tfidfsim <- quanteda::dfm_subset(dfmsource, conversation == x)
  tfidfsim <- quanteda.textstats::textstat_simil(tfidfsim, margin = "documents", method = "cosine")
  tfidfsim <- as.matrix(tfidfsim)
}
utterance.sim.simmax <- function(dfmsource, x) {
  # subset conversation from tfidf document-feature matrix and compute similarity matrix
  tfidfsim <- tfidfsim(dfmsource, minecraftcorpusdf$conversation[minecraftcorpusdf$file == x][1])
  
  # average effort for each of g's utterances
  lines <- 1:nrow(tfidfsim)
  sims <- c()
  for(line in lines) {
    prevutsims <- tfidfsim[line, 1:(line-1)]
    simmax <- max(prevutsims)
      # weight <- 1/(1 + length(prevutsims) - which.max(prevutsims))
    sims <- append(sims, simmax)
  }
  sims
}
addsim <- function(dfmsource, x) {
  thisconvo <- collapseturns(readminecraftconvdf(x))
  thisconvo <- cbind(thisconvo, data.frame(similarity = utterance.sim.simmax(dfmsource, x)))
  thisconvo[1, "similarity"] <- NA
  thisconvo
}
filenames <- list.files("/Users/louisteitelbaum/R.Projects/MinecraftCorpus/MinecraftCorpusDialogues")
minecraftcorpusdf <- lapply(X = filenames, FUN = addsim, dfmsource = minecraftcorpus_tfidf)
minecraftcorpusdf <- do.call(rbind, minecraftcorpusdf)

# orthographic length
minecraftcorpusdf$length <- nchar(minecraftcorpusdf$text)

# repair
minecraftcorpusdf$repair <- grepl("\\?", minecraftcorpusdf$text) & minecraftcorpusdf$role == "B"

# tfidfmean
minecraftcorpusdf$tfidfmean <- rowMeans(as.matrix(minecraftcorpus_tfidf_mat), na.rm = TRUE)

# tfidfsum
minecraftcorpusdf$tfidfsum <- rowSums(as.matrix(minecraftcorpus_tfidf_mat), na.rm = TRUE)

  # aggregate data
library(tidyverse)
    # this one is useful for the vec_rep_each function
library(vctrs)

d <- minecraftcorpusdf %>%
  group_by(conversation, role, participant) %>%
  summarise(
    avglength = mean(length, na.rm = T),
    avgtfidfmean = mean(tfidfmean, na.rm = T),
    avgsim = mean(similarity, na.rm = T),
    n = n()
    ) %>%
  pivot_wider(names_from = role, values_from = c(participant, avglength, avgtfidfmean, avgsim, n)) %>%
  group_by(conversation) %>%
  summarise(
    participant_b = max(participant_B, na.rm = T),
    participant_a = max(participant_A, na.rm = T),
    avglength_b = max(avglength_B, na.rm = T),
    avglength_a = max(avglength_A, na.rm = T),
    avgtfidfmean_b = max(avgtfidfmean_B, na.rm = T),
    avgtfidfmean_a = max(avgtfidfmean_A, na.rm = T),
    avgsim_b = max(avgsim_B, na.rm = T),
    avgsim_a = max(avgsim_A, na.rm = T),
    n_b = max(n_B, na.rm = T),
    n_a = max(n_A, na.rm = T)
  )

d <- d %>%
  group_by(participant_b) %>%
  mutate(
    blen = mean(avglength_b),
    btfidf = mean(avgtfidfmean_b),
    bsim = mean(avgsim_b)
  ) %>%
  group_by(participant_a) %>%
  mutate(
    alen = mean(avglength_a),
    atfidf = mean(avgtfidfmean_a),
    asim = mean(avgsim_a)
  ) %>%
  ungroup(participant_a)

totalrepairs <- minecraftcorpusdf %>%
  group_by(conversation) %>%
  summarise(
    repairs = sum(repair)
  )

d$repairfreq <- totalrepairs$repairs / d$n_a

    # full participant aggregates
agglengthsb <- d %>%
  group_by(participant_b) %>%
  summarise(
    blen = mean(blen),
    btfidf = mean(btfidf),
    bsim = mean(bsim),
    participant = participant_b
  ) %>%
  arrange(participant_b)

agglengthsa <- d %>%
  group_by(participant_a) %>%
  summarise(
    alen = mean(alen),
    atfidf = mean(btfidf),
    asim = mean(asim),
    participant = participant_a
  ) %>%
  arrange(participant_a)

aggs <- merge(x = agglengthsb,
                    y = agglengthsa,
                    by = "participant",
                    all.x = TRUE)

aggs$agglen <- (aggs$blen + aggs$alen) / 2
aggs$aggtfidf <- (aggs$btfidf + aggs$atfidf) / 2
aggs$aggsim <- (aggs$bsim + aggs$asim) / 2

aggs <- aggs %>%
  group_by(participant) %>%
  summarise(
    agglen = mean(agglen),
    aggtfidf = mean(aggtfidf),
    aggsim = mean(aggsim)
  )

d <- d %>%
  arrange(participant_b)
pnumber <- as.vector(rle(d$participant_b)[1])
agglengths <- vec_rep_each(aggs$agglen, pnumber[["lengths"]])
aggtfidfs <- vec_rep_each(aggs$aggtfidf, pnumber[["lengths"]])
aggsims <- vec_rep_each(aggs$aggsim, pnumber[["lengths"]])

d <- d %>%
  arrange(participant_b) %>%
  mutate(
    agglen = agglengths,
    aggtfidf = aggtfidfs,
    aggsim = aggsims
  )
