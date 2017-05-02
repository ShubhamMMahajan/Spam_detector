#Change the working directory before running the code
setwd("C:\\Users\\shubh\\Documents\\GitHub\\Spam_detector")
spam_data <- read.csv("C:\\Users\\shubh\\Documents\\GitHub\\Spam_detector\\spam.csv", header = TRUE, check.names = TRUE)

library(rpart)
spam_data <- as.data.frame(spam_data)
print(is.data.frame(spam_data))
#list of words I consider spam
free_word = list("free", "free of cost", "free of charge", "bonus")
congratulations = list("congratulations", "congrats", "win", "winner", "won")
thanks = list("thanks", "thank", "thank you")
other_spam = list("urgent", "private", "pls", "sms", "txts", "critical", "crucial", "reply")

i = 0
#create new columns and by default set all rows of the column equal to false
spam_data["free_word"] <- NA
spam_data$free_word <- FALSE

spam_data["congrats_word"] <- NA
spam_data$congrats_word <- FALSE

spam_data["thanks"] <- NA
spam_data$thanks <- FALSE

spam_data["other_spam_word"] <- NA
spam_data$other_spam_word <- FALSE
spamFilter <- function(text_data = spam_data) {
    for (texts in text_data$v2) {
        i = i + 1
        list_of_words = unlist(strsplit(texts, " "))
        list_of_words <- tolower(list_of_words)
        for (j in 1:length(list_of_words)) { #iterates through each word
            list_of_words[j] <- gsub("[[:punct:]]", "", list_of_words[j]) #gets rid of punctuation from words
            #if the text contains any words from the list specified above then change the appropriate column to true
            if(list_of_words[j] %in% free_word){
                text_data[i, 6] <- TRUE
            }
            else if(list_of_words[j] %in% congratulations) {
                text_data[i, 7] <- TRUE
            }
            else if(list_of_words[j] %in% thanks) {
                text_data[i, 8] <- TRUE
            }
            else if(list_of_words[j] %in% other_spam) {
                text_data[i, 9] <- TRUE
            }
        }
    }
    
    return(text_data)
}

spam_data = spamFilter()
s = sample(5572,5000)

spam_train <- spam_data[s, ]
spam_test <- spam_data[-s, ]

library(rpart)
library(rpart.plot)

tree <- rpart(v1 ~ free_word + congrats_word + thanks + other_spam_word, spam_train, method = "class")
rpart.plot(tree)
p <- predict(tree, spam_test, type = "class")
table(spam_test$v1, p)



