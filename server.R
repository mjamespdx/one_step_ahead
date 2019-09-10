#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(dplyr)
library(stringr)
library(ngram)

load('data/dt2.rds')
load('data/dt3.rds')
load('data/dt4.rds')
load('data/dt5.rds')


nextWord <- function(x)
{
    # for no input
    if(x == '')
        return("There doesn't seem to be anything to predict...")
    
    # for tracking multipliers and searches
    alpha <- 1
    flag4 <- 0
    flag3 <- 0
    flag2 <- 0
    flag1 <- 0
    
    # clean up input
    x <- removePunctuation(x) %>% tolower() %>% stripWhitespace() %>%
        trimws('right')
    
    # truncate long strings
    if(wordcount(x) > 4)
    {
        x <- word(x, -c(4:1))
        x <- paste(x[[1]], x[[2]], x[[3]], x[[4]], sep = ' ')
    }
    
    # search in 5-grams
    if(wordcount(x) == 4)
    {
        xSearch <- gsub(' ','_',x)
        searchTerm <- paste('^',xSearch,'_*', sep = '')
        matchesN4 <- dt5[grep(pattern = searchTerm, dt5$Content),
                         ]$Frequency/sum(dt5[grep(pattern = searchTerm, dt5$Content),]$Frequency)
        names(matchesN4) <- dt5[grep(pattern = searchTerm, dt5$Content),]$Content
        
        
        # check if more matches needed
        if(length(matchesN4) < 5)
        {
            x <- word(x, -c(3:1))
            x <- paste(x[[1]], x[[2]], x[[3]], sep = ' ')
        }
        
        flag4 <- 1
    }
    
    # search in 4-grams
    if(wordcount(x) == 3)
    {
        if(flag4 == 1)
            alpha <- 0.4
        
        xSearch <- gsub(' ','_',x)
        searchTerm <- paste('^',xSearch,'_*', sep = '')
        matchesN3 <- dt4[grep(pattern = searchTerm, dt4$Content),
                         ]$Frequency/sum(dt4[grep(pattern = searchTerm, dt4$Content),]$Frequency)
        names(matchesN3) <- dt4[grep(pattern = searchTerm, dt4$Content),]$Content
        
        
        # check if more matches needed
        if(length(matchesN3) < 5 )
        {
            x <- word(x, -c(2:1))
            x <- paste(x[[1]], x[[2]], sep = ' ')
        }
        
        flag3 <- 1
    }
    
    # search in 3-grams
    if(wordcount(x) == 2)
    {
        if(flag4 == 1 & flag3 == 1)
            alpha <- 0.4^2
        else if(flag4 == 0 & flag3 == 1)
            alpha <- 0.4
        
        xSearch <- gsub(' ','_',x)
        searchTerm <- paste('^',x,'_*', sep = '')      
        matchesN2 <- dt3[grep(pattern = searchTerm, dt3$Content),
                         ]$Frequency/sum(dt3[grep(pattern = searchTerm, dt3$Content),]$Frequency)
        names(matchesN2) <- dt3[grep(pattern = searchTerm, dt3$Content),]$Content
        
        
        # check if more matches needed
        if(length(matchesN2) < 5)
            x <- word(x, -1)
        
        flag2 <- 1
    }
    
    # search in 2-grams
    if(wordcount(x) == 1)
    {
        if(flag4 == 1 & flag3 == 1 & flag2 == 1)
            alpha <- 0.4^3
        else if(flag4 == 0 & flag3 == 1 & flag2 == 1)
            alpha <- 0.4^2
        else if(flag4 == 0 & flag3 == 0 & flag2 == 1)
            alpha <- 0.4
        
        searchTerm <- paste('^',x,'_*', sep = '')
        matchesN1 <- dt2[grep(pattern = searchTerm, dt2$Content),
                         ]$Frequency/sum(dt2[grep(pattern = searchTerm, dt2$Content),]$Frequency)
        names(matchesN1) <- dt2[grep(pattern = searchTerm, dt2$Content),]$Content
        
        flag1 <- 1
    }
    
    # construct the return vector based on searches made
    if(flag4 == 1 & flag3 == 0)
        words <- matchesN4
    else if(flag4 == 0 & flag3 == 1 & flag2 == 0)
        words <- matchesN3
    else if(flag4 == 1 & flag3 == 1 & flag2 == 0)
        words <- c(matchesN4, matchesN3)
    else if(flag4 == 1 & flag3 == 1 & flag2 == 1 & flag1 == 0)
        words <- c(matchesN4, matchesN3, matchesN2)
    else if(flag4 == 1 & flag3 == 1 & flag2 == 1 & flag1 == 1)
        words <- c(matchesN4, matchesN3, matchesN2, matchesN1)
    else if(flag4 == 0 & flag3 == 1 & flag2 == 1 & flag1 == 1)
        words <- c(matchesN3, matchesN2, matchesN1)
    else if(flag4 == 0 & flag3 == 1 & flag2 == 1 & flag1 == 0)
        words <- c(matchesN3, matchesN2)
    else if(flag4 == 0 & flag3 == 0 & flag2 == 1 & flag1 == 1)
        words <- c(matchesN2, matchesN1)
    else if(flag4 == 0 & flag3 == 0 & flag2 == 1 & flag1 == 0)
        words <- matchesN2
    else
        words <- matchesN1
    
    # the home stretch
    words <- words[order(words, decreasing = TRUE)]
    names(words) <- gsub('_', ' ', names(words)) %>% word(-1)
    words <- words[unique(names(words))] %>% as.table()
    return(words[1:5])
}

shinyServer(function(input, output) {

    output$userOut <- renderTable(colnames = FALSE, striped = FALSE, 
                                  hover = FALSE, width = '100%', {
                                      nextWord(input$userIn)
                                  })
    
})