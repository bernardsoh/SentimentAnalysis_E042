library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(tm)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(wordcloud)
library(SnowballC)

data <- read.csv("predict_sentiments_result.csv")
data2 <- read.csv("leehsienloong.csv")

data$Date <- strptime(as.character(data1$date),format="%d/%m/%Y")
data$Date <- as.POSIXct(data1$Date)
data2$Date <- strptime(as.character(data2$date),format="%d/%m/%Y")
data2$Date <- as.POSIXct(data2$Date)

data$Month <- as.numeric(as.character(strftime(data1$Date,format="%m")))
data2$Month <- as.numeric(as.character(strftime(data2$Date,format="%m")))

corpus <- iconv(data$comments)
corpus <- Corpus(VectorSource(corpus))

corpus2 <- iconv(data2$comments)
corpus2 <- Corpus(VectorSource(corpus2))


#Clean text
corpus <- tm_map(corpus, tolower) #convert all the text into lower case

corpus <- tm_map(corpus, removePunctuation) #removing special characters from the text

corpus <- tm_map(corpus, removeNumbers) #remove numbers from the text data

cleanset <- tm_map(corpus, removeWords, stopwords('english')) #remove all stopwords from the text. Stop words mean like "the, is, at, on"

removeURL2 <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL2)) #remove duplicate https links 

cleanset <- tm_map(cleanset, stemDocument) #Text stemming - which reduces words to their root form
cleanset <- tm_map(cleanset, stripWhitespace)

tdm <- TermDocumentMatrix(cleanset) #After cleansing the textual content data, the following step is to matter the incidence of every word, to perceive famous or trending topics.
tdm <- as.matrix(tdm)


#Clean text corpus2
corpus2 <- tm_map(corpus2, tolower) #convert all the text into lower case

corpus2 <- tm_map(corpus2, removePunctuation) #removing special characters from the text

corpus2 <- tm_map(corpus2, removeNumbers) #remove numbers from the text data

cleanset2 <- tm_map(corpus2, removeWords, stopwords('english')) #remove all stopwords from the text. Stop words mean like "the, is, at, on"

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset2 <- tm_map(cleanset2, content_transformer(removeURL)) #remove duplicate https links 

cleanset2 <- tm_map(cleanset2, stemDocument) #Text stemming - which reduces words to their root form
cleanset2 <- tm_map(cleanset2, stripWhitespace)

tdm2 <- TermDocumentMatrix(cleanset2) #After cleansing the textual content data, the following step is to matter the incidence of every word, to perceive famous or trending topics.
tdm2 <- as.matrix(tdm2)

#------------------------------------------------------------------------------------Start of UI.R-------------------------------------------------------------------

ui <- fluidPage(
        
        # App title ----
        titlePanel("Singaporean's Perception Towards Covid-19 By Sentiment Analysis (DIP Project E042)"), br(),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                # Sidebar panel for inputs ----
                
                sidebarPanel(
                        
                        # Input: Slider for the number of bins ----
                        
                        selectInput(inputId="channel1",label="Social Media Page To Analyze:",choices = c("(Facebook) mustsharenews"="C1",
                                                                                                         "(Facebook) govtechsg"="C2",
                                                                                                         "(Twitter) wakeupsingapore"="C3",
                                                                                                         "(Twitter) mothershipsg"="C4",
                                                                                                         "(Instagram) leehsienloong"="C5",
                                                                                                         "(Instagram) moh_singapore"="C6",
                                                                                                         "(Instagram) thenewpaper"="C7"),
                                    selected = "C6",multiple = F),
                        
                        #Include box to download
                        downloadButton("downloaddata", em('Download Prediction',style="text-align:center;color:blue;font-size:100%")), br(),br(),
                        
                        
                        sliderInput(inputId = "range1",
                                    label = "Date Range:",
                                    min = as.Date("2020-01-01","%Y-%m-%d"),
                                    max = as.Date("2021-12-31","%Y-%m-%d"),
                                    value= c(as.Date("2021-12-31"),as.Date("2020-12-31")),
                                    timeFormat="%Y-%m-%d")
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Histogram ----
                        plotOutput(outputId = "Plot1"),
                        plotOutput(outputId = "Plot2"),
                        plotOutput(outputId = "Plot3"),
                        plotOutput(outputId = "Plot4"),
                        plotOutput(outputId = "Plot5"),
                        plotOutput(outputId = "Plot6"),
                        plotOutput(outputId = "Plot7"),
                        plotOutput(outputId = "Plot8")
                        
                )
        )
)


#------------------------------------------------------------------------------------Start of SERVER.R-------------------------------------------------------------------
# Define server logic required to draw a histogram ----
server <- function(input, output){
        
        # 1. It is "reactive" and therefore should be automatically
        #    re-executed when inputs (input$bins) change
        # 2. Its output type is a plot
        
        output$Plot1 <- renderPlot({
                
                my_summary <- data %>% count(sentiments, Month, Date, sort = TRUE)
                p2 <- data %>% ggplot(aes=(x=Date))
                p2 <- ggplot(my_summary, mapping = aes(Date, n, fill = sentiments)) +
                        geom_bar(stat = "identity") +
                        ggtitle("Sentiment Prediction Results: moh_singapore ")
                
                p2
                
                #hist(x, breaks = bins, col = sColor, border = input$border1,
                #xlab = "Waiting time to next eruption (in mins)",
                #main = "Histogram of waiting times")
        })
        
        output$Plot2 <- renderPlot({
                
                w <- rowSums(tdm)
                w <- subset(w, w>=25)
                barplot(w,
                        las = 2,
                        col = rainbow(50),
                        main = 'Frequency of Trending Topics (moh_singapore)')
                
        })
        
        output$Plot3 <- renderPlot({
                
                #image composed of keywords found within a body of text, where the size of each word indicates its count in that body of text
                w <- sort(rowSums(tdm), decreasing = TRUE)
                set.seed(1000)
                wordcloud(words = names(w),
                          freq = w,
                          max.words = 150,
                          random.order = F,
                          min.freq = 5,
                          colors = brewer.pal(8, 'Dark2'),
                          scale = c(5, 0.3),
                          rot.per = 0.7)
        })
        
        output$Plot4 <- renderPlot({
                
                comments <- iconv(data$comments)
                s <- get_nrc_sentiment(comments) #Obtain sentiment scores
                #head(s)
                
                barplot(colSums(s),
                        las = 2,
                        col = rainbow(10),
                        ylab = 'Count',
                        main = 'Sentiment Scores for Comments (moh_singapore)')      
        }) 
        
        output$Plot5 <- renderPlot({
                
                my_summary2 <- data2 %>% count(sentiments, Month, Date, sort = TRUE)
                p4 <- data %>% ggplot(aes=(x=Date))
                p4 <- ggplot(my_summary2, mapping = aes(Date, n, fill = sentiments)) +
                        geom_bar(stat = "identity") +
                        ggtitle("Sentiment Prediction Results: leehsienloong ")
                
                p4
                
                #hist(x, breaks = bins, col = sColor, border = input$border1,
                #xlab = "Waiting time to next eruption (in mins)",
                #main = "Histogram of waiting times")
        })
        
        output$Plot6 <- renderPlot({
                
                w2 <- rowSums(tdm2)
                w2 <- subset(w2, w2>=25)
                barplot(w2,
                        las = 2,
                        col = rainbow(50),
                        main = 'Frequency of Trending Topics (leehsienloong)')
                
        })
        
        output$Plot7 <- renderPlot({
                
                #image composed of keywords found within a body of text, where the size of each word indicates its count in that body of text
                w2 <- sort(rowSums(tdm2), decreasing = TRUE)
                set.seed(100)
                wordcloud(words = names(w2),
                          freq = w2,
                          max.words = 150,
                          random.order = F,
                          min.freq = 5,
                          colors = brewer.pal(8, 'Dark2'),
                          scale = c(5, 0.3),
                          rot.per = 0.7)
        })
        
        output$Plot8 <- renderPlot({
                
                comments2 <- iconv(data2$comments)
                s2 <- get_nrc_sentiment(comments2) #Obtain sentiment scores
                #head(s)
                
                barplot(colSums(s2),
                        las = 2,
                        col = rainbow(10),
                        ylab = 'Count',
                        main = 'Sentiment Scores for Comments (leehsienloong)')      
        })
        
        
}

shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------------End of SERVER.R-------------------------------------------------------------------
