#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(topicmodels)
library(shinythemes)
# Define UI for application that draws a histogram
ui <-dashboardPage(skin = "green",
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),          
    dashboardHeader(title = "Team 5 Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("step-forward")),
        menuItem("ZIPF's Law", tabName = "zip", icon = icon("gavel")),
        menuItem("Cultural barriers", tabName = "barriers", icon = icon("thumbs-down")),
        menuItem("Professional background", tabName = "professional", icon = icon("user-tie")),
        menuItem("LDA", tabName = "lda", icon = icon("list")),
        menuItem("Sentiment", tabName = "sentiment", icon = icon("sad-cry")),
        menuItem("Conclusion", tabName = "conclusion", icon = icon("thumbs-up")),
        menuItem("References", tabName = "reference", icon = icon("exclamation"))
        
    )),
    dashboardBody(tabItems(
        # First tab content -> zip
        tabItem(tabName = "zip",fluidPage(
          theme = shinytheme("journal"),
        
        # Application title
        titlePanel("ZIPF's Law"),
        
        # Sidebar with a slider input for intercept in ZIPF's Law
        sidebarLayout(
            box(title = "Inputs", status = "warning",solidHeader = TRUE,width = 4,
                sliderInput("intercept",
                            "Intercept",
                            min = -0.20,
                            max = 0.50,
                            value = -0.50),
                sliderInput("slope",
                            "Slope",
                            min = -4.0,
                            max = -1.0,
                            value = -1.1),
                sliderInput("size",
                            "Size",
                            min = 1.0,
                            max = 4.0,
                            value = 0.5),
                sliderInput("linetype",
                            "Intercept 
                        Line type",
                            min = 1.0,
                            max = 4.0,
                            value = 2)
            ),
            # Show a plot of the generated distribution
            box(title = "ZIPF's Law", status = "primary",solidHeader = TRUE, width = 8,
                plotOutput("distPlot")
                
            )
        )
    )),
    # Second tab content -> zip
    tabItem(tabName = "lda",
            fluidPage(
                # Application title
                titlePanel("LDA Analysis"),
                
                box(title = "Inputs", status = "warning",solidHeader = TRUE,width = 4,
                    sliderInput("klusters",
                                "Select number of Cluster",
                                min = 2.0,
                                max = 4.0,
                                value = 4.0)
                    ),
                box(title = "LDA per documnet", status = "primary",solidHeader = TRUE, width = 8,height = 600,
                plotOutput("lda")
                )
                )
            ),
    # Third tab content -> barriers
    tabItem(tabName = "barriers",
            fluidPage(
                
                box(title = "Inputs", status = "warning",solidHeader = TRUE,width = 4,
                    sliderInput("variable_barrier",
                                "Select number diplayed",
                                min = 7.0,
                                max = 10.0,
                                value = 10.0),
                    radioButtons("color", "Select color:",
                                 c(
                                   "Red" = "#781f1f",
                                   "Purple" = "#4d2749"
                                   )),
                ),
                box(title = "What kind of cultural barriers do you have with your team?", status = "primary",solidHeader = TRUE, width = 8,height = 600,
                    plotOutput("barriers")
                )
            )
    ),
    # Fourth tab content -> professional
    tabItem(tabName = "professional",
            fluidPage(
                
                box(title = "Inputs", status = "warning",solidHeader = TRUE,width = 4,
                    sliderInput("variable_professional",
                                "Select number diplayed",
                                min = 7.0,
                                max = 10.0,
                                value = 10.0)
                ),
                box(title = "Describe the professional background of your teammates?", status = "primary",solidHeader = TRUE, width = 8,height = 600,
                    plotOutput("professional")
                )
            )
    ),
    # First  tab content -> intro
    tabItem(tabName = "conclusion",
            fluidPage(
                      img(src = 'conclusion.png', style="max-width: 100%; height: auto; margin-bottom: 25px;")
                     
                
            )
    ),
    # Last  tab content -> conclusion
    tabItem(tabName = "intro",
            fluidPage(headerPanel("Introduction"),
                      img(src = 'image1.png', style="max-width: 100%; height: auto; margin-bottom: 25px;"),
                      img(src = 'image2.png', style="max-width: 100%; height: auto; margin-bottom: 25px;"),
                      img(src = 'image3.png', style="max-width: 100%; height: auto; margin-bottom: 25px;"),
                      img(src = 'image4.png', style="max-width: 100%; height: auto; margin-bottom: 25px;")
                      
            )
    ),
    # First  tab content -> intro
    tabItem(tabName = "reference",
            fluidPage(
              
              img(src = 'refence.png', style="max-width: 100%; height: auto; margin-bottom: 25px;")
              
            )
    ),
    # Fifth   tab content -> sentiment
    tabItem(tabName = "sentiment",
            fluidPage(
                
                box(title = "Inputs", status = "warning",solidHeader = TRUE,width = 4,
                    selectInput("question", "What question do you want to see?",
                                choices = c("How did you manage to solve conflicts with your teammates?"="Question5", 
                                            "What kind of cultural barriers do you have with your team? "="Question4",
                                            "Which conflicts do you have with your teammates? "="Question3" ,
                                            "Describe the professional background of your teammates? "="Question2" ))
                ),
                box(title =  textOutput("selected_var"), status = "primary",solidHeader = TRUE, width = 8,height = 600,
                    plotOutput("sentiment")
                )
            )
    )
    )
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
##########First part##########
    #necessary libraries
    library(textreadr)
    library(dplyr)
    library(magrittr)
    library(tidyr)
    library(scales)
    library(NLP)
    library(tm)
    library(dplyr)
    library(tidytext)
    library(textdata)
    library (reshape2)
    library(tidyverse)
    library(twitteR)
    library(igraph)
    library(tidytext)
    library(ggplot2)
    library(reshape2)
    library(wordcloud)
    library(ggraph)
    library(scales)
    library(readtext)
    library(stringr)
    #Reading the files 
    file <- "Answer.csv"
    answer_df <- read_csv(file)
    #as charcter transformation
    answer_df$Question1 <- as.character(answer_df$Question1)
    answer_df$Question2 <- as.character(answer_df$Question2)
    answer_df$Question3 <- as.character(answer_df$Question3)
    answer_df$Question4 <- as.character(answer_df$Question4)
    answer_df$Question5 <- as.character(answer_df$Question5)
    answer_df$Question6 <- as.character(answer_df$Question6)
    #Junk words
    junk <- data_frame(word= c('backgrounds','background','rum', 'I', 'i', 'i\'m', 'try', 'didn\'t', 'had', 'one', 'still', 'any', 'null', 'NA','NULL',  NULL, NA), lexicon = rep('junk',each = 16))
    stop_words_total <- rbind(stop_words,junk)
############TOKENIZE######
#tokenize question 1
answer_df_question_1_df <- answer_df%>%
    unnest_tokens(word, Question1) %>%
    anti_join(stop_words_total) %>%
    count(word, sort = TRUE)
#tokenize question 2
answer_df_question_2_df <- answer_df%>%
    unnest_tokens(word, Question2) %>%
    anti_join(stop_words_total) %>%
    count(word, sort = TRUE)
#tokenize question 3
answer_df_question_3_df <- answer_df%>%
    unnest_tokens(word, Question3) %>%
    anti_join(stop_words_total) %>%
    count(word, sort = TRUE)
#tokenize question 4
answer_df_question_4_df <- answer_df%>%
    unnest_tokens(word, Question4) %>%
    anti_join(stop_words_total) %>%
    count(word, sort = TRUE)
#tokenize question 5
answer_df_question_5_df <- answer_df%>%
    unnest_tokens(word, Question5) %>%
    anti_join(stop_words_total) %>%
    count(word, sort = TRUE)
#tokenize question 6
answer_df_question_6_df <- answer_df%>%
    unnest_tokens(word, Question6) %>%
    #anti_join(stop_words_total) %>%
    count(word, sort = TRUE)

    
    
    
    
#########TF_IDF############
question1_text <- answer_df$Question1
question1 <- data_frame(question1_text)%>%
    rename(text = question1_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)

question2_text <- answer_df$Question2
question2 <- data_frame(question2_text)%>%
    rename(text = question2_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)

question3_text <- answer_df$Question3
question3 <- data_frame(question3_text)%>%
    rename(text = question3_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)

question4_text <- answer_df$Question4
question4 <- data_frame(question4_text)%>%
    rename(text = question4_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)

question5_text <- answer_df$Question5
question5 <- data_frame(question5_text)%>%
    rename(text = question5_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)

question6_text <- answer_df$Question6
question6 <- data_frame(question6_text)%>%
    rename(text = question6_text)%>%
    unnest_tokens(word,text)%>%
    anti_join(stop_words_total)


questions <- bind_rows(mutate(question1, question="1"),
                       mutate(question2, question= "2"),
                       mutate(question3, question="3"),
                       mutate(question4, question="4"),
                       mutate(question5, question= "5"),
                       mutate(question6, question="6")
)%>%
    count(question, word, sort=T)%>%
    ungroup()

total_words <- questions %>%
    group_by(question) %>%
    summarize(total=sum(n))

question_words <- left_join(questions, total_words)


question_words <- question_words %>%
    bind_tf_idf(word, question, n)%>%
    arrange(desc(tf_idf))
freq_by_rank <- question_words %>%
    group_by(question) %>%
    mutate(rank = row_number(),
           `term frequency` = n/total)

#############Rendering############
output$distPlot <- renderPlot({
        #let's plot ZIPF's Law
        freq_by_rank %>%
            ggplot(aes(rank, `term frequency`, color=question))+
            #let's add a tangent line , the first derivative, and see what the slop is
            geom_abline(intercept=input$intercept, slope= input$slope, color='gray50', linetype=input$linetype)+
            geom_line(size= input$size, alpha = 0.8, show.legend = FALSE)+
            scale_x_log10()+
            scale_y_log10()

    })
output$lda <- renderPlot({
    dtm_example <- question_words%>%
        cast_dtm(question, word, n)
    question_words_lda <- LDA(dtm_example,k=input$klusters, control = list(seed=123))
    
    question_words_gamma <- tidy(question_words_lda, matrix="gamma")
    question_words_gamma
    
    question_words_topics <- tidy(question_words_lda, matrix="beta")
    question_words_topics
    
    top_terms <- question_words_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    
    
    top_terms %>%
        mutate(term=reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend=FALSE) +
        facet_wrap(~topic, scales = "free") +
        coord_flip()
    
    
    
})
output$barriers <- renderPlot({
    #let's plot ZIPF's Law
    answer_question_4_df_histogram <- answer_df %>%
        unnest_tokens(word, Question4) %>%
        anti_join(stop_words_total) %>%
        count(word, sort=TRUE) %>%
        top_n(input$variable_barrier) %>%
        mutate(word=reorder(word,n)) %>%
        ggplot(aes(word, n))+
        
        geom_col()+
        ggtitle("What kind of cultural barriers do you have with your team?")+
        geom_bar(stat="identity", alpha=0.7, fill=input$color)+
        xlab(NULL)+
        coord_flip() + 
      coord_fixed()
    
    print(answer_question_4_df_histogram)
    #print(answer_question_2_df_histogram)
    
})

output$professional <- renderPlot({
    #let's plot ZIPF's Law
    answer_question_2_df_histogram <- answer_df %>%
        unnest_tokens(word, Question2) %>%
        anti_join(stop_words_total) %>%
        count(word, sort=TRUE) %>%
        top_n(input$variable_professional) %>%
        mutate(word=reorder(word,n)) %>%
        ggplot(aes(word, n, fill=n))+
        geom_col(show.legend = FALSE) +
        geom_col()+
        #ggtitle("Describe the professional background of your teammates?")+
        xlab(NULL)+
        coord_flip()
    print(answer_question_2_df_histogram)
    #print(answer_question_2_df_histogram)
    
})

output$sentiment <- renderPlot({
    
    if (input$question == "Question5"){
        bing_counts <- answer_df %>%
            unnest_tokens(word, Question5) %>%
            anti_join(stop_words_total) %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort=T) %>%
            ungroup()
    }
    if (input$question == "Question4"){
        bing_counts <- answer_df %>%
            unnest_tokens(word, Question4) %>%
            anti_join(stop_words_total) %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort=T) %>%
            ungroup()
    }
    if (input$question == "Question3"){
        bing_counts <- answer_df %>%
            unnest_tokens(word, Question3) %>%
            anti_join(stop_words_total) %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort=T) %>%
            ungroup()
    }
    if (input$question == "Question2"){
        bing_counts <- answer_df %>%
            unnest_tokens(word, Question2) %>%
            anti_join(stop_words_total) %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort=T) %>%
            ungroup()
    }
    

    bing_counts %>%
        group_by(sentiment) %>%
        top_n(2) %>%
        ungroup() %>%
        mutate(word=reorder(word, n)) %>%
        ggplot(aes(word, n, fill=sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y")+
        labs(y="Contribution to sentiment", x=NULL)+
        coord_flip()
   
    
})
output$image2 <- renderImage({
        return(list(
            src = "images/image1.png",
            contentType = "image/png",
            alt = "intro"
        ))
  
    
}, deleteFile = TRUE)

output$selected_var <- renderText({ 
  if (input$question == "Question5"){
    var <- "How did you manage to solve conflicts with your teammates?"
  }
  if (input$question == "Question4"){
   var <-  "What kind of cultural barriers do you have with your team? "
  }
  if (input$question == "Question3"){
    var <- "Which conflicts do you have with your teammates? "
  }
  if (input$question == "Question2"){
    var <-"Describe the professional background of your teammates? "
  }
  paste("", var)
})

}

# Run the application 
shinyApp(ui = ui, server = server)
#