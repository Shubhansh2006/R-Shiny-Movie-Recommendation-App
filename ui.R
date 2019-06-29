#ui.R
library(shiny)
library(shinythemes)
genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")
Movie_find<-read.csv('movie_finder_data.csv',stringsAsFactors = F)
Movie_find$title<- gsub(', The','',Movie_find$title)
Movie_find$title<- gsub(', A','',Movie_find$title)
movie_list<-as.character(Movie_find$title)

shinyUI(

navbarPage(theme = shinytheme("slate"),
title = div(img(src = "https://media.giphy.com/media/FQkGpU34nbXrO/giphy.gif", width = "250px", height = "48px", style="margin-top: -14px; margin-right:-14px;margin-left:-14px", height = 50)),
  tabPanel(tags$b('Recommend Movie'),
           
           fluidRow(h3("Select the movies you like") ,
             wellPanel(
                        fluidRow(column(6,selectInput("input_genre", "Genre #1",genre_list)),
                        column(6,uiOutput("ui"))),
                        fluidRow(column(6,selectInput("input_genre2", "Genre #2",genre_list)),
                        column(6,uiOutput("ui2") )),
                        fluidRow(column(6,selectInput("input_genre3", "Genre #3",genre_list)),
                        column(6,uiOutput("ui3")))
               #submitButton("Update List of Movies")
                      )
           ),
           wellPanel(
                      fluidRow(
                                column(3,sliderInput("year", label = "Select Year Range", min = 1902,max = 2015, value = c(1902, 2015))),
                                column(2,checkboxGroupInput("genre1", "Select Genres",
                                                           c("Action" = "Action",
                                                             "Adventure" = "Adventure",
                                                            "Animation" = "Animation",
                                                            "Children"="Children" ))),
                                              column(2,checkboxGroupInput("genre2", "",
                                                          c("Comedy" = "Comedy",
                                                            "Crime" = "Crime",
                                                            "Documentary" = "Documentary",
                                                            "Drama"="Drama" ))),
                                            column(2,checkboxGroupInput("genre3", "",
                                                          c("Fantasy" = "Fantasy",
                                                            "Film Noir" = "Film.Noir",
                                                            "Horror" = "Horror",
                                                            "Musical"="Musical" ))),
                                            column(2,checkboxGroupInput("genre4", "",
                                                          c("Mystery" = "Mystery",
                                                            "Romance" = "Romance",
                                                            "Sci Fi" = "Sci.Fi",
                                                            "Thriller"="Thriller" ))),
                      column(4,h3("You Might Like The Following Movies as well"),tableOutput("table"))   
                          )
                  )   
          )
  ,
   tabPanel(tags$b('Search Movie'),
    fluidRow(column(1),column(8,selectInput("Movie_chosen", "Select your movie",movie_list))),
    wellPanel(
              fluidRow(column(1),
              column(8,
              fluidRow(column(4,uiOutput("Movie_chosen"))),
              fluidRow(column(4,uiOutput("movie_desc"))),
              br(),
              br(),
              fluidRow(column(3,uiOutput("Imdb_Link")))
              ),column(2,uiOutput('Image'))
            )
    )
    
  ),
#### Making tag panel heading bold 
tabPanel(tags$b('About'),
         tags$hr(),
         div(
           tags$img(src='purdue-university-logo.jpg', height=100, width=300), style="text-align: center; margin-bottom:10px;"),
         h3('This project has been developed by 2019 MS-BAIM students from Krannert School of Management at Purdue University.',style="text-align: center"),
         div(  
           div(tags$img(src='img1.jpg', width=220, height=340),style="width: 25%;text-align: center;display: inline-block;float: left;"),
           div(tags$img(src='img2.jpg', width=220, height=340),style="width: 25%;text-align: center;display: inline-block;float: left;"),
           div(tags$img(src='img3.jpg', width=220, height=340),style="width: 25%;text-align: center;display: inline-block;float: left;"),
           div(tags$img(src='img4.jpg', width=220, height=340),style="width: 25%;text-align: center;display: inline-block;float: left;"),
           style="text-align: center;width: 100%; height: 30px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
         div(  
           div(h5("Shubhansh Jain is a 2019 Purdue MS- Business Analytics and Information Management student having interests in Healthcare, Safety and Retail domains. He is particularly interested in business risk analysis and consulting. One day he wants to use anaytics in field of human behaviour analysis. His hobbies include playing chess, guitar and solving puzzles."),style="width: 20%; margin-left:2.5%; margin-right: 2.5%; text-align: center;display: inline-block;float:left;border-radius: 40px "),
           div(h5("Cassandra  Liu is a 2019 Purdue MS- Business Analytics and Information Management student having interests in Finance and Management Consulting domains. Her expertise is in performing market predictive analytics and she is also intrested to work in entertainment industry."),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%"),
           div(h5("Sudeep Kurian is a 2019 Purdue MS- Business Analytics and Information Management student having interests in Retail, Logistics, Safety and Compliance domains. He is particularly interested in the technological aspects of the industry specifically, machine learning and neural networks. He likes to play soccer and guitar."),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
           div(h5("Muthuraja Palaniappan is a 2019 Purdue MS- Business Analytics and Information Management student having interests in Finance and Marketing domains. His area of iterest includes machine learning. He is keen to contribute in finance industry using Business Analytics. He loves playing tennis and writing blogs on public speaking."),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
           style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
         div(  
           div(h5("You can get to know more about him :"),tags$a(href="https://www.linkedin.com/in/shubhansh-jain/","here"),style="width: 20%; margin-left:2.5%; margin-right: 2.5%; text-align: center;display: inline-block;float:left;border-radius: 40px "),
           div(h5("You can get to know more about her :"),tags$a(href="https://www.linkedin.com/in/cassandra-yachu-liu-7939a9104/","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%"),
           div(h5("You can get to know more about him :"),tags$a(href="https://www.linkedin.com/in/sudeep-kurian-75721614b/","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
           div(h5("You can get to know more about him :"),tags$a(href="https://www.linkedin.com/in/muthuraja-palaniappan-7307bb7/","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
           style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;")
)
)
)
