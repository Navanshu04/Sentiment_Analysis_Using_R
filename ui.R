
library(shiny)

shinyUI(fluidPage(
  
  headerPanel("Twitter Sentiment Analysis"),
  
  # Getting User Inputs
  
  sidebarPanel(
    
      
    selectInput("category", "Enter category for analysis", c("Movie", "Product", "People"), selected="Select", selectize=TRUE
              ),
    textInput ("searchTerm","Enter hashtag to be searched with '#'",  "#"),
    sliderInput("maxTweets","Number of recent tweets to use for analysis",min=0,max=10000,value=500), # The max can, of course, be increased
    #sliderInput("noOfDays","No of recent days upto which tweets are retrieved",min=1,max=7,value=7,step=1), 
    submitButton(text="Analyse")    
  ),
  
  mainPanel(
    tabsetPanel(
      
	tabPanel("Top Trending Tweets Today",	
selectInput("trendingTable","Choose location to extract trending tweets", c("Worldwide" ,"Afganistan","Australia","Brazil","Canada","Chiba","France","Germany","India","Japan","Mexico","Pakistan",
                                                                            "Russia","Britain","US"), selected = "Worldwide", selectize = FALSE), 
        submitButton(text="Search"),    HTML("<div><h3>top trending 
	hashtags on Twitter</h3></div>"),
   tableOutput("trendtable"),
			HTML
               ("<div> </div>")),


      tabPanel("WordCloud",HTML("<div><h3>Most used words associated with the hashtag</h3></div>"),plotOutput("word"),
               HTML
               ("<div><h4>word cloud of The Treanding Topic 
			</h4></div>")),

      
      tabPanel("Histogram",HTML
               ("<div><h3> Histograms graphically depict the positivity or negativity of peoples' opinion about of the hashtag
			</h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
               ),
      
      
      tabPanel("Pie Chart",HTML("<div><h3>Depicting sentiment on a scale of 5</h3></div>"), plotOutput("piechart"),HTML
               ("<div><h4></h4></div>")
                
              ),

      
      tabPanel("Table",HTML( "<div><h3> Depicting sentiment in a tablular form on a scale of 5 </h3></div>"), tableOutput("tabledata"),
			HTML ("<div><h4> </h4></div>")),
      
      
      tabPanel("Top Tweeters",HTML("<div><h3> Top 20 Tweeters of hashtag </h3></div>"),plotOutput("tweetersplot"), tableOutput("tweeterstable")),
      
      tabPanel("Top Hashtags of User", textInput("user", "Enter user name", "@"),submitButton(text="Search"), plotOutput("tophashtagsplot"), HTML("<div><h3> Hashtag frequencies in the tweets of the tweeter
			</h3></div>")  )    
    )
  )
  
))