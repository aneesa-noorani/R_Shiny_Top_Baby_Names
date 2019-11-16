#installing packages
if(!require('readxl'))install.packages('readxl')
if(!require('ggplot2'))install.packages('ggplot2')
if(!require('plotly'))install.packages('plotly')
if(!require('shiny'))install.packages('shiny')
if(!require('shinythemes'))install.packages('shinythemes')
pacman::p_load(readxl, ggplot2, plotly, shiny, shinythemes)

#only uploaded data, didn't include column names or anything
Top_Girl_Names <- read_excel("Top100_Popular_Baby_Names.xlsx", 
                             sheet = "Girls' Names", range = "C8:GN107", col_names = FALSE)

#deleting every third column since it's blank
#these are the columns to delete
cols_to_delete <- seq(3, ncol(Top_Girl_Names), 3)
#new df with every 3rd column removed
Top_Girl_Names <- Top_Girl_Names[,-cols_to_delete]

#need to name columns. First creating array of column names, then will paste
name_id <- seq(1,129,2)
counts_id <- seq(2,130,2)
count_name <- paste("counts",1954:2018,sep="_")
name_years <- paste("Names", 1954:2018, sep = "_")

#assigning column names by pasting from above
colnames(Top_Girl_Names)[counts_id] <- count_name
colnames(Top_Girl_Names)[name_id] <- name_years

#now, same thing for boys
Top_Boy_Names <- read_excel("Top100_Popular_Baby_Names.xlsx", 
                            sheet = "Boys' Names", range = "C8:GN107", col_names = FALSE)

#deleting every third column, beginning at column 3
cols_to_delete <- seq(3, ncol(Top_Boy_Names), 3)
Top_Boy_Names <- Top_Boy_Names[,-cols_to_delete]

#naming columns. don't need to create column names, already have from above
colnames(Top_Boy_Names)[counts_id] <- count_name
colnames(Top_Boy_Names)[name_id] <- name_years

#need list of unique girl names
girl_only_names <- Top_Girl_Names[,name_id]
unique_Girl_Names <- unique(unlist(girl_only_names))

#need list of unique boy names
boy_only_names <- Top_Boy_Names[,name_id]
unique_Boy_Names <- unique(unlist(boy_only_names))

## ______ creating Shiny app _____ ##

ui <- fluidPage(
    navbarPage(title = "Shiny Assignment 1", theme = shinytheme("cerulean"),
               #first tab
               tabPanel("Top 10 Girls' Names", 
                        selectInput(inputId = "girl_year", 
                                    label = strong("Which year would you like to see girl names for?"),
                                    choices = seq(1954,2018),
                                    selected = 2018),
                        tableOutput("Top 10 Girls' Names")
               ),
               #second tab
               tabPanel("Top 10 Boys' Names",
                        selectInput(inputId = "boy_year", 
                                    label = strong("Which year would you like to see boy names for?"),
                                    choices = seq(1954,2018),
                                    selected = 2018),
                        tableOutput("Top 10 Boys' Names")
               ),
               #third tab
               tabPanel("Girl Name Popularity Over Time",
                        selectInput(inputId = "girl_name", 
                                    label = strong("Which female name would you like to see popularity for?"),
                                    choices = unique_Girl_Names,
                                    selected = unique_Girl_Names[2]),
                        plotlyOutput(outputId = "lineplot_girl", height = "600px")
               ),
               
               #fourth tab
               tabPanel("Boy Name Popularity Over Time",
                        selectInput(inputId = "boy_name", 
                                    label = strong("Which male name would you like to see popularity for?"),
                                    choices = unique_Boy_Names,
                                    selected = unique_Boy_Names[3]),
                        plotlyOutput(outputId = "lineplot_boy", height = "600px")
               )
            )
        ) #end of ui


server <- function(input, output, session) {
    #output for first tab
    output$"Top 10 Girls' Names" <- renderTable({
        #creating vector that combines the selected year with 'Names' and 'counts' to mimic the column names I have
        col<-c(paste("Names",input$girl_year,sep="_"),paste("counts",input$girl_year,sep="_"))
        
        #selecting those particular rows from my dataset
        selected_Top_Girl_Names <- Top_Girl_Names[,col]
        
        #making sure counts column is an integer
        selected_Top_Girl_Names[,2] <- as.integer(unlist(selected_Top_Girl_Names[,2]))
        
        #sorting 
        girl_ordered <- selected_Top_Girl_Names[order(-selected_Top_Girl_Names[2]),]
        head(x=girl_ordered, n=10) #we only want to display the top 10 from ordered list
    })
    
    #output for second tab
    output$"Top 10 Boys' Names" <- renderTable({
        #creating vector that combines the selected year with 'Names' and 'counts' to mimic the column names I have
        col<-c(paste("Names",input$boy_year,sep="_"),paste("counts",input$boy_year,sep="_"))
        
        #selecting those particular rows from my dataset
        selected_Top_Boy_Names <- Top_Boy_Names[,col]
        
        #making sure counts column is an integer
        selected_Top_Boy_Names[,2] <- as.integer(unlist(selected_Top_Boy_Names[,2]))
        
        #sorting 
        boy_ordered <- selected_Top_Boy_Names[order(-selected_Top_Boy_Names[2]),]
        head(x=boy_ordered, n=10) #we only want to display the ordered list
    })
    
    #output for third tab
output$"lineplot_girl" <- renderPlotly({
    rank_seq <- seq(1:100)
    #creating new dataframe with column for rankings, like original
    
    Girl_Names_w_Ranking <- cbind(rank_seq, Top_Girl_Names)
        
    ##creating new data frame with just the ranks for all the years for a given name
    Girl_Name_rank <- data.frame(year = seq(1954,2018), Rank_Order = 0)
        
    #for loop to select ranking for a given name for each year
    #for each year
    for (i in seq(1954,2018)){
        #select Names column
        name_column <- c(paste("Names",i,sep="_"))
        #create unlisted vector that selects just name column  for a given year
        Rank_unlist = unlist(Girl_Names_w_Ranking[,name_column])
        #modifying our Girl_Name_rank dataframe to select name from name column,
        #and then select row index
        Girl_Name_rank[Girl_Name_rank$year==i,'Rank_Order'] <- 
            if(input$girl_name %in% Rank_unlist)which(Rank_unlist==input$girl_name)
            else ranking = 101
        }
        
        #need to reverse the rankings for plotting
        reverse_rank <- c(101 - Girl_Name_rank[,2])
        Girl_Name_rank_reverse <- cbind(year = Girl_Name_rank$year, reverse_rank)
        #convert to data frame
        Girl_Name_rank_reverse <- as.data.frame(Girl_Name_rank_reverse)
        class(Girl_Name_rank_reverse)
        
         p <- ggplot(data = Girl_Name_rank_reverse, 
            mapping = aes(Girl_Name_rank_reverse$year, Girl_Name_rank_reverse$reverse_rank,
                       group = 1,
                       #text = : this part defines what displays when you hover
                        text = paste0("Year: ", Girl_Name_rank_reverse$year, "<br>", 
                                      "Rank: ", Girl_Name_rank_reverse$reverse_rank))) +
            geom_line(color='red', size = 1.5) + 
            labs(
                title = paste("Popularity of Name '",input$girl_name,"' Over Time", sep = ""),
                caption = "Rank of 100 is highest popularity level"
            ) +
            theme(
                plot.title = element_text(hjust = 0.5 ,color = "black", size = 18, face = "bold"),
                plot.caption = element_text(size = 14 ,face = "italic")
            ) +
            xlab("Year") + ylab("Popularity Rank") +
            scale_x_continuous(name = "Year", breaks = seq(1954, 2018, 4), limits = c(1954,2018))
        
         #parse down ggplot from above down to only include 'text' as hover
          p %>% ggplotly(tooltip = "text") %>% plotly_build()
            
    })
    
    #output for fourth tab
    output$"lineplot_boy" <- renderPlotly({
        rank_seq <- seq(1:100)
        Boy_Names_w_Ranking <- cbind(rank_seq, Top_Boy_Names)
        
        ##creating new data frame with just the ranks for all the years for a given name
        Boy_Name_rank <- data.frame(year = seq(1954,2018), Rank_Order = 0)
        
        #for loop to select ranking for a given name for each year
        #for each year
        for (i in seq(1954,2018)){
            #select Names column
            name_column <- c(paste("Names",i,sep="_"))
            #create unlisted vector that selects just name column  for a given year
            Rank_unlist = unlist(Boy_Names_w_Ranking[,name_column])
            #modifying our Boy_Name_rank dataframe to select name from name column,
            #and then select row index
            Boy_Name_rank[Boy_Name_rank$year==i,'Rank_Order'] <- 
                if(input$boy_name %in% Rank_unlist)which(Rank_unlist==input$boy_name)
            else ranking = 101
        }
        
        #need to reverse the rankings for plotting
        reverse_rank <- c(101 - Boy_Name_rank[,2])
        Boy_Name_rank_reverse <- cbind(year = Boy_Name_rank$year, reverse_rank)
        #convert to data frame
        Boy_Name_rank_reverse <- as.data.frame(Boy_Name_rank_reverse)
        
        p <- ggplot(data = Boy_Name_rank_reverse, 
                    mapping = aes(Boy_Name_rank_reverse$year, Boy_Name_rank_reverse$reverse_rank,
                                  group = 1,
                                  text = paste0("Year: ", Boy_Name_rank_reverse$year, "<br>", 
                                                "Rank: ", Boy_Name_rank_reverse$reverse_rank))) +
            geom_line(color='blue', size = 1.5) + 
            labs(
                title = paste("Popularity of Name '",input$boy_name,"' Over Time", sep = ""),
                caption = "Rank of 100 is highest popularity level"
            ) +
            theme(plot.title = element_text(hjust = 0.5 ,color = "black", size = 18, face = "bold"),
                plot.caption = element_text(size = 14 ,face = "italic")
            ) +
            xlab("Year") + ylab("Popularity Rank") +
            scale_x_continuous(name = "Year", breaks = seq(1954, 2018, 4), limits = c(1954,2018))
        
        #parse down ggplot from above down to only include 'text' as hover
        p %>% ggplotly(tooltip = "text") %>% plotly_build()
        
    }) #end for output for 4th tab
    
} #end of server fxn

shinyApp(ui = ui, server = server)
