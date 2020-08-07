#Loading required libraries
library(shiny)
library(tidyverse)
library(plotly)

#Reading input files
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

names(salary_potential)[names(salary_potential)=="name"] <- "Name"
names(tuition_cost)[names(tuition_cost)=="name"] <- "Name"

#filtering for colleges with 4 year degree programs 
tc_4year <- tuition_cost %>% filter(degree_length=="4 Year") %>% filter(type=="Private"|type=="Public") %>% select(Name,state,type,room_and_board,in_state_tuition,out_of_state_tuition)
names(tc_4year) <- c("Name","state","type","Roomandboard","Instate","Outstate")
tc_4year <- na.omit(tc_4year)

#creating a dataframe to store avg tuition and salary values 
avg_tc_bystate <- tc_4year %>% group_by(state,type) %>% summarize(outstate=mean(Outstate))
avg_tc_bystate <- spread(avg_tc_bystate,type,outstate)
avg_sp_bystate <- salary_potential %>% group_by(state_name) %>% summarize(mean_early_pay=mean(early_career_pay),mean_mid_pay=mean(mid_career_pay))
names(avg_sp_bystate)[names(avg_sp_bystate)=="state_name"]<-"state"
avg_sp_bystate$state <-  gsub("-"," ",avg_sp_bystate$state)

avg_tc_sp_bystate <- inner_join(avg_tc_bystate,avg_sp_bystate,"state")
avg_tc_sp_bystate <- as.data.frame(avg_tc_sp_bystate)

tc_sp <- inner_join(salary_potential,tc_4year,"Name")

# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Salary and Tuition Potential by State in the U.S"),
  
  # Sidebar with text box inputs 
  sidebarLayout(
    sidebarPanel(width=3,
                 h4("Select a state", align = "center"),
                 h6("Selection will update the chart",align="center"),
                 selectInput('xcol', label='State', avg_tc_sp_bystate$state),
                 h4("Select a university/college", align = "center"),
                 h6("Selection will update tuition information below the chart",align="center"),
                 selectInput('ycol',label='College',choices = NULL)
     ),
    
    # Show a plot and table of college tuition
    mainPanel(
       plotlyOutput("distPlot"),
       tableOutput("collegename")
    )
    
  )
)
)
