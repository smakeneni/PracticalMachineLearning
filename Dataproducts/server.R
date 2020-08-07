#Loading library files
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

# Define server logic required to plot and update the table
shinyServer(function(input, output,session) {
  
#dynamically updating the second input
observeEvent(input$xcol,{updateSelectInput(session,"ycol",
                choices = tc_4year[tc_4year$state==input$xcol,"Name"])})
  
#Plotting pay vs tuition by state    
  output$distPlot <- renderPlotly({
   tc_esp <- tc_sp %>% filter(state==input$xcol) %>% 
      ggplot(aes(Name=Name))+geom_segment(aes(x=Outstate,xend=Outstate,y=early_career_pay,
          yend=mid_career_pay),color="gray")+
        geom_point(aes(x=Outstate,y=early_career_pay,color="Early career pay"),size=3)+
      geom_point(aes(x=Outstate,mid_career_pay,color="Mid career pay"),size=3)+theme_classic()+
      xlab("Out of state tuition")+ylab("Pay")+ 
      scale_color_manual(values=c(rgb(0.2,0.7,0.1,0.5),rgb(0.7,0.2,0.1,0.5)))+
      theme(legend.title=element_blank())
      
    ggplotly(tc_esp)
    
  })

#outputting the table    
output$collegename <- renderTable({
    test<-tc_4year %>% filter(Name==input$ycol) %>% select(Name,Roomandboard,Instate,Outstate)
    test
  })

  
})
