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
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
#read in all the csv files
temp = list.files(pattern="*.csv")
allData2 <- lapply(temp, read.csv)
allData <- do.call(rbind, allData2)

years<-c(1980:2018)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   selectInput("Year", "Select the year to visualize", years, selected = 2018)
  ),
  dashboardBody(
      fluidRow(
      column(2,
             fluidRow(
               box(title = "PieChart of Quality Days", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("pieChart1", height = 580)
               )
             )
        ),
      column(2,
             fluidRow(
               box( title = "CO Pie", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("COPie", height = 250)
               )
             ),
             fluidRow(
               box( title = "NO2 Pie", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("NO2Pie", height = 250)
               )
             ),
             fluidRow(
               box( title = "CO Bar", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("COBar", height = 250)
               )
             ),
             fluidRow(
               box( title = "NO2 Bar", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("NO2Bar", height = 250)
               )
             )
      ),
      column(2,
             fluidRow(
               box( title = "Ozone Pie", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("OzPie", height = 250)
               )
             ),
             fluidRow(
               box( title = "SO2 Pie", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("SO2Pie", height = 250)
               )
             ),
             fluidRow(
               box( title = "Ozone Bar", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("OzBar", height = 250)
               )
             ),
             fluidRow(
               box( title = "SO2 Bar", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("SO2Bar", height = 250)
               )
             )
      ),
      column(2,
             fluidRow(
               box( title = "PM2.5", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("PM2Pie", height = 250)
               )
             ),
             fluidRow(
               box( title = "PM210", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("PM10Pie", height = 250)
               )
             ),
             fluidRow(
               box( title = "PM2.5", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("PM2Bar", height = 250)
               )
             ),
             fluidRow(
               box( title = "PM10", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("PM10Bar", height = 250)
               )
             )
        ),
      column(2,
             fluidRow(
               box(title = "Number of Days Each Pollutant was the Main", solidHeader = TRUE, status = "primary", width = 6,
                   dataTableOutput("tab2", height = 250)
               )
             )
             ),
      column(2,

             fluidRow(
               box(title = "Number of the Different Quality Days", solidHeader = TRUE, status = "primary", width = 6,
                   dataTableOutput("tab1", height = 250)
               )
             ),
             fluidRow(
               box(title = "BarChart of Quality Days", solidHeader = TRUE, status = "primary", width = 6,
                   plotOutput("Bar", height = 250)
               )
             )
             ),
      column(4,
             box(
               title="Sources", width=24,background = "light-blue",
               "For this project I used professor Johnson's starter code to begin writing the application.
               I also used stackoverflow for formatting data, tutorialspoint and the Rshiny tutorial to help plot
               the data, and I used google to generate hex values for the colors. All the data is provided by and can be found on te EPA website. Libraries Used: ggplot2, DT, shiny, and shinydashboard"
             ),
             box(
               title="Findings from the data",width=24,
               "After analyzing and skimming through the differenct decades of the data I noticed that theres a high recording of SO2 from when the data was first being recorded. There maybe be a correlation with that
               and living conditions since there were also increased days of Unhealthy air quality. As time goes on though we see that number start to decrease and instead PM2.5 and PM10 being to rise when initially they were 
               practically 0. This could be from the government not reading it prior in earlier times. Overall, air quality begins to see massive strides of improvement in the early to mid 2000s.
               This may be from humans actually taking the environment more into account and the government forcing companies to properly dispose of waste."
             )
             )
    )       

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  justOneYearReactive <- reactive({subset(allData, allData$Year == input$Year)})
  #Pie Charts to attempt to highlight the main pollutant in each
  output$COPie <- renderPlot({
    all <-justOneYearReactive()
    colors <- c("red4","springgreen1","plum2","lightcyan","lightgoldenrod1","slategray2")
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$'Days.CO'/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("CO",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                                              axis.ticks = element_blank(),
                                                                                                                                                                              panel.grid  = element_blank())+ ggtitle(title)
  })
  output$NO2Pie <- renderPlot({
    all <-justOneYearReactive()
    colors <- c("firebrick2","springgreen4","plum2","lightcyan","lightgoldenrod1","slategray2")
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$'Days.NO2'/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("NO2",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                                              axis.ticks = element_blank(),
                                                                                                                                                                              panel.grid  = element_blank())+ ggtitle(title)
  })
  output$OzPie <- renderPlot({
    all <-justOneYearReactive()
    colors <- c("firebrick2","springgreen1","plum4","lightcyan","lightgoldenrod1","slategray2")
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$'Days.Ozone'/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("Ozone",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                                              axis.ticks = element_blank(),
                                                                                                                                                                              panel.grid  = element_blank())+ ggtitle(title)
  })
  output$SO2Pie <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","lightgoldenrod1","slategray")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$'Days.SO2'/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("SO2",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                                              axis.ticks = element_blank(),
                                                                                                                                                                              panel.grid  = element_blank())+ ggtitle(title)
  })
  output$PM2Pie <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$Days.PM2.5/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("PM2.5",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                                              axis.ticks = element_blank(),
                                                                                                                                                                              panel.grid  = element_blank())+ ggtitle(title)
  })
  output$PM10Pie <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","Blue","lightgoldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    percentage= round((x$Days.PM10/sum(df$DaysInYear))*100,2)
    percentage = paste(percentage,"%",sep="")
    title = paste("PM10",percentage,sep=": ")
    ggplot(df,aes(x="pollutant",y=DaysInYear,fill=Pollutant,main=percentage))+geom_bar(width=1,stat="identity") +coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                 axis.ticks = element_blank(),
                                                                                                                 panel.grid  = element_blank())+ ggtitle(title)
      
  })
  

  #Data Tables displaying the actual numbers for pollutants and day quality
  output$tab1 <- DT::renderDataTable({
    DT::datatable({
      all <-justOneYearReactive()
      x <- subset(all, all$County=="Cook" & all$State=="Illinois")
      df <- data.frame(
        Quality_Day = c("Good Days","Moderate Days","Unhealthhy for Sen. Group Days","Unhealthy Days","Very Unhealthy Days","Hazardous Days"),
        Number_of_Days = c(x$`Good.Days`, x$`Moderate.Days`, x$`Unhealthy.for.Sensitive.Groups.Days`, x$`Unhealthy.Days`,x$`Very.Unhealthy.Days`,x$`Hazardous.Days`))},
         options = list(searching = FALSE,lengthChange = FALSE,rownames= FALSE)
    )
      
    }  
    )
  output$tab2 <- DT::renderDataTable({
    DT::datatable({
      all <-justOneYearReactive()
      x <- subset(all, all$County=="Cook" & all$State=="Illinois")
      df <- data.frame(
        Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
        DaysInYear= c(x$'Days.CO',x$'Days.NO2',x$'Days.Ozone',x$'Days.SO2',x$'Days.PM2.5',x$'Days.PM10'))},
        options = list(searching = FALSE,lengthChange = FALSE,rownames= FALSE)
      )
  }   
  )  
  #Pie Chart for Day Quality
  output$pieChart1 <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    
    colors<- c("#003f5c","#444e86","#955196","#dd5182","#ff6e54","#ffa600")
    df<- data.frame(
      Quality_Day = c("Good Days","Hazardous Days","Moderate Days","Very Unhealthy Days","Unhealthhy for Sen. Group Days","Unhealthy Days"),
      Days = c(x$`Good.Days`,x$`Hazardous.Days`,x$`Moderate.Days`,x$`Very.Unhealthy.Days`,x$`Unhealthy.for.Sensitive.Groups.Days`,x$`Unhealthy.Days`)
    )
    df$DayType <- paste(df$Quality_Day,paste(round(((df$Days/sum(df$Days))*100),2),"%"),sep="-")
    df$Label <- paste(round(((df$Days/sum(df$Days))*100),2),"%",sep="")
    head(df)
    ggplot(df, aes(x="Quality",y=Days,fill=DayType)) + geom_bar(width=1,stat="identity") + coord_polar("y") + scale_fill_manual(values=colors) +  theme(axis.text = element_blank(),
                                                                                                                                                axis.ticks = element_blank(),
                                                                                                                                                panel.grid  = element_blank()) +
      geom_text(aes(label=Label),position=position_stack(vjust=.5))
    
  })
  #Bar chart for the Day Quality
  output$Bar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors<- c("#003f5c","#444e86","#955196","#dd5182","#ff6e54","#ffa600")
    df <- data.frame(
      Quality_Day = c("Good Days","Moderate Days","Unhealthhy for Sen. Group Days","Unhealthy Days","Very Unhealthy Days","Hazardous Days"),
      Number_of_Days = c(x$`Good.Days`,x$`Moderate.Days`,x$`Unhealthy.for.Sensitive.Groups.Days`,x$`Unhealthy.Days`,x$`Very.Unhealthy.Days`,x$`Hazardous.Days`)
    )
    ggplot(df,aes(x=Quality_Day,y=Number_of_Days),fill=Quality_Day) + geom_bar(stat="identity")+ scale_fill_manual(values=c("#003f5c","#444e86","#955196","#dd5182","#ff6e54","#ffa600"))+
      theme_minimal()
  })
  #Bar charts for each pollutant
  output$COBar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('y','n','n','n','n','n')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
  output$NO2Bar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('n','y','n','n','n','n')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
  output$OzBar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('n','n','y','n','n','n')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
  output$SO2Bar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('n','n','n','y','n','n')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
  output$PM10Bar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('n','n','n','n','n','y')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
  output$PM2Bar <- renderPlot({
    all <-justOneYearReactive()
    x <- subset(all, all$County=="Cook" & all$State=="Illinois")
    colors <- c("firebrick2","springgreen1","plum2","lightcyan","goldenrod1","slategray2")
    df<- data.frame(
      Pollutant = c("CO","NO2","Ozone","SO2","PM2.5","PM10"),
      DaysInYear= c(x$Days.CO,x$Days.NO2,x$Days.Ozone,x$Days.SO2,x$Days.PM2.5,x$Days.PM10)
    )
    df$newCOl <- c('n','n','n','n','y','n')
    ggplot(df, aes(x=Pollutant,y=DaysInYear,fill=newCOl)) + geom_bar(stat="identity")+ scale_fill_manual(values=c('y'='red','n'="gray"),guide=FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

