#Load all libraries needed
if(!require('redcapAPI') && !require('kableExtra') && !require('knitr') && !require('magick') && !require('tidyverse') && !require('plotly') && !require("processx") && !require('webshot') && !require('ggraph') && !require('grid')){
  install.packages(redcapAPI,tidyverse,plotly,processx,webshot,knitr,ggraph,grid)
}
library(redcapAPI)
library(tidyverse)
library(plotly)
library(data.tree)
library(tibble)
library(processx)
library(webshot)
library(knitr)
library(kableExtra)
library(igraph)
library(ggraph)
library(grid)
library(gridExtra)
library(magick)


#Exports record and organizes so that it can be merged with coding_sheet.
export_record <- function(rtoken, record){
  rcon <- redcapConnection( url= "https://rc.bcchr.ca/redcap/api/", token=rtoken)
  # Exports single record ID, then transposes
  df <- exportRecords(rcon, records = record, dates = FALSE, checkboxLabels = FALSE )
  transposed <- t(df)
  #Changes name of first column to Input
  colnames(transposed) <- c('Input')
  #Extract row names of matrix df
  rn <- row.names(transposed)
  #Changes transposed matrix to data.frame and makes row names into column 1
  df_2 <- transposed %>% as.data.frame %>% cbind('Field Name' =rn,.)
  row.names(df_2)<- NULL
  return(df_2)
}

#Reads the coding_sheet and makes into a dataframe
read_coding <- function(coding_sheet){
  cs <- read.csv(coding_sheet)
  names(cs)[1] <- 'Field Name'
  return(cs)
}

#Exports record from redcap, organizes data, merge with coding_sheet
export_merge <- function(record, coding_sheet, token){
  df <- export_record(token, record)
  cs <- read_coding(coding_sheet)
  #Merges both dataframes to create a single dataframe for all the analysis
  merged <- dplyr::left_join(df, cs, by='Field Name')
  return(merged)
}



#For each subcateogry, calculates max score and hospital score. Output is a dataframe.
sum_each_group <- function(df){
  #Remove any questions without a Grouping, if Max score is NA (ie. input is text), or if Score is NA (ie. left unfilled in survey)
  intermediate <- df[!(is.na(df$Grouping) | df$Grouping=="" | is.na(df$Max) | is.na(df$Score)), ]
  #Make Sum column of type 'double'
  intermediate$Score <- as.double(intermediate$Score)
  #Make Max column of type 'double'
  intermediate$Max <- as.double(intermediate$Max)
  #Sum each subcategory, skipping the NA values (these are presumed text answers)
  summed <- intermediate %>% group_by(Grouping) %>% summarise(Max = sum(Max), Sum = sum(Score))
  #Create new column of percent
  summed <- mutate(summed, Percent = Sum/Max)
  return(summed)
}

#Adds pathstring to hierarchy
create_pathstring <- function(hierarchy){
  tree <- read_csv(hierarchy)
  names(tree)[ncol(tree)] <- 'Grouping'
  #Create pathString needed to make tree
  tree <- tree %>% tidyr::unite('pathString',1:(ncol(tree)) , na.rm = TRUE, remove = FALSE, sep = '/')
  tree$pathString <- paste('Hospital', tree$pathString, sep ='/')
  return(tree)
}

#Makes the dataframe for the tree
make_tree_df <- function(hierarchy, df){
  summed <- sum_each_group(df)
  treedf<- create_pathstring(hierarchy)
  tree_with_sum <- dplyr::left_join(treedf,summed, by='Grouping')
  tree_with_sum <- tree_with_sum %>% replace(., is.na(.), 0)
  return(tree_with_sum)
}

#Makes the data tree
create_tree <- function(df_tree){
  df_tree$Grouping <-as.factor(df_tree$Grouping)
  hospital <- as.Node(df_tree)
  hospital$Do(function(x) x$Max <- Aggregate(x, "Max", sum))
  hospital$Do(function(x) x$Sum <- Aggregate(x, "Sum", sum))
  return(hospital)
}


#Make plotly bar graph for a Grouping with title (because when subplots are made in plotly they take away the title of the graph)
make_graph <- function(string, groups_df, colour){
  a <- groups_df %>% dplyr::filter(from == string) %>% mutate(Percent = Sum/Max)
  p <- plot_ly(x= as.vector(a$Percent), y= as.vector(a$to), color=I(colour), orientation = 'h', type = "bar", name = string) %>%
    layout(
      title = a[1,1],
      xaxis = list(range=c(0,1), tickfont = list(family = 'Arial, sans-serif', size = 10)),
      yaxis = list(tickfont = list(family = 'Arial, sans-serif', size = 10)),
      annotations = list(x = 0.5 , y = 1.07, text = string, showarrow = F, 
                         xref='paper', yref='paper', xanchor='center')
    )
  return(p)
}

#Make plotly bar graph for a Grouping with out a title
make_graph_no_title <- function(string, groups_df, colour){
  a <- groups_df %>% dplyr::filter(from == string) %>% mutate(Percent = Sum/Max)
  p <- plot_ly(x= as.vector(a$Percent), y= as.vector(a$to), color=I(colour), orientation = 'h', type = "bar", name = string) %>%
    layout(
      title = a[1,1],
      xaxis = list(range=c(0,1), tickfont = list(family = 'Arial, sans-serif', size = 10)),
      yaxis = list(tickfont = list(family = 'Arial, sans-serif', size = 10))
    )
  return(p)
}

#Make RADAR graphs
make_radar_graph <- function(string, groups_df){
  a<-groups_df %>% dplyr::filter(from == string) %>% dplyr:: filter(Max != 0) %>% mutate(Percent = Sum/Max)
  p <- plot_ly(
    theta= as.vector(a$to), 
    r= as.vector(a$Percent), 
    type = "scatterpolar", 
    fill = 'toself',
    mode = 'markers'
  ) %>%
    layout(
      font = list(family = 'sans serif', size = 20),
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1)
        )
      ),
      showlegend = F
    )
  return(p)
}

#Make demographics chart
large_demographic_chart <- function(df){
  y <- c(toString(df$Input[df$'Field Name' == 'name_facility']))
  neonate <- c(as.numeric(df$Input[df$'Field Name' == 'age_range']))
  baby <- c(as.numeric(df$Input[df$'Field Name' == 'age_range2']))
  toddler <- c(as.numeric(df$Input[df$'Field Name' == 'age_range3']))
  child <- c(as.numeric(df$Input[df$'Field Name' == 'age_range4']))
  data <- data.frame(y, neonate, baby, toddler, child)
  
  
  p <- plot_ly(data, x = neonate, y= ~y, type = 'bar', orientation = 'h', name = '0-28 days', marker = list(color = 'rgba(38, 24, 74, 0.8)')) %>%
    add_trace(x = ~baby, name='28-364 days', marker = list(color = 'rgba(71, 58, 131, 0.8)'))%>%
    add_trace(x = ~toddler, name='1-4 years', marker = list(color = 'rgba(122, 120, 168, 0.8)'))%>%
    add_trace(x = ~child, name='4+ years', marker = list(color = 'rgba(164, 163, 204, 0.85)'))%>%
    layout(barmode = 'stack',
           bargap = 0.7,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.10, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        zeroline = FALSE),
           showlegend=TRUE)%>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate / 2, y = y,
                    text = data[,'neonate'],
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate + baby / 2, y = y,
                    text = data[,'baby'],
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate + baby + toddler / 2, y = y,
                    text = data[,'toddler'],
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate + baby + toddler + child / 2, y = y,
                    text = data[,'child'],
                    font = list(family = 'Arial', size = 12,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE)
  return (p)
}

#Make a smaller graph demographics
small_demographic_chart <- function(df){
  y <- c(toString(df$Input[df$'Field Name' == 'name_facility']))
  neonate <- c(as.numeric(df$Input[df$'Field Name' == 'age_range']))
  child <- c(as.numeric(df$Input[df$'Field Name' == 'above28_days']))
  data <- data.frame(y, neonate, child)
  
  
  p <- plot_ly(data, x = neonate, y= ~y, type = 'bar', orientation = 'h', name = '0-28 days', marker = list(color = 'rgba(38, 24, 74, 0.8)')) %>%
    add_trace(x = ~child, name='>28 days', marker = list(color = 'rgba(164, 163, 204, 0.85)'))%>%
    layout(barmode = 'stack',
           bargap = 0.7,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.10, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        zeroline = FALSE),
           showlegend=TRUE)%>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate / 2, y = y,
                    text = data[,'neonate'],
                    font = list(family = 'Arial', size = 11,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE) %>%
    add_annotations(xref = 'x', yref = 'y',
                    x = neonate + child / 2, y = y,
                    text = data[,'child'],
                    font = list(family = 'Arial', size = 11,
                                color = 'rgb(248, 248, 255)'),
                    showarrow = FALSE)
  return (p)
}


#Make good table with subcategories (for grouping that has further subcategories)
good_subgroup_table <- function(string){
  c <- groups_df %>% dplyr:: filter(from == string) %>% dplyr::filter(Max != 0)
  c <- c$to 
  table <- tibble()
  for (i in c){
    a <- df %>% dplyr:: filter(Grouping == i) %>% dplyr::filter(Max != 0)
    t <- a %>% dplyr:: filter(Max == Score) %>% dplyr :: select (Grouping, Question)
    t <- mutate(t, Comments='')
    table <- dplyr:: bind_rows(table,t)
  }
  
  table %>% kable('latex', booktabs = F, longtable = T) %>%
    kable_styling(full_width = T, latex_options = c('hold'), font_size = 8) %>%
    column_spec(column = 1, width = '3cm') %>%
    column_spec(column = 2, width = '7cm') %>%
    row_spec (0, bold=T) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle")
  
  return(table)
  
}

#Make bad table with subcategories
bad_subgroup_table <- function(string){
  c <- groups_df %>% dplyr:: filter(from == string) %>% dplyr::filter(Max != 0)
  c <- c$to 
  table <- tibble()
  for (i in c){
    a <- df %>% dplyr:: filter(Grouping == i) %>% dplyr::filter(Max != 0)
    t <- a %>% dplyr:: filter(Max > Score) %>% dplyr :: select (Grouping, Question, Input)
    t <- mutate(t, Comments='')
    table <- dplyr:: bind_rows(table,t)
  }
  
  table %>% kable('latex', booktabs = F, longtable = T) %>%
    kable_styling(full_width = T, latex_options = c('hold'), font_size = 8) %>%
    column_spec(column = 1, width = '3cm') %>%
    column_spec(column = 2, width = '7cm') %>%
    row_spec (0, bold=T) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle")
  
  return(table)
}

#Table for items checked yes 
good_table <- function(string){
  a <- df %>% dplyr:: filter(Grouping == string) %>% dplyr::filter(Max != 0)
  t <- a %>% dplyr:: filter(Max == Score) %>% dplyr :: select (Question)
  t <- mutate(t, Comments='')
  t <- t %>% kable('latex', booktabs = F, longtable = T) %>%
    kable_styling(full_width = T, latex_options = c('hold'), font_size = 8) %>%
    column_spec(column = 1, width = '8cm') %>%
    column_spec(column = 2, width = '4cm') %>%
    row_spec (0, bold=T)
  return(t)
}

#Table for items not completed
bad_table <- function(string){
  b <- df %>% dplyr:: filter(Grouping == string) %>% dplyr::filter(Max != 0)
  r <- b %>% dplyr:: filter(Max > Score) %>% dplyr :: select (Question, Input)
  r <- mutate(r, Comments='')
  r <- r %>% kable('latex', booktabs = F, longtable = T) %>%
    kable_styling(full_width = T, latex_options = c('hold'), font_size = 8) %>%
    column_spec(column = 1, width = '4cm') %>%
    column_spec(column = 2, width = '4cm') %>%
    row_spec (0, bold=T)
  return(r)
}


