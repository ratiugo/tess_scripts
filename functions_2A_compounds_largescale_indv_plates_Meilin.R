

#This script includes functions that are to process/analyse cleaned data
#First load required libraries:

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)
library(here)
library(stringr)
library(plotly)
library(ggrepel)


#' load all the lengthened dataframes that have been cleaned (i.e. are in long
#' format) by the lengthened list, then we are going to process all of these together
#' Call the grouped dataframe we're creating by the data or some other unique id
#' Alternatively, if you'd like individual plots please see code in 'individual plate' script
#' You only need the last line of code in the section below if you have mismatched 
#' cases in your dataframes, which hopefully you shouldn't have!

filter_df <- function(plate_df, hour) {
  plate_df %>%
    filter(time == {{hour}}) %>%
    group_by(mutant, time) %>%
    mutate(mean_OD = mean(value),
           sd_OD = sd(value)) %>%
    rowid_to_column() 
}

#lapply to filter each df
filter_df_xxdatehere_list <- lapply(long_df_list_xxdatehere, function(x) filter_df(x, 24))


# a function for three_sd creation
three_sd_calc <- function(filtered_plate) {
  sd_value <- filtered_plate$sd_OD[1]
  mean_value <- filtered_plate$mean_OD[1]
  return(mean_value + 3*sd_value)
}


#create a wt_filter list

wt_filter_list_xxdatehere <- lapply(filter_df_xxdatehere_list, function(x) filter(x, mutant == "WT"))

#if you can get list to work for plotting, use this:
mean_list_xxdatehere <- lapply(wt_filter_list_xxdatehere, function(x) mean(x$mean_OD))

#if you can get list to work for plotting, use this:
three_sd_list <- lapply(wt_filter_list_xxdatehere, function(x) three_sd_calc(x))



#run mean without a function - use (index_2)-2 for y position
#mean_3021 <- mean(filter_3021$mean_OD)


#now a function to plot by plate
#for this function: specify the plate you're plotting (ideally the filtered df)
#' specify the three_sd variable you've created
#' the mean variable you've created
#' the title of your plot
#' the label for group 1 (grouped alphabetically by mutant),
#' the label for group 2
#' The function for individual plates is a little different than grouped 
#' There are labelled points, currently only points that meet the condition of 
#' being a 'wildtype' mutant and having a value greater than significance cut off
#' are labelled, feel free to change/add to these parameters within this function

plate_plotting <- function(plate_df, three_sd_id, mean_id, title,
                           group1_label, group2_label){
  #below the aes will plot the rowid assigned to each row in your dataframe against the value
  #colour is grouping by 'mutant' column, change to appropriate condition if necessary
  ggplot({{plate_df}}, aes(rowid, value, colour = mutant)) +
    #geom_point will create a scatter plot
    geom_point() + 
    #this will create a horizontal line at the 3sd cut off, remove 'linetype' argument
    #for it to be a solid line (default)
    geom_hline(yintercept = {{three_sd_id}}, col = "red", linetype = "dashed") +
    #this will create the same at the mean, same for default solid line as above
    geom_hline(yintercept = {{mean_id}}, linetype = "dashed") +
    #the below lines are a messy way to specify a large type size and a font I like
    #change font size to 16 for something that looks better at a smaller size (or play around)
    #and change font to something you like in your devices fonts
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme_minimal(base_size = 30) +
    theme(text = element_text(size = 30, family = "Canela")) +
    #the below adds a label '3sigma (the actual symbol) to the plot at the line
    annotate("text", x = 390, y = {{three_sd_id}} + 0.1, 
             label = expression( ~ 3*sigma), colour = "red") +
    #the below does the same but for mean
    annotate("text", x = 390, y = {{mean_id}} + 0.05,
             label = expression( ~ mu))  +
    #now to label the points, just change the conditions depending on what you want 
    #(don't change the legend/nudge/padding without saving a different version 1st
    geom_text_repel(data = subset({{plate_df}}, value > {{three_sd_id}} & mutant != "MT"),
                    aes(label = Well),
                    show.legend = F,
                    box.padding = 0.5,
                    nudge_y = 0.2) +
    #this justifies plot title to centre
    theme(plot.title = element_text(hjust = 0.5)) +
    #this anchors x and y axis to 0, and lets the upper limits be specified by the data
    #change from NA to a value if you want consistent upper limits
    ylim(c(0,NA)) +
    xlim(c(0, NA)) +
    #this changes legend name to legend, specifies some colours, and puts in the labels
    #you define when calling the function
    #change the colours as you see fit, just make sure to keep them as strings!
    scale_color_manual(name = "Legend", 
                       labels = c({{group1_label}}, {{group2_label}}), 
                       values = c("#66BB6A", "#FFA000",  "#5C6BC0")) +
    #now the title, and x/y axis labels, again change the labels here as you see fit
    ggtitle({{title}}) +
    labs( x = "Well", y = "Growth Reading")
}


#'I cannot for the life of me get a list to work with the plotting function
#'i have tried including dev.off() in the function but this doesn't help, so below is 
#'some theoretical code to make this happen but it's commented out for now
#'Until that's functional, you will have to manually change each dataframe in the 
#'function, plate parameter for each run sadly (use find & replace)



# plates_list <- lapply(filter_df_xxdatehere_list, function(x) 
#   plate_plotting(x, three_sd_list, mean_list_xxdatehere,
#                  "positive control","wildtype"))


#change to each dataframe, running dev.off() will save to whichever folder the 
#project is located in
plate_plotting(filter_3021, threesd_3021, mean_3021,
               "Time vs. OD 600 for 3021 4 hrs",
               "positive control", "wildtype")

dev.off()

#' now a function to isolate significant hits from each plate 
#' then we'll export it to a csv
#' generally, it's best to run this for each plate but immediately below is
#' some code set up to run this per run (~30 plates) 
#' for individual plates please refer to appropriate script
isolate_hits <- function(filter_plate_list, three_sd_list_) {
  filter_plate_list %>%
    filter(value > {{three_sd_list}},
           #remove this next line if you only have one condition in the dataframe
           mutant != "MT")
}

#' running the function
hits_xxdatehere_24hrs_list <- lapply(filter_df_xxdatehere_list, function(x) 
                                     isolate_hits(x, three_sd_list_xxdatehere))

#'cleaning the data a little, it's arranged by just value 
hits_xxdatehere_24hrs_platelist <- hits_xxdatehere_24hrs_list %>%
  arrange(desc(value)) %>%
  #this line doesn't usually work, but worth a try
  select(-c(mean_OD, sd_OD, rowid))


top_hits_unique <- function(top_hits){
  top_hits %>%
    unique()
}

#create a unique dataframe of hits (no repeats)
unique_hits <- lapply(hits_xxdatehere_24hrs_platelist, function(x) unique(x)) 

#' make sure unique dataframe is actually unique and only contains large screening plates
#' also make sure there's only the condition we're interested in
unique_hits <- bind_rows(unique_hits) %>%
  arrange((plate_ID)) %>%
  relocate("mutant", .before = "value") %>%
  unite("mutant", "mutant":"Mutant", na.rm = TRUE, remove = TRUE) %>%
  filter(mutant == "WT") %>%
  unique() %>%
  filter(plate_ID > 2000)


#' now write to a csv file with each plate, hits, sadly there's no easy way to 
#' get the list to also have plate_ID apply here in the name
#' for now, exporting hits as grouped list is best, but if you want
#' to use the below function, do feel free just don't forget to replace with the date/id!
lapply(hits_xxdatehere_24hrs_platelist, function(x) write_csv(x, "xxdatehere_output",
                                                              "top_hits_24hr_xxdatehere.csv"))


#' can also write csv with grouped hits after filtering for unique values
#' don't forget to create a folder with the date!
write_csv(unique_hits, here::here("xxdatehere_output", "unique_xxdatehere_hits.csv"))


#' You can also use the above code to save a filtered dataset, for double checking your filtering
#' went well, but you will have to also open it to see the plate ID, though since
#' it's just for checking that shouldn't be a huge issue!