library(cdcanthro)
library(tidyverse)
library(data.table)
library(psych)
library(skimr)


# master table starts with ID only - saved as csv file on server ABCD folder
ABCD_all_ID <- read.csv("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/ABCD_all_ID.csv")

# rename this dataframe to start your own dataset
core <- ABCD_all_ID
core <- distinct(core) 

# recursive list of files in release 5.0 folder - all tables
allfiles <- list.files("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/Release 5.0", full.names = T, recursive = T, pattern = ".*.csv")

# function that can retrieve variables from the tables store in ABDC release 5.0 folder on server and copy them to another table, either "core" line 24 above, or merge to existing ABCD table
# define the table and variable you need here, var can be a vector

table_to_find <- "abcd_y_lt.csv"  # the table to find
var_to_find <- "site_id_l"        # the variable to find
mydata <- core                    # the dataframe to merge the new variables

getvar <- function(table_to_find, var_to_find, mydata){
  
  # open the file and get the variable and merge with master by ID and event
  myfile <- list.files("C:/Users/cwyse/OneDrive - Maynooth University/General/Research/ABCD/June Data Package CAW/Release 5.0", full.name=T, recursive = TRUE, pattern=table_to_find)
  myfile <- fread (myfile)
  
  ABCDdata <<- myfile %>%
    select(src_subject_id, eventname, all_of(var_to_find)) %>% #select the variable we want
    right_join(mydata)%>%   #join by ID and event
    distinct()   #remove duplicates %>%
  
  return(ABCDdata) # returns a table called ABCD data with the ID number and merged data
}
