rm(list=ls(all=TRUE))


#Loading a dataframe "reading_times.csv" (use csv [comma-separated values] file format!)
read_times <- read.table(file.choose(), header=TRUE, sep=",", quote="", comment.char="", fill=TRUE)

#header: whether column names are present in the spreadsheet
#sep: what character the particular csv file uses to separate cells
#quote: the characters used to delimit strings in a spreadsheet. (Leave disabled)
#comment.char: what character is used to denote comments in spreadsheet. (Leave disabled)
#fill: if true, rows with missing values will have NAs inserted in those cells


str(read_times)                #Check that the dataframe has been loaded properly

head(read_times)               #Get the first several rows; (row names are numbers)

tail(read_times)               #Get the last several rows

is.data.frame(read_times)

dim(read_times)                #Give the size in rows and columns as a vector


#selecting a column
read_times$PARTICIPANT

#Change a column to a factor
read_times$PARTICIPANT <- factor(read_times$PARTICIPANT)

#Subsetting   (never print the whole dataframe to the screen!)
read_times[1,]                                            #select a row
read_times[,5]                                            #select a column
read_times[1,5]                                           #select an element

subset(read_times, AGE>30 & GENRE=="tweet")
subset(read_times, AGE<30 | GENRE=="poem")
subset(read_times, PARTICIPANT==3)

subset(read_times, PARTICIPANT==3)[,4]                    #subset then get a column

indices <- which(read_times$GENRE=="poem"); indices       #get the index of rows in which a particular column contains a value

read_times[indices,]                                      #you can then use these indices to subset using bracketing

#Editing values
read_times[1,5] <- 5000; read_times[1,]

fix(read_times)                              #launch a spreadsheet editor GUI from RStudio to edit directly

r <- read_times                              #I prefer short dataframe names
r$READING_TIME

#Test for missing data

r[1,5] <- NA; r[1,]                     #Change an element in row 1 to NA
complete.cases(r)                       #indicates which rows have complete data


#Sorting
r$AGE
order(r$AGE)                            #note that this prints the row indices when ordered according to the selected columns
r[order(r$AGE),]                        #sort ascending by age (notice comma!)
r[order(r$AGE,-r$READING_TIME),]        #sort ascending by age, descending by reading time

r <- r[order(r$AGE,-r$READING_TIME),]

#Renaming/renumbering rows
row.names(r) <- seq(12)                 #you can assign any vector of values as row names--just make sure it's the right length!

#Adding a column
r$NEW_COLUMN <- seq(12)

#Removing a column
r$NEW_COLUMN <- NULL

#Substituting values in a vector (or column)
demo_vector <- c("some text; separated by a semi-colon","some other text; separated by a semi-colon")
gsub(";", ":", demo_vector)                       #subsitute occurrences of the first argument with the second argument in the data structure indicated in the third argument

#Saving a dataframe
write.table(read_times, "reading_times_modified.csv", quote=FALSE, sep=",", row.names=FALSE)
