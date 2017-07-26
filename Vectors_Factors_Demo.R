#Installing and Loading packages
library(ggplot2)                  #Click the Run button above to run the line of code that the cursor is currently on

#set working directory. To do this, go to "Session" in the menu above, then "set working directory", then "Choose directory...". Finally navigate to the folder where you have saved all the files that you will be working with.

#As a calculator
2+2
3*4
10*(5-8)    #order of operations is respected
81/9
10^3

#Incomplete lines
2*
4

#Cursor Up to retrieve previously typed lines

#Functions
log(x=100,base=10)
log(100,10)          #can exclude argument names
log(base=10,x=100)   #having the argument names does allow you to change the order however
log10(100)           #more specific log functions for base 10 and 2

#Nesting calls
sqrt(log10(100))     #output of the inner function is passed directly as output to the outer function

#Default arguments
log(x=100)          #base argument has a default value of Euler's number (i.e., the natural log)

#Help on functions
?log                #help information appears in help tab in bottom right of screen

#Variable assignment
a <- log10(100)
b<-log(81)                     #spacing doesn't matter
c<-                  exp(1000)
d = 5*5                         #can use equal sign, too

#Namespace rules
my_result = 75/5                #underscores
my.result = 4^3                 #decimals :-(
My_Result                       #case sensitivity
.myresult = 100                 #initial position for decimals only
_myresult = 100                 #RStudio is highlighting a syntax error with a red circle with X on the left
88myresult = 100                #no numbers initially
my88result = 100                #but internally ok

#Commenting
log(100,10) #This will return the log of 100 with base 10
log(100,10) This will return the log of 100 with base 10

#Clearing data
rm(a)                          #See environment tab on top right of RStudio; variable "a" disappears
rm(list=ls(all=TRUE))          #Clear ALL variables (you will use this a lot!--best to memorize!)

#Basic data types
is.numeric(3678)               #various kinds of "is...." functions to check whether something is a particular type         
is.character("happy")
is.character("3678")
is.numeric("3678")             #This number is actually a character string according to R because of the quotation marks
is.character('3678')           #Double or single quotes OK

###Vectors
my_vector <- c(1,2,3,7,8,9); my_vector       #you can return a just-assigned variable like this
a <- log10(100)
is.vector(a)                                 #it's a vector, too!
length(my_vector)

#Some basic functions for vectors
min(my_vector)
max(my_vector)
sum(my_vector)

#Arithmetic on vectors
my_vector - 50                              #Single number is subtracted from each element of vector
my_vector * 10
another_vector <- c(2,4,6,8,10,12)
my_vector * another_vector                  #equal length vectors are multiplied elementwise
a_third_vector <- c(2,4,6)
another_vector * a_third_vector                  #when length of 1 is multiple of length of other, shorter is repeated


#Combining vectors
big_cities <- c("Amsterdam","Rotterdam","The Hague"); big_cities       #vectors can contain character strings, too
some_other_cities <- c("Nijmegen","Arnhem")
c(big_cities,some_other_cities)                                        #combine 2 or more vectors
append(big_cities,some_other_cities,2)                                 #insert second vector after the Xth element of first vector
several_dutch_cities <- c(big_cities,some_other_cities)

#Naming elements of vectors
names(big_cities) <- c("capital","main port","seat of government"); big_cities

#Subsetting
several_dutch_cities[1]                     #R indexes from 1!!!!
several_dutch_cities[1:3]                   #subset for a range of values
several_dutch_cities[-5]                    #get everything but the 5th element              
several_dutch_cities[-4:-5]                 #get everything but the 4th through 5th elements
several_dutch_cities[5:1]                   #get the reverse order of the 1st through 5th elements
several_dutch_cities[c(1,4)]                #get the 1st and 4th elements
several_dutch_cities[c(4,1)]
big_cities[2]
big_cities[[2]]                             #for vectors w/ name attributes, you can return just the element's value w/ double brackets
head(several_dutch_cities,2)                #head() and tail() return the specified number of elements from beginning and end of a sequence
tail(several_dutch_cities,2)

#Automatic type casting
numbers_and_cities <- c(my_vector,some_other_cities); numbers_and_cities      #if a vector has at least one character string in it, ALL elements are treated as character strings.
is.character(numbers_and_cities[1])
str(my_vector)                      #summary information about an object
str(numbers_and_cities)

#Useful functions for creating/editing vectors
rep(5,10)                           #repeat some value or values X number of times
rep(some_other_cities,3)            #repeat a vector X number of times
rep(some_other_cities,each=3)       #repeat each element in a vector X number of times

(1:10)                              #get a range of values
seq(10)                             #get a range from 1 to X
my_range_of_values <- seq(4,17,2); my_range_of_values          #get a range from X to Y, at increments of Z
seq(1,3,length.out=11)              #divide a range into X equal sized increments

sample(several_dutch_cities,2)      #get a random sample from a set of values; default is without replacement
sample(seq(10),15,replace=TRUE)

my_vector
my_vector[3] <- 1000                #replace 1 or more elements of a vector
my_vector[4:5] <- 200

sort(my_vector)                     #sort ascending
sort(my_vector, decreasing=TRUE)    #sort descending

#Logical expressions and logical operators
my_vector==200                      #is equal to
my_vector!=200                      #is not equal to
my_vector>200                       #greater than
my_vector<=200                      #less than or equal to
my_vector>1 & my_vector<200         #and operator
my_vector<9 | my_vector>200         #or operator

c(1,9,500) %in% my_vector           #check if elements are in a vector; returns boolean vector corresponding to the first vector

sum(my_vector<9 | my_vector>200)    #Number true (works because it's a Boolean and internally TRUE=1 and FALSE=0)
table(my_vector<9 | my_vector>200)  #Table number of trues and falses

######!!!!!!!!!!!!!!!!!!!!
which(my_vector<9 | my_vector>200)         #Indices of true items; very useful!
######!!!!!!!!!!!!!!!!!!!!

my_vector[my_vector<9 | my_vector>200]     #Can pass logical expression as arg of subsetting; YOU WILL USE THIS A LOT!!!


#Set theory functions

unique(my_vector)         #the "set" of all items (i.e., discards repetitions)

x <- c(1,2,3,4)
y <- c(3,4,5,6)

union(x,y)                #the set of all items from both vectors
intersect(x,y)            #the shared items from the two vectors
setdiff(x,y)              #the items in x that are not in y
setdiff(y,x)              #the items in y that are not in x


###factors

responses_vector <- c("zebra","hippo","lion","lion","zebra","giraffe","hippo","giraffe","hippo","lion","hippo")
responses <- factor(responses_vector); responses                #convert a vector to a factor. A factor records how many different types of things are present in it, and they are the primary data structure used by R to code categorical independent variables

#reveal the numbers underlying the factors (R codes factors underlyingly as numbers)
as.numeric(responses)

#add a new level & changing the status of an element
responses[3] <- "tiger"                                            #doesn't work; level must already exist to assign an element with this value
levels(responses) <- c("giraffe","hippo","lion","zebra","tiger")   #to add new level, must create vector that repeats existing levels and then adds new one at the very end
responses[3] <- "tiger"                                            #now you can assign the value "tiger" to element 3

#remove/change levels
levels(responses) <- c("not scary","scary","scary","not scary","scary")    #for each existing level, give it the name of the new level that you want its members to belong to




###saving vectors and factors

a_numeric_vector <- c(3.5, 5.7, 7.2, 9.1)

cat(a_numeric_vector, file="a_numeric_vector.txt", sep="\n")                           #output a vector to a file; sep='\n'  indicates use newline character to separate the elements of the vector

a_second_numeric_vector <- c(5.5, 7.9, 1.1)

cat(a_second_numeric_vector, file="a_numeric_vector.txt", sep="\n", append=TRUE)       #append to the end of an existing file

cat(as.vector(responses), file="a_factor.txt", sep="\n")                               #output a factor to a file


###Loading vectors (load factors as vectors then convert)

rm(a_numeric_vector)                                                                   #clear the variable

a_numeric_vector <- scan(file.choose(), sep="\n")                                      #load a numeric_vector vector

numeric_commas_as_decimals <- scan(file.choose(), sep="\n", dec=",")                   #load a vector w/ commas as decimal

######NOTE what=character(0)!!!!!!!!
a_character_vector <- scan(file.choose(), sep="\n", what=character(0))                 #load a character vector






