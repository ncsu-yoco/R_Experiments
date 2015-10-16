# *** PART I ***

# converts column into pre-mentioned data type
convertData <- function(data, newType) {
  # for each row of data
#  print("Convert Data")
 # print(length(data))
  for(i in 1:length(data)) {
    if (newType[i] == "numeric")
 #     print(sprintf("Converting %d",i))
      data[,i] = as.numeric(data[,i])
  #    print(mode(data[,i]))
  }
  # return the data with modified column type
  return (data)
}

# read transposed data and convert it into proper form with appropriate header
read.table.transposed <- function(filename, dataType) {
  # read data from file
  #filename="student_data_02.csv"
  data = read.table(filename, sep = ",")
  # transpose data
  data = data.frame(t(data), stringsAsFactors = FALSE)
  # extract column headers
  colHeads = data[1,]
  # remove row with column headers
  data = data[-1,]
  # set column headers
  colnames(data) = colHeads
  # convert data into specified statement using custom 'convertData' function
#  print("Converting Data")
  data = convertData(data, dataType)
  return(data)
}

# returns NA is column has different values, used to replace grades if they don't match
remove_dups <- function(cols) {
  cols = cols[!is.na(cols)]
  if ( length(unique(cols)) == 1 )
    return (unique(cols))
  return (NA)
}

# Main function
# read data from first file
sd_01 = read.table("student_data_01.csv", header = T, sep = ",")
# specify column types for second data frame
colTypes = c( rep("character", 3), rep("numeric",  13) )
# read transposed data from second file and covert it into proper format
sd_02 = read.table.transposed("student_data_02.csv", colTypes)
# merge two data sets based on all columns
sd = merge(sd_01, sd_02, by = colnames(sd_01), all = T)
# get list of duplicate elements
dups = sd[duplicated(sd$id),]$id
# for each duplicate student id
for(i in dups) {
  # get all the rows with given student id
  rows = sd[sd$id == i,]
  # remove the rows from the data set
  sd = sd[!(sd$id == i),]
  # generate new row with values matching from all rows, different values are replaced by NA 
  newrow = apply(rows, 2, remove_dups)
  # append newly generated row to data set
  sd = rbind(sd, newrow)
}
# after removing duplicates, columns were converted into character type. Converting them back to numeric type
sd = convertData(sd, colTypes)
# replace column header from 'id' to 'studentId'
names(sd)[4] <- "studentId"
# write output to file
write.table(sd, "student_grades.csv", sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)

# *** END OF PART I ***

# *** PART II ***

# returns appropriate grade based on value of score
map_grades <- function(score) {
  if (is.na(score)) return ("NA")
  else if (as.numeric(score) >= 90) return ("A")
  else if (as.numeric(score) >= 80) return ("B")
  else if (as.numeric(score) >= 70) return ("C")
  else if (as.numeric(score) >= 60) return ("D")
  else if (as.numeric(score) >= 50) return ("E")
  else if (as.numeric(score) >= 40) return ("F")
  else return ("Default")
}

# takes each column and maps replace with appropriate grade based on score
assign_grades <- function(data) {
  for(i in 6:16) {
    data[,i] = sapply(data[,i], map_grades)
  }
  return (data)
}

# find average score for each student and store it in new column
sd[,"average"] = apply(sd[,c(6:16)], 1, mean, na.rm = T)
# find median of score for each student and store it in new column
sd[,"median"] = apply(sd[,c(6:16)], 1, median, na.rm = T)
# calcute mean score for male and female students
male_mean_score = apply(as.matrix(subset(sd, sd$gender == "M")$average), 2, mean)
female_mean_score = apply(as.matrix(subset(sd, sd$gender == "F")$average), 2, mean)
# print the result
print(sprintf("Male students had an average grade of %.3f", male_mean_score))
print(sprintf("Female students had an average grade of %.3f", female_mean_score))
lsd = sd
#lsd = data.frame(t(t(sd)), stringsAsFactors = F)
lsd = assign_grades(lsd)
write.table(lsd, "student_letter_grades.csv", sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)
lsd[,"final_grade"] = sapply(lsd[,17], map_grades)
grade_count = table(lsd[,c("final_grade")])
print(sprintf("Total Count for each letter grade %s: %d",names(grade_count), grade_count))

# *** END OF PART II ***