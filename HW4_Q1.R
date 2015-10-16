# converts column into pre-mentioned data type
convertData <- function(data, newType) {
  # for each row of data
  for(i in 1:length(data)) {
    # switch according to corrosponding element in newType vector
    FUN <- switch( newType[i],
                   character = as.character,
                   numeric = as.numeric,
                   factor = as.factor)
    # set column type based on value returned
    data[,i] <- FUN(data[,i])
  }
  # return the data with modified column type
  return (data)
}

# read transposed data and convert it into proper form with appropriate header
read.table.transposed <- function(filename, dataType) {
  # read data from file
  data = read.table(filename, sep = ",")
  # transpose data
  data = data.frame(t(data), stringsAsFactors = F)
  # extract column headers
  colHeads = data[1,]
  # remove row with colum headers
  data = data[-1,]
  # set column headers
  colnames(data) = colHeads
  # convert data into specified statement using custom 'convertData' function
  data = convertData(data, dataType)
  return(data)
}

# Main function
# read data from first file
sd_01 = read.table("student_data_01.csv", header = T, sep = ",")
# specify column types for second data frame
colTypes = c( rep('character', 3), rep('numeric',  13) )
# read transposed data from second file and covert it into proper format
sd_02 = read.table.transposed("student_data_02.csv", colTypes)
# merge two data sets based on all columns
sd = merge(sd_01, sd_02, by = colnames(sd_01), all = TRUE)
# get list of duplicate elements
dups = sd[duplicated(sd$id),]$id
# returns NA is column has diiferent values, used to replace grades if they dont match
remove_dups <- function(cols) {
  if ( length(unique(cols)) == 1 )
    return (unique(cols))
  return (NA)
}
# for each duplicate student id
for(i in dups) {
  # get all the rows with given student id
  rows = sd[sd$id==i,]
  # remove the rows from the data set
  sd = sd[!(sd$id==i),]
  # generate new row with values matching from all rows, differnt values are replaced by NA 
  newrow = apply(rows,2,remove_dups)
  # append newly generated row to datab set
  sd = rbind(sd, newrow)
}
# replace column header from 'id' to 'studentId'
names(sd)[4] <- "studentId"
# write output to file
write.table(sd, "student_grades.csv", sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)