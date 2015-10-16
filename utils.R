myprod <- function(x, y)
{
  product <- x * y
  return (product)
}

mymath <- function(x, y)
{
  multiplication <- x * y
  addition <- x + y
  output <- list(sum=addition,
                 product=multiplication)
  return(output)
}

mydiv <- function(numerator=1, denominator=1)
{
  div <- NA
  error <- "OK"
  if( denominator == 0 )
  {
    error <- "Division by zero"
  }
  else
  {
    div <- numerator/denominator
  }
  output <- list(division=div,
                 status=error)
  return(output)
}

matrixRowSum <- function(data)
{
  if( is.matrix(data) == TRUE)
  {
    margin = 1
    sums <- apply(data, margin, sum)
    return (sums)
  }
  else
  {
    print("ERROR: argument must be matrix")
    return (NULL)
  }
}

matrixColSum <- function(data)
{
  if( is.matrix(data) == TRUE)
  {
    margin = 2
    sums <- apply(data, margin, sum)
    return (sums)
  }
  else
  {
    print("ERROR: argument must be matrix")
    return (NULL)
  }
}

printNum <- function(num, msg = "Number is:",some)
{
  return (sprintf("%s %d",msg,num))
}