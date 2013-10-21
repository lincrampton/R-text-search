''' assignment for Dr. Peng's Computing for Data Analysis on Coursera.  Designed to search a provided text file (homicides.txt) and (assuming that the input is non-null) return the quantity of homicide victims of an input age.  Lots of ways to accommplish this, so have functions agecount1, agecount2 and agecount3'''

agecount1 <- function(age = NULL) {
  #exclude invalid input
  if (is.null(age)) stop('need to input an age')
  
  #read in the file
  homicides <- readlines("homicides.txt")
  
  #select matching expressions
  r <- regexec('([0-9]+)[ \t]*year[s]*[ \t]*old', homicides)
	m <- regmatches(lincides, r)
	age_list <- (sapply(m, function(x) x[2]))
	age_list <- na.omit(sapply(m, function(x) x[2]))

	# count the number of matches
	num_matches = 0
	for (each_age in age_list)	{
		if (each_age == age )	num_matches = num_matches + 1
	}
	return(num_matches)
}

agecount2 <- function(age = NULL) {
  #exclude invalid input
  if (is.null(age)) stop('need to input an age')
  
  #read in the file
  homicides <- readlines("homicides.txt")
  
  #select matching expressions
  pat1 = sprintf("\\s+%d\\s+year\\s+old", age)  #remember not to exclude '1 year old' entries
	res1 = grep(pat1, homicides,ignore.case = T)
	pat2 = sprintf("\\s+%d\\s+years\\s+old", age)
	res2 = grep(pat2, homicides,ignore.case = T)
	
  #count the number of matches
  return(length(res2) + length(res1))
}

	    
agecount3 <- function(age = NULL) {
  #exclude invalid input
  if (is.null(age)) stop('need to input an age')
  
  #read in the file
  homicides <- readlines("homicides.txt")
  
  #select matching expressions
	if (age == '1')	{
		new_query <- paste0(age, " year old")
	} else	{
		new_query <- paste0(age, " years old")
	}
	    
	# count and return the number of matches
	return(length(grep(new_query, homicides, ignore.case = TRUE)))
}


  
  
