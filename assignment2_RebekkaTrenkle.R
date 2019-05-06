

# define the function called frequent_words using the R function "function"
# https://www.rdocumentation.org/packages/base/versions/3.3.0/topics/function
# function input: text: The string in which the frequent pattern is searched; 
# k: the length of the pattern we search for
# the function frequent_words returns the sequence of the most frequent patterns
# in the input string "text"

frequent_words = function(text,k){
  frequent_patterns <- {}
  # creates an empty list named frequent_patterns
  count <-{}
  # creates an empty list named count
  for (i in 0:(nchar(text) -k)){
    # The for-loop iterates from zero to the length of the string 
    # called "text" minus the length of the pattern k  
    # the function nchar determines the length of its argument 
    #  (a character vector, here: "text)  
    # https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/nchar
  pattern <- substr(text, i + 1, i + k)
  # creates the variable called pattern that holds the current pattern as a string
  # the function substr extracts a substring from a character vector
  # (here "text") and assigns it to pattern. It takes the substring from text, 
  # starting at the (i+1)th character (according to the current iteration of the loop)
  # and ending at the (i+k)th character, thus creating a new string with k characters
  # that represent the current pattern
  # https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
  count[i + 1] <-pattern_count(text, pattern)
  # the list count is altered: the (i+1)th entry (i according to the loop) of
  # count is replaced by the output of the function "pattern_count" (which is
  # defined below) after it was executed with the inputs "text" and "pattern"
  }
  max_count = max(count)
  # create the variable max_count that holds the maximum value of count
  # using the R function max that returns the maximal value of a vector
  # https://www.rdocumentation.org/packages/whitebox/versions/0.1.0/topics/max
  for (i in 0:(nchar(text) -k)) {
    # for-loop that iterates from zero to the length of text minus k
    if (count[i + 1] == max_count)
      # if statement: if the entry of count at the position i+1 is the 
      # maximal count do the following:
      frequent_patterns <- append(frequent_patterns, substr(text, i + 1, i + k))
    # add the sequence of the current pattern (created by the substring beginning
    # at position i+1 and ending at position i+k of the string "text") to the list
    # called frequent_patterns (which is thereby modified) using the function append
    # https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/append
  }
  return(unique(frequent_patterns))
  # return statement of the function frequent_words:
  # the function executed on the input parameters "text" and "k" gives as output
  # the most frequent patterns of the length k within the string "text"
  # the sequences of the most frequent patterns are stored in the list frequent_patterns
  # the function unique returns the entries of the list once, avoiding duplicates
  # https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/unique
}


# the function pattern_count searches for the number of times a given pattern
# (stored in the variable "pattern") occurs in the string "text"

pattern_count <-function(text, pattern) {
  # creates the function pattern_count that takes the 
  # input parameters "text" and "pattern"
  count <-0
  # creates the variable called count and assigns it to zero
  pattern_length <-nchar(pattern)
  # creates the variable pattern_length and assigns it the length of string "pattern"
  # using the nchar function (link see above)
  for (i in 0:(nchar(text) -pattern_length)) {
    # for-loop that iterates from zero to the length of the string "text" minus
    # the length of the pattern pattern_length
    if (substr(rep(text), i + 1, i + pattern_length) == pattern)
      # if statement: if the substring created (by the function substr) 
      # from a replicate of the string "text" (using the function rep)
      # is equal to the pattern that was used as input for the function
      # increase count by 1
      # https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/rep
      count <-count + 1}
  return(count)
  # returns the value of count
}

# execute the function frequent_words with the input parameters of the 
# sample dataset from Roaslind
text = 'ACGTTGCATGTCGCATGATGCATGAGAGCT'
k = 4
frequent_words(text,k)

# execute the function frequent_words with the input parameters of the 
# extra dataset from Roaslind

text_extra = 'CGGAAGCGAGATTCGCGTGGCGTGATTCCGGCGGGCGTGGAGAAG
CGAGATTCATTCAAGCCGGGAGGCGTGGCGTGGCGTGGCGTGCGGATTCAAGCCGGCGG
GCGTGATTCGAGCGGCGGATTCGAGATTCCGGGCGTGCGGGCGTGAAGCGCGTGGAGGA
GGCGTGGCGTGCGGGAGGAGAAGCGAGAAGCCGGATTCAAGCAAGCATTCCGGCGGGAG
ATTCGCGTGGAGGCGTGGAGGCGTGGAGGCGTGCGGCGGGAGATTCAAGCCGGATTCGC
GTGGAGAAGCGAGAAGCGCGTGCGGAAGCGAGGAGGAGAAGCATTCGCGTGATTCCGGG
AGATTCAAGCATTCGCGTGCGGCGGGAGATTCAAGCGAGGAGGCGTGAAGCAAGCAAGC
AAGCGCGTGGCGTGCGGCGGGAGAAGCAAGCGCGTGATTCGAGCGGGCGTGCGGAAGCGAGCGG'
k_extra = 12
frequent_words(text_extra, k_extra)

