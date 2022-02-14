# ------------------------------
# A simple function to remove leading and trailing whitespace
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
# by Bernhard Kausler
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
