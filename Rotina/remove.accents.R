remove.accents <- function(x){
  string <- gsub("`|\\'", "", iconv(x, to="ASCII//TRANSLIT")) 
  string <- gsub('~','',string)
  return(string)    
}