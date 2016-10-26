writeYml <- function(list, filename){
  list <- yaml::as.yaml(list)
  con <- file(filename, 'w')
  writeLines(list, con)
  close(con)
}

col_string <- function(df, string) {
  grep(string, names(df))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

strReplace <- function(x, list){
  y <- vector()
  for(i in 1:length(x)){
    y <- c(y, stringr::str_replace_all(x[i], list))
  }
  return(y)
}
