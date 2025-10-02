files <- paste0("Rmd/Datorövning-", 1:4, ".Rmd")
# i <- 1
for(i in 1:4){
  f <- files[i]
  knitr::purl(f, paste0("Skriptversioner/kap-", i, ".R"))
  
  txt <- readLines(paste0("Skriptversioner/kap-", i, ".R"))
  txt <- txt[substr(txt, 1, 5) != "## --"]
  txt[-intersect(which(txt[-1] == txt[-length(txt)]) + 1, which(txt == ""))]
  
  writeLines(txt, paste0("Skriptversioner/kap-", i, ".R"))
}
