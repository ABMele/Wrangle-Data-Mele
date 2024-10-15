url <- "link.ctice/who_clean.csv"

dest_file <- "rda/murders.rdata"

download.file(url, destfile = dest_file)

url <- "https://github.com/rairizarry/murders/blob/master/rdas/murders.rda"

download.file(url, destfile = dest_file)

?download.file
