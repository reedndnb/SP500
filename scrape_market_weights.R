# Using readHTMLTable - this fails to parse the ticker, which is an attribute of input tags
# library(XML)
# tables <- readHTMLTable("http://slickcharts.com/sp500", header=FALSE, as.data.frame=TRUE)
# df <- tables[[2]]
# write.csv(df, file="s&p500_weights.csv", row.names=FALSE)

extractRowInfo <- function(row) {
  columns <- xml_nodes(row, xpath="td");
  td <- columns[1]

  # url
  val <- xml_nodes(td, xpath="a/@href")[[1]];
  url <- getElement(val[1], "href");
  
  # name
  val <- xml_nodes(td, xpath="a/text()")
  name <-val[[1]]

  # weight
  td <- columns[3]
  weight <- xml_text(td[[1]])

  # ticker
  td <- columns[2]
  input <- xml_nodes(columns[2], css="div>input")[1]
  ticker <- xml_attr(input[[1]], name="value")
  
#  return(c("Ticker"=ticker, "Url"=url, "Name"=name, "Weight"=weight));
  return(c(ticker, url, xmlValue(name), weight));
}

download.file("http://slickcharts.com/sp500", destfile="sp500.html")
sp500 <- html("sp500.html");
rows <- html_nodes(sp500, xpath="//table[2]/tr");
rows <- lapply(rows, extractRowInfo);

# See: http://stackoverflow.com/questions/4227223/r-list-to-data-frame
df <- data.frame(matrix(unlist(rows), nrow=length(rows), byrow=T));
write.csv(df, file="sp500-weights.csv", row.names=FALSE);
