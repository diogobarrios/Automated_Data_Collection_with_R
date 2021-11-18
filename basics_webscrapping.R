# the Basics ---- 
# Automated Data Collection with R
# resoource: http://www.r-datacollection.com

## specifying the packages that will be needed
packages <- c("stringr", "XML", "maps", "RCurl", "RJSONIO", "jsonlite",
              "plyr", "httr", "RSQLite", "webdriver", "RSelenium", "tidyverse", "rvest")

## Checking if it is installed otherwise load it.
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = T)) {
    install.packages(x, dependencies = T)
    library(x, character.only = T)
    }
  }
)


# Chapter 1 - Introduction ------

url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

# get the HTML data to a object
heritage_data <- getURL(url)

# Parse the information from HTML 
heritage_parsed <- htmlParse(heritage_data, encoding = "UTF-8")

# As function says, read the table from HTML as table in R
tables <- readHTMLTable(heritage_parsed, stringsAsFactors = F, header = T)
danger_table <- tables[[2]]
names(danger_table)

# just want some cols
danger_table <- danger_table[,c(1, 3, 4, 6, 7)]
colnames(danger_table) <-  c("name", "locn", "crit", "yins", "yend")

# verifying the first 3 lines
danger_table$name[1:3]

# Inspecting how the data is 
danger_table$crit[1:3] # "Cultural:(iv)" 
danger_table$yend[1:3] # "2001â€“" "1992â€“" "2013â€“"
danger_table$yins[1:3] # "1979" "1991" "1986"

# Cleaning these data
danger_table$crit <- ifelse(str_detect(danger_table$crit, "Natural") == T, "nat", "cult")
# Verify the result
danger_table$crit[1:3]

danger_table$yins <- as.numeric(danger_table$yins)
# Verify the result
danger_table$yins[1:3]

# Extracting and using REGEX, is like:
# [:digit:] - one digit, I want the all 4 digits, so I put {4}, like multiplying, or
# [:digit:][:digit:][:digit:][:digit:] but it's ugly.
yend_clean <- unlist(str_extract(danger_table$yend, "[:digit:]{4}"))
danger_table$yend <- as.numeric(yend_clean)


danger_table$locn[1]

reg_y <- "[/][ -]*[:digit:]*[.]*[:digit:]*[;]"
reg_x <- "[;][ -]*[:digit:]*[.]*[:digit:]*"

# Extracting only ... / 30.84167; 29.66389.. " with reg_y
y_coords <- str_extract(danger_table$locn, reg_y)

# removing the first 3 characters and the last 2 ones "/ 30.84167;"
y_coords <- as.numeric(str_sub(y_coords, 3, -2))

# Add the col y_coords
danger_table$y_coords <- y_coords

x_coords <- str_extract(danger_table$locn, reg_x)
x_coords <- as.numeric(str_sub(x_coords, 3, -1))
danger_table$x_coords <- x_coords

# Removing the locn col
danger_table$locn <- NULL

# Verifying the final result
head(danger_table)
dim(danger_table) #52 rows & 6 cols

# Visualization with "maps" package

# Mark as triangle "19" as "nat" or circle "2" as "cult"
pch <- ifelse(danger_table$crit == "nat", 19, 2)
map("world", col = "darkgrey", lwd = 0.5, mar = c(0.1, 0.1, 0.1, 0.1))
points(danger_table$x_coords, danger_table$y_coords, pch = pch)
box()


# Chapter 2 - HTML -----

url_ch2 <- "http://www.r-datacollection.com/materials/ch-2-html/fortunes.html"

# readLines doesnt interpret as a DOM the html file.
fortunes <- readLines(con = url_ch2)
fortunes

# With parser XML will read HTML file as DOM
fortunes_parsed <- htmlParse(file = url_ch2)
print(fortunes_parsed)

# Create a handler to delete all coments in the DOM tree
h1 <- list(comment <- function(x){NULL})
fortunes_parsed <- htmlTreeParse(file = url_ch2, handlers = h1, asTree = T)
fortunes_parsed

# Create a handler to delete all "div" & "title"
h2 <- list(startElement = function(node, ...){
  name <- xmlName(node)
  if(name %in% c("div", "title")){NULL} else {node}
}
)
fortunes_parsed <- htmlTreeParse(file = url_ch2, handlers = h2, asTree = T)



# Chapter 3 - XML & JSON ----

# to validate XML file, if it is well written:
# http://www.xmlvalidation.com

url_ch3 <- "http://www.r-datacollection.com/materials/ch-3-xml/stocks/technology.xml"

# Parsing from the URL
stocks_parsed <- xmlParse(file = url_ch3, validate = T)
stocks_parsed

# top-level extraction of the XML file
stocks_root <- xmlRoot(stocks_parsed)

# Root element name:
xmlName(stocks_root)

# How many childs has:
xmlSize(stocks_root) 

# Transform in dataframe
stocks_df <- xmlToDataFrame(stocks_root)
head(stocks_df, 3)
tail(stocks_df, 3)

# Only Apple's daily closing value and date
# First create a handler
branchFun <- function(){
  container_close <- numeric()
  container_date <- numeric()

"Apple" =  function(node, ...){
  date <- xmlValue(xmlChildren(node)[[c("date")]])
  container_date <<- c(container_date, date)
  close <- xmlValue(xmlChildren(node)[[c("close")]])
  container_close <<- c(container_close, close)
  #print(c(close, date)); Sys.sleep(0.5)
    }
  getContainer <- function() data.frame(date = container_date, close = container_close)
  list(Apple = Apple, getStore = getContainer)
  }

h5 <- branchFun()
# Better memory efficiency with handlers = list()
invisible(xmlEventParse(file = url_ch3, branches = h5, handlers = list()))

# store the data
apple.stock <- h5$getStore()
head(apple.stock, 5)

url_ch3.1 <- "http://www.r-datacollection.com/materials/ch-3-xml/bond.xml"

bond <- xmlParse(url_ch3.1)
bond
class(bond)
# top-level node
root <- xmlRoot(bond)
# The name of the top-level node
xmlName(root)
# How much they are!
xmlSize(root)

# We can use index numerically or with name but using root
# cause this one has "XMLInternalElementNode" and bond doesnt.
movie_id_1 <- root[[1]]
movie_id_1_budget <- root[["movie"]][[5]][[1]]

# Take all values from movie_id2
xmlSApply(root[[2]], xmlValue)     

# Take by attr
xmlSApply(root, xmlGetAttr, "id")

# Tranform in dataframe, since from the root node, only
# are grandchildren
movie_df <- xmlToDataFrame(root)
movie_df



# JSON

url_ch3.2 <- "http://www.r-datacollection.com/materials/ch-3-xml/indy.json"


# to verify if it is a JSON file
isValidJSON(url_ch3.2)


indy <- fromJSON(getURL(url_ch4.1, encoding = "latin1"))
# Which class it is? #list
class(indy)  

# Extract info by variable
# the recursive ensures that all componentes of the list are unlisted
# use.name = TRUE, we can identify all original key/value pairs with the name "name"
indy.vec <- unlist(indy, recursive = T, use.names = T)

# Detect the name of the movies
indy.vec[str_detect(names(indy.vec), "name")]

# And I can get the years vector
indy[[1]][["year"]]

# or using these way
sapply(indy[1], "[","year")


# Chapter 4 - xPath ----

# Using url_ch2

parsed_doc <- htmlParse(file = url_ch2)

# Extracting using absolute path
xpathSApply(doc = parsed_doc, path = "/html/body/div/p/i")

# Extracting using relative path
xpathSApply(doc = parsed_doc, path = "//body//p/i")
xpathSApply(doc = parsed_doc, path = "//p/i")

# Using wildcard operator *
xpathSApply(doc = parsed_doc, path = "//body/div/*/i")

# Using .. operator
xpathSApply(doc = parsed_doc, path = "//title/..")

# using | pipe operator to indicate several paths
xpathSApply(doc = parsed_doc, path = "//address | //title")

# Anther way of indicating several paths
twoQueries <- c(address = "//address", title = "//title")
xpathSApply(doc = parsed_doc, twoQueries)

# Extracting using relations
xpathSApply(parsed_doc, path = "//a/ancestor::div")
# Here we get all the node until the div that preceds the <a> node.
# now if we want to get some node from the div cluster we just do it
# using relative path
xpathSApply(parsed_doc, path = "//a/ancestor::div//i")

xpathSApply(parsed_doc, path = "//title/parent::*")

# Extracting using predicates
xpathSApply(parsed_doc, path = "//div/p[position()=1]")
# Here we get all the <p> nodes that is in the first place after the <div> node

# If we want to see the last node we simply
xpathSApply(parsed_doc, path = "//div/p[last()]")
# if we want the see the nodes before the last nodes
xpathSApply(parsed_doc, path = "//div/p[last()-1]")

# This will count how many <a> nodes are afther the div
xpathSApply(parsed_doc, path = "//div[count(.//a)>0]")

xpathSApply(parsed_doc, path = "//div[count(./@*)>2]")
xpathSApply(parsed_doc, path = "//div[not(count(./@*)>2)]")

# Extracting using textual predicates
# Select all <div> nodes that contain "October 2011" in the attr DATE
xpathSApply(parsed_doc, path = "//div[@date = 'October/2011']")

xpathSApply(parsed_doc, path = "//*[contains(text(), 'magic')]")
xpathSApply(parsed_doc, path = "//*[contains(text(), 'request')]")

# Extracting node elements
xpathSApply(parsed_doc, path = "//title", fun = xmlValue)

xpathSApply(parsed_doc, path = "//i", fun = xmlValue)

# Extracting node attr
xpathSApply(parsed_doc, path = "//div", xmlAttrs)

# Exctrating specific node attr
xpathSApply(parsed_doc, path = "//div", xmlGetAttr, "lang")


# Extracting info with functions

# tolowercase
lowerCasefun <- function(x){
  x <- tolower(xmlValue(x))
  x
}

xpathSApply(parsed_doc, path = "//div//i", fun = lowerCasefun)

# Date

datefun <- function(x){
  require(stringr)
  date <- xmlGetAttr(node = x, name = "date")
  year <- str_extract(date, "[0-9]{4}")
  year
}

xpathSApply(parsed_doc, path = "//div", datefun)

# Creating variables and using them on xPath expression

url_ch3
# Create a variable with the name of the companies
companies <- c("Apple", "IBM", "Google")

expQuery <- sprintf("//%s/close", companies)

getClose <- function(x){
  value <- xmlValue(x)
  company <- xmlName(xmlParent(x))
  mat <- c(company = company, value = value)
}

stocks <- as.data.frame(t(xpathSApply(stocks_parsed, expQuery, getClose)))
stocks$value <- as.numeric(as.character(stocks$value))
head(stocks, 3)



# Chapter 5 - HTTP ----

# These comands will return in the corresponding plataform
R.version$version.string
R.version$platform

cat(getURL("http://httpbin.org/headers",
           useragent = str_c(R.version$version.string, R.version$platform, sep = ", ")))
# cat() is used to concatenate and print the results over several lines

# Referer, it stores the URL of the page that referred the user to the current page

cat(getURL("http://httpbin.org/headers", referer = "http://www.r-datacollection.com"))

# Basic Authentification with Base64

#Enconding Base64
(secret <- base64("Este é o meu nome"))

#Decoding Base64
base64Decode(secret)

# RcURL or cURL 
# http://curl.haxx.se/libcurl/c/curl_easy_setopt.html

# GET form
url_ch5 <- "http://www.r-datacollection.com/materials/http/GETexample.php"
namepar <- "Eddie"
agepar <- "32"
url_get <- str_c(url_ch5, "?", "name=",namepar, "&", "age=", agepar)

cat(getURL(url_get))

# Or we can use getForm
cat(getForm(url_ch5, name = "Eddie", age = "32"))

# Using the customrequest option
url_ch5.1 <- "r-datacollection.com/materials/http/helloworld.html"
res <- getURL(url_ch5.1, customrequest = "HEAD", header = T)
cat(str_split(res, "\r")[[1]])

# Using curlperfom

content <- basicTextGatherer()
header <- basicHeaderGatherer()
debug <- debugGatherer()

performOption <- curlOptions(url = url_ch5.1,
                             writefunc = content$update,
                             headerfunc = header$update,
                             debugfunc = debug$update,
                             verbose = T)
curlPerform(.opts = performOption)

# using the value() function to show the results
str_sub(content$value())
cat(header$value())
names(debug$value())

# default Rcurl options

defaultOptions <- curlOptions(
  httpheader = list(
    from = "diogobarrios22@gmail.com",
    'user-agent' = str_c(R.version$platform, 
                         R.version$version.string,
                         sep = ", ")),
  followlocation = T,
  maxredirs = 10,
  connecttimeout = 10,
  timeout = 10,
  cookiefile = "RCurlCookies.txt",
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# options(RCurlOptions = defaultOptions)

# Chapter 7 - SQL ----

# Estabish connection
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, "birthdays.db")

# 'plain' SQL
sql <- "SELECT * FROM birthdays"
res <- dbGetQuery(con, sql)
res
res <- dbSendQuery(con, sql)
fetch(res)

# Exercise:
sqlite <- dbDriver("SQLite")
pokedex <- dbConnect(sqlite, "pokedex.sqlite")

dbListTables(pokedex)

query1 <- "SELECT * FROM pokemon WHERE weight > 4000"
res <- dbGetQuery(pokedex, query1)
res
query2 <- "SELECT * FROM pokemon_species"
res2 <- dbGetQuery(pokedex, query2)
head(res2)

query3 <- "SELECT identifier, pokemon.id, height, weight, base_experience FROM pokemon_species
           JOIN pokemon ON pokemon_species.id = pokemon.id
           WHERE pokemon.weight > 4000" 
res3 <- dbGetQuery(pokedex, query3)
res3

query4 <- "SELECT identifier FROM pokemon_species
           WHERE identifier LIKE '%Nido%'"
res4 <- dbGetQuery(pokedex, query4)
res4

query5 <- "SELECT identifier FROM pokemon_species"
res5 <- dbSendQuery(pokedex, query5)
fetch(res5, n = 5)
dbClearResult(res5)

query6 <- "SELECT pokemon.id, 
                    pokemon_species.id,
                    identifier AS name, 
                    height, 
                    weight, 
                    evolves_from_species_id AS evolves_species, 
                    evolution_chain_id FROM pokemon_species
            JOIN pokemon ON pokemon_species.id = pokemon.id"
pokeview <- dbGetQuery(pokedex, query6)
# View(pokeview)

# Chapter 8 - Regular Expressions -----

example.obj <- "1. A small sentence. - 2. Another tiny sentence."

# Exact Character matching
  
str_extract(example.obj, "small")
str_extract(example.obj, c("small", "big"))

# Extract every matvh
str_extract(example.obj, "sentence")
str_extract_all(example.obj, "sentence")

out <- str_extract_all(c("text", "manipulation", "basics"), "a")
out <- unlist(out)

# REGEX is case-sensitive
# won't find any exact match
str_extract(example.obj, "SMALL")

# ^ for the begining of the sentence
# $ for the end of the sentence

str_extract(example.obj, "2")

str_extract(example.obj, "^2")

unlist(str_extract_all(example.obj, "sentence$"))

# \ pipe operator, works like OR

unlist(str_extract_all(example.obj, "sentence|tiny"))

# Generalizing REGEX

# . period character matches any character

str_extract(example.obj, "sm.ll")

# [] character classes means any character within the square bracket will match

str_extract(example.obj, "t[aeoui].y")

# [ - ] character classes but with range using dash

str_extract(example.obj, "s[a-z]all")

unlist(str_extract_all(example.obj, "[uwv. ]"))

# All alphabetic characters

unlist(str_extract_all(example.obj, "[:punct:]"))

unlist(str_extract_all(example.obj, "[[:punct:]ABC]"))

# Does the opposite
unlist(str_extract_all(example.obj, "[^[:alnum:]]"))

str_extract(example.obj, "s[:alpha:][:alpha:][:alpha:]l")

# or can be done with a quantifier {number inside}

str_extract(example.obj, "s[:alpha:]{3}l")


str_extract(example.obj, "A.+sentence")

str_extract(example.obj, "A.+?sentence")

unlist(str_extract_all(example.obj, "(.en){1,5}"))

unlist(str_extract_all(example.obj, "\\."))

# taking the words
unlist(str_extract_all(example.obj, "\\w+"))

# or i can use [[:alnum]_]
unlist(str_extract_all(example.obj, "[:alnum:]+"))


# Backreferecing
str_extract(example.obj, "([:alpha:]).+?\\1")

str_extract(example.obj, "(\\b[b-z]+\\b).+?\\1")



# Example

raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"

# [:alpha:] to get all letters, .,(space character) to take all name that have space
# between and impose a quantifcer 2, that have to have at least 2 letters.
name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))

phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))

phoneBook <- data.frame(name = name, phone = phone)
phoneBook

# Stringr Package

# locate of a match in a string
str_locate(example.obj, "tiny")

# take it off!
str_sub(example.obj, start = 35, end = 38)

# I can replace too the word with another 4 letters word
str_sub(example.obj, start = 35, end = 38) <- "huge"
example.obj

str_replace(example.obj, pattern = "huge", replacement = "giant")

# split the string
unlist(str_split(example.obj, "-"))

unlist(str_split(example.obj, "[:blank:]", 5))

# Working with several strings
char.vec <- c("this", "and this", "and that")

str_detect(char.vec, "this")

str_count(char.vec, "this")

str_count(char.vec, "\\w+")

str_length(char.vec)

str_pad(char.vec, width = max(str_length(char.vec)), side = "both", pad = " ")

# str_trim strips, excess of white spaces from the edges

str_trim(str_pad(char.vec, width = max(str_length(char.vec)), side = "both", pad = " "))

# join strings together

#cat(str_c(str_trim(str_pad(char.vec, width = max(str_length(char.vec)), side = "both", pad = " ")), 
#      collapse = "\n"))

str_c("text", "manipulation", sep = " ")

str_c("text", c("manipulation", "basics"), sep = " ")

# Approx Match

agrep("Barack Obama", "Barack H. Obama", max.distance = list(all = 3))


# Exercise

raw <- "clcopCow1zmstc0d87wnkig7OvdicpNuggvhryn92Gjuwczi8hqrfpRxs5Aj5dwpn0TanwoUwisdij
7Lj8kpf03AT5Idr3coc0bt7yczjatOaootj55t3Nj3ne6c4Sfek.r1w1YwwojigOd6vrfUrbz2.2
bkAnbhzgv4R9i05zEcrop.wAgnb.SqoU65fPa1otfb7wEm24k6t3sR9zqe5fy89n6Nd5t9kc4fE905gmc4Rgxo5nhDk!gr"

str_extract_all(raw, "[:alpha:]{2,}")
str_extract_all(raw, "[:digit:]{1,}")

secret_message <- unlist(str_extract_all(raw, "[:upper:]"))
secret_message <- str_c("C","O","N","G","R","A","T","U","L","A","T","I","O","N","S",
        "Y","O","U","A","R","E","A","S","U","P","E","R","N","E","R","D", sep = "")

first <- str_extract(secret_message, "[:alpha:].+?S")
second <- str_extract(secret_message, "Y[:alpha:]U")
third <- str_extract(secret_message, "A.E")
fourth <- str_extract(secret_message, "[:alpha:]{9}$")

secret_message <- str_c(first, second, third, fourth, sep = " ")
secret_message

# A PRACTICAL TOOLBOX FOR WEB SCRAPING AND TEXT MINING ----
# Chapter 9 - Scraping the Web ----

# Identifying locations using getHTMLLinks()

# used httr package
url_ch9 <- GET("https://elections.maryland.gov/elections/2012/election_data/index.html")

# retrive me a object with a response class, is list full of choices of info.
class(url_ch9)

# get all links based on the url provided, but I have to use
# rawToChar because of the class response, and them getHTMLLinks will be able to
# read
links <- getHTMLLinks(rawToChar(url_ch9$content))

#I only want links that has "_General.csv"
filenames <- links[str_detect(links, "_General.csv")]
filenames_list <- as.list(filenames)

# only to see if I get it!
filenames_list[1:3]

# Constructing a download function to download all .csv files
# create a function that has filename, base url and the folder that we receive all csv's
downloadCSV <- function(filename, baseurl, folder){
  # creating the diretory
  dir.create(folder, showWarnings = F)
  # creating the fileurl with concatenate baseurl with filename
  fileurl <- str_c(baseurl, filename)
  # if the file doesn't exist the the folder, download it to the folder and sleep for 1s
  if(!file.exists(str_c(folder, "/", filename))) {
    download.file(fileurl, destfile = str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

# Using l_ply() from tha plyr package, for list 
l_ply(filenames_list,
      downloadCSV,
      baseurl = "https://elections.maryland.gov/elections/2012/election_data/",
      folder = "elect12_maryland")

# See the result, how many files did I downloaded
length(list.files("./elect12_maryland"))

list.files("./elect12_maryland")[1:3]

# Using again httr

url_ch9.1 <- GET("https://planning.maryland.gov/Redistricting/Pages/MD-Cong-Legis-Dist.aspx")
links9.1 <- getHTMLLinks(rawToChar(url_ch9.1$content))
links9.1

filenames9.1 <- links9.1[str_detect(links9.1, "2010Maps/Leg/District.+")]
# This next step is redundant, but only to know that I can do this if I have another links
# that I scrap but doesnt finish in .pdf
filename_list9.1 <- str_extract_all(filenames9.1, "District.+pdf")
filename_list9.1[1:3]

# Constructing a download pdf function
downloadPDF <- function(filename, baseurl, folder, handle){
  dir.create(folder, showWarnings = F)
  fileurl <- str_c(baseurl, filename)
  if(!file.exists(str_c(folder,"/",filename))){
    content <- getBinaryURL(fileurl, curl = handle)
    writeBin(content, str_c(folder,"/",filename))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle(useragent = str_c(R.version$platform, R.version$version.string, sep = ", "),
                        httpheader = c(from = "eddie@datacollection.com"))

l_ply(filename_list9.1, 
      downloadPDF,
      baseurl = "https://planning.maryland.gov/Redistricting/Documents/Documents/2010Maps/Leg/",
      folder = "elec12_maryland_maps",
      handle = handle)


# Downloading multiple files from an FTP index

# Note
# For FTP servers the getHTMLLinks() command is not an option,
# because the documents are not structured as HTML.

ftp <-  "ftp://cran.r-project.org/pub/R/web/views/"

ftp_files <- getURL(ftp, dirlistonly = T)

ftp_files

filenames_html <- unlist(str_extract_all(ftp_files, ".+html"))
filenames_html[1:3]

# Constructing a download pdf function
downloadFTP <- function(filename, baseurl, folder, handle){
  # create the directory
  dir.create(folder, showWarnings = F)
  # create the fileurl
  fileurl <- str_c(baseurl, filename)
  # If doesnt show a filename in the folder specified then..
  if(!file.exists(str_c(folder, "/", filename))){
    # get the binary file and handle the connection
    content <- getBinaryURL(fileurl, curl = handle)
    # write&save binary file 
    writeBin(content, str_c(folder, "/", filename))
    Sys.sleep(1)
  }
}

handle_ftp <- getCurlHandle(ftp.use.epsv = F)

l_ply(filenames_html, 
      downloadFTP, 
      baseurl = "ftp://cran.r-project.org/pub/R/web/views/",
      folder = "cran_tasks", handle = handle)

length(list.files("./cran_tasks"))


# Manipulating URLs to access multiple pages

# Using RCurl
url_transparency <- getURL("https://www.transparency.org/en/press/p1")

# Parsing html
transparency_html <- htmlParse(url_transparency)
transparency_html
transparency_links <- unlist(str_extract_all(getHTMLLinks(transparency_html),
  # I want all words that start after forward slash, any char(.), one or more 
  # times, and has "-"

                            pattern = "press/[a-z].+(-).+"))

transparency_links

# getting from multiple pages
pages <- str_c("/p", seq(1, 151, 1))


# Constructing a function to get all pages links
getPageURLs <- function(url){
  # Parsing the url within the getURL funtction from RCurl
  baseurl <- htmlParse(getURL(url))
  # Starting "/p1" until "/p151", by 1 
  pages <- str_c("/p", seq(1, 151, 1))
  # Contonate a list with url + pages
  url_list <- as.list(str_c(url, pages))
  url_list[length(url_list) + 1] <- url
  return(url_list)
}

url_list <- getPageURLs("https://www.transparency.org/en/press")
url_list[1:3]

# Constructing a function to return all index from each page

dlPages <- function(pageurl, folder, handle){
  dir.create(folder, showWarnings = F)
  page_name <- str_c(str_extract(pageurl, "p[:digit:]+"), ".html")
  if(page_name == "NA.html"){page_name <- "/base.html"}
  if(!file.exists(str_c(folder,"/",page_name))){
    content <- try(getURL(pageurl, curl = handle))
    write(content, str_c(folder, "/", page_name))
    Sys.sleep(1)
  }
}

handle <- getCurlHandle()


# l_ply(url_list, dlPages, folder = "tp_imdex", handle = handle)

# list.files("./tp_imdex")[1:3]

# Extract all releases from each page, which are 15 per page
getPressURLs <- function(folder){
  pages_parsed <- lapply(str_c(folder, "/", dir(folder)), htmlParse)
  urls <- unlist(llply(pages_parsed, getHTMLLinks))
  press_urls <- urls[str_detect(urls, "http.+/press/")]
  press_urls_list <- as.list(press_urls)
  return(press_urls_list)
}

# folder <- "tp_imdex"
# lapply(str_c(folder, "/", dir(folder)), getURL)

press_urls_list <- getPressURLs(folder = "tp_imdex")
length(press_urls_list)

# Download press releases
press_release <- unlist(str_extract_all(press_urls_list, pattern = "press/[:alnum:].+(-).+"))
length(press_release) 


# Convenient functions to gather links, lists, and tables from HTML documents

# first object stores the URL for the article
mac_url <- "https://en.wikipedia.org/wiki/Niccol%C3%B2_Machiavelli"

# second stores the source code
mac_source <- readLines(con = mac_url, encoding = "UTF-8")

# third stores the parsed document 
mac_parsed <- htmlParse(mac_source, encoding = "UTF-8")
# XML content does not seem to be XML: '' if I use mac_url directly on mac_parsed

# fourth holds only one node of the parsed document,
mac_node <- mac_parsed["//p"]

# I can use getHTMLLinks to achieve

getHTMLLinks(getURL(mac_url))[1:10]

getHTMLLinks(mac_source)[1:10]
  
getHTMLLinks(mac_parsed)[1:10]

# Using Xpath

getHTMLLinks(mac_source, xpQuery = "//a[@class = 'extiw']/@href")[1:3]

mac_xpath <- "//img[contains(@src, 'Machiavelli')]/@src"

getHTMLExternalFiles(mac_source, xpQuery = mac_xpath)[1:3]  

# To give me the names of the tables in HTML source code
names(readHTMLTable(mac_source))
# But since I get all table names, 'NULL', so i can get by indexing
readHTMLTable(mac_source)[1][[1]]

readHTMLTable(mac_source, stringAsFactors = F)[[1]][13:14,1]

influential <- readHTMLTable(mac_source, 
                             elFun = getHTMLLinks,
                             stringAsFactors = F)[[1]][13,]

influential[1:3]

influenced <- readHTMLTable(mac_source, 
                            elFun = getHTMLLinks, 
                            stringAsFactors = F)[[1]][14,]
influenced[1:3]

# Dealing with HTML forms

info <- debugGatherer()

handle_form <- getCurlHandle(cookiejar = "",
                             followlocation = T,
                             autoreferer = T,
                             debugfunc = info$update,
                             verbose = T,
                             httpheader = list(
                               from = "diogobarrios22@gmail.com",
                               'user-agent' = str_c(R.version$version.string,
                                                    R.version$platform)
                             ))

xmlAttrsTodf <- function(parsedHTML, xpath) {
  x <- xpathApply(parsedHTML, xpath, xmlAttrs)
  x <- lapply(x, function(x) as.data.frame(t(x)))
  do.call(rbind.fill, x)
}


url_form <- "http://wordnetweb.princeton.edu/perl/webwn"
html_form <- getURL(url_form, curl = handle_form)
parsed_form <- htmlParse(html_form)

xmlAttrsTodf(parsed_form, "//form")

xmlAttrsTodf(parsed_form, "//form[1]/input")

html_form_res <- getForm(uri = url_form, 
                         curl = handle_form,
                         # Search term
                         s = "data")
parsed_form_res <- htmlParse(html_form_res)

# get only the string from the search term that I put before
xpathApply(parsed_form_res, "//li", xmlValue)

# See the info
# cat(str_split(info$value() ["headerOut"], "\r")[[1]])

# Another way to achieve the same result,
# When we search a term, the url only changes with ?s=data
url_form1 <- "http://wordnetweb.princeton.edu/perl/webwn?s=data"

html_form1_res <- getURL(url_form1, curl = handle_form)

parsed_form1_res <- htmlParse(html_form1_res)

xpathApply(parsed_form_res, "//li", xmlValue)

# RHTMLForms package

url_wordNet <- "http://wordnetweb.princeton.edu/perl/webwn"
form_wordNet <- getHTMLFormDescription(url_wordNet)
formFunction_wordNet <- createFunction(form_wordNet)[[1]]
                                       
html_wordNet <- formFunction_wordNet(s = "data", .curl = handle_form)
parsed_wordNet <- htmlParse(html_wordNet)
xpathApply(parsed_wordNet, "//li", xmlValue)

# Using cookies

info <- debugGatherer()        # a place to store cookie information in
handle_cookie <- getCurlHandle(cookiejar = "",
                               followlocation  = T,
                               autoreferer = T,
                               debugfunc = info$update,
                               verbose = T,
                               httpheader = list ( 
                                 from = "diogobarrios22@gmail.com",
                                 'user-agent' = str_c(R.version$version.string,
                                                      R.version$platform)))

search_url <- "www.biblio.com/search.php?keyisbn=data"
cart_url   <- "www.biblio.com/cart.php"

# download the search results page and directly parse and save it in search_page.
search_page <- htmlParse(getURL(url = search_url, curl = handle_cookie))

# Adding items to the shopping cart is done via HTML forms
xpathApply(search_page, "//div[@class ='actions']/form")[[1]]
         
# extract the book IDs to later add items to the cart
xpathBid <- "//div[@class = 'actions']/form/input[@name='bid']/@value"
bids <- unlist(xpathApply(search_page, xpathBid, as.numeric))
bids[1]
# add the first three items from the search results page to the shopping cart by 
# sending the necessary information (bid,add,andint) to the server
for(i in seq_along(bids)){
  res <- getForm(uri = cart_url,
                 curl = handle_cookie,
                 add = 1,
                 bid = bids[i],
                 int = "keyword_search")
}

# retrieve the shopping cart and check out the items that have been stored
cart <- htmlParse(getURL(cart_url, curl = handle_cookie))
clean <- function(x) str_replace_all(xmlValue(x), "(\t) | (\n\n)", "")  

cart_title_books <- xpathSApply(cart, "///h4[@class='title']", clean)
# If i used only xmlValue instead of clean, it will output the same result

# Managing cookies manually

handle_cookie1 <- getCurlHandle(cookiejar = "cookies.txt")
res1 <- getURL("http://httpbin.org/cookies/set?k1=v1&k2=v2",
              curl = handle_cookie1) 
# All cookies are stored in this file whenever cookielist = "FLUSH" is
# specified as option to an RCurl function 
handle_cookie1 <- curlSetOpt(cookielist = "FLUSH", curl = handle_cookie1)

readLines("cookies.txt")  
new_handle_cookie1 <- getCurlHandle(cookiefile = "cookies.txt")  
getURL("http://httpbin.org/cookies", curl = new_handle_cookie1, cookielist = "ALL")  

# Scraping data from AJAX-enriched webpages with RSelenium/webdriver

# Instead of bypassing web browsers, we leverage their capabilities of 
# interpreting JavaScript and formulating changes to the live DOM tree by 
# directly including them into the scraping process. 
# Essentially, this means that all communication with a webpage is routed 
# through a web browser session to which we send and from which we receive 
# information  

# Case study: Federal Contributions Database
url_fcd <- "http://www.r-datacollection.com/materials/selenium/intro.html"

# start a firefox browser
rD<- rsDriver(browser = "firefox")
# use default server initialisation values
remDr <- rD$client
# Open new session
remDr$open()
# Navigate through webpage
remDr$navigate("http://www.r-datacollection.com/materials/selenium/intro.html")

# To know WindowID
remDr$getWindowHandles()
# to see the page
remDr$screenshot(display = T)
# Get url
remDr$getCurrentUrl()

# get title page
remDr$getTitle()

# click the button "database" with xPath
ButtonID <- remDr$findElement("xpath", "html/body/div/div/form/input")
# I have to take [2] off to get the right 
# Click the Button "Enter database"
ButtonID$clickElement()
remDr$screenshot(display = T)

# But When I click in the button open up 2 pages, and the focus of Selenium
#inst in the page where I want. 
# windowHandle!
allHandles <- remDr$getWindowHandles
allHandles()
# I have 2 options, or I close window or I switch the window
# Close Window
# remDr$closeWindow()

# Change focus to another window
remDr$switchToWindow(allHandles()[1])
# remDr$closeWindow()

# know working with the database

yearID <- remDr$findElement("xpath", "//*[@id='yearSelect']")
monthID <- remDr$findElement("xpath", "//*[@id='monthSelect']")
recipID <- remDr$findElement("xpath", "//*[@id='recipientSelect']")

# In order to change the year, we perform a mouse click on the year field
yearID$clickElement()
# we need to pass the keyboard input that we wish to enter into the database field
yearID$sendKeysToElement(list("2013"))
# The same for Month
monthID$clickElement()
monthID$sendKeysToElement(list("January"))
# The same for recipient
recipID$clickElement()
recipID$sendKeysToElement(list("Barack Obama"))

# Send the query to the database with a click on the submit button
submitID <- remDr$findElement("xpath", "/html/body/div/form/div/button")

# Pass the corresponding ID element to the clicking function.
submitID$clickElement()

# extract the underlying HTML code from the live DOM tree and search the code for a table
# Save the HTML info
page_source <- remDr$getPageSource()

# I have to unlist() page_source first, and then readHTMLTable will the table.
# Header = T to get the first line as the colnames
moneyTab <- readHTMLTable(unlist(page_source), which = 1, header = T)

# Chapter 10 - Statistical text processing ----

# urls for parse

all_links <- character()
signatures <-  system.file("CurlSSL", cainfo = "cacert.pem",package = "RCurl")
handle <- getCurlHandle(useragent = str_c(R.version$platform, R.version$version.string, sep = ", "),
                        httpheader = c(from = "diogobarrios22@gmail.com"))

for(page in seq(from = 1, to = 279, by = 1)) {
  next_page_govpt <- str_c("https://www.portugal.gov.pt/pt/gc22/comunicacao/comunicados?p=", page)
  
  results <- getURL(next_page_govpt, cainfo = signatures, handle = handle)
  results_tree <- htmlParse(results, encoding = "UTF-8")
  all_links <- c(all_links, xpathSApply(results_tree, 
                                        "//a[@class= 'seemore uppercase hidden-sm hidden-xs']", 
                                        xmlGetAttr, 
                                        "href"))
 
 print(paste0("Page No:",page))
 Sys.sleep(20)
}


for(i in 1:length(all_links)){
  url <- all_links[i]
  tmp <- getURL(url, .encoding = "UTF-8")
  write(tmp, str_c("Press_releases/", i, ".html"))
  print(paste0("Page No:", i))
  Sys.sleep(20)
}

# Retirar a data e hora - span.PageSelector__ctl0_SimpleRecordPageLauncher__ctl0_SimpleRecordViewer_PublicationDate
# Retirar o título - div.title ou h1
# Retirar o texto div.gov-texts
# área associada span.tagss





