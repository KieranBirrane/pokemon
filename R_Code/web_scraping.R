###
### Serebii Web Scraping
###

#####
##### Download and set packages
#####
# install.packages("rvest")
library("rvest")




#Specifying the url for desired website to be scrapped
number = "445"
url <- paste("https://www.serebii.net/pokedex-sm/",number,".shtml",sep="")

#Reading the HTML code from the website
webpage <- read_html(url)


#Using CSS selectors to scrap the rankings section
bold_data_html = html_nodes(webpage,'b')
bold_data <- html_text(bold_data_html)

name_xml = trimws(bold_data[13])
pokemon_name = substr(name_xml,7,nchar(name_xml))


#Converting the ranking data to text
rank_data <- html_text(bold_data_html)

#Let's have a look at the rankings
head(rank_data[13])

ability_data_html = html_nodes(webpage,'td.fooinfo')
ability_data_html = html_nodes(webpage,'tr')
ability_data <- html_text(ability_data_html)





