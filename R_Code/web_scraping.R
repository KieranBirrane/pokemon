###
### Serebii Web Scraping
###

#####
##### Load data
#####
pokemon = read.csv(file = "Pokemon_Data\\pokemon.csv", stringsAsFactors = FALSE)
pokemon_list = read.csv(file = "Standard_Data\\pokemon_master_list.csv", stringsAsFactors = FALSE)
natures = read.csv(file = "Standard_Data\\natures.csv", stringsAsFactors = FALSE)

###
### Update pokemon_list
###

get_pokemon_details(151,1)
get_pokemon_details(251,152)
get_pokemon_details(386,252)
get_pokemon_details(493,387)
get_pokemon_details(649,494)
get_pokemon_details(721,650)
get_pokemon_details(807,722)


prepare_pokemon_details()


