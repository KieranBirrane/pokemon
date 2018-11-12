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
get_pokemon_details(213)
get_pokemon_details(350)
get_pokemon_details(358)
get_pokemon_details(445)
get_pokemon_details(807,1)

pokemon_list_update = get_pokemon_details("Standard_Data\\pokemon_master_list.csv",213)



prepare_pokemon_details()


