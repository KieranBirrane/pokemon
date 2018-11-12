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

pokemon_list_update = get_pokemon_details(151,1)
pokemon_list_update = get_pokemon_details(213)
pokemon_list_update = get_pokemon_details(350)
pokemon_list_update = get_pokemon_details(358)
pokemon_list_update = get_pokemon_details(445)
pokemon_list_update = get_pokemon_details(807,1)



for(i in 1:nrow(pokemon_list_update)){
  row = pokemon_list_update[i,]
  
  if(nrow(pokemon_list[pokemon_list["pokemon_number"]==row["pokemon_number"][[1]],])==0){
    pokemon_list = rbind(pokemon_list,row)
  }else{
    pokemon_list[pokemon_list["pokemon_number"]==row["pokemon_number"][[1]],] = row
  }
}

pokemon_list = pokemon_list[order(pokemon_list["pokemon_number"]),]

write.csv(pokemon_list, file = "Standard_Data\\pokemon_master_list.csv", row.names = FALSE)
