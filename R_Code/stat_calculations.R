###
### Pokemon Stat Calculations
###

#####
##### Load data
#####
pokemon = read.csv(file = "pokemon.csv", stringsAsFactors = FALSE)
pokemon_list = read.csv(file = "pokemon_master_list.csv", stringsAsFactors = FALSE)
natures = read.csv(file = "natures.csv", stringsAsFactors = FALSE)

tempdf1 = merge(pokemon, natures, by="nature", all.x=TRUE)
tempdf2 = merge(tempdf1, pokemon_list, by="pokemon_name", all.x=TRUE)

pokemon_data = tempdf2
# colnames(pokemon_details)



pokemon_id = "shuckle"
pokemon_id = "chimecho"
pokemon_id = "milotic"
pokemon_id = "togekiss"
pokemon_id = "chimecho2"
pokemon_details = pokemon_data[pokemon_data["id"]==pokemon_id,]




pokemon_details = reset_evs(pokemon_details)

pokemon_details = get_bulk_split(pokemon_details,127)


get_stats(pokemon_details,"ev")
get_total_evs(pokemon_details)
get_stats(pokemon_details,"actual")


pokemon_data[pokemon_data["id"]==pokemon_id,] = pokemon_details

output = pokemon_data[,-which(names(pokemon_data) %in% base_stats)]
output = output[,-which(names(output) %in% nature_stats)]
write.csv(output, file = "pokemon.csv", row.names = FALSE)


