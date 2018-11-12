###
### Pokemon Functions
###

#####
##### Set working directory
#####
getwd()
wd = "E:\\GitHub\\pokemon"
setwd(wd)

#####
##### Download and set packages
#####
# install.packages("rvest")
library("rvest")

#####
##### Set variables
#####
pokemon_info = c("pokemon_name","level","nature")
actual_stats = c("hp","attack","defence","special_attack","special_defence","speed")
actual_iv = paste("iv_",actual_stats,sep="")
actual_ev = paste("ev_",actual_stats,sep="")
base_stats = paste("base_",actual_stats,sep="")
nature_stats = paste("nat_",actual_stats,sep="")
stat_no_ev = paste(actual_stats,"_no_ev",sep="")
stat_plus_ev = paste(actual_stats,"_plus_ev",sep="")



#####                         ##################################################
##### Set functions for Stats ##################################################
#####                         ##################################################
calc_hp <- function(pokemon_details){
  
  stat = "hp"
  iv = pokemon_details[paste("iv_",stat,sep="")]
  base = pokemon_details[paste("base_",stat,sep="")]
  ev = pokemon_details[paste("ev_",stat,sep="")]
  level = pokemon_details["level"]
  
  pokemon_details[stat] = floor(((iv+2*base+floor(ev/4))*level/100)+10+level)
  pokemon_details[paste(stat,"_no_ev",sep="")] = iv*level/100 + 2*base*level/100 + 10 + level
  pokemon_details[paste(stat,"_plus_ev",sep="")] = floor(pokemon_details[paste(stat,"_no_ev",sep="")] + floor(ev/4)*level/100)
  
  return(pokemon_details)
}

calc_stat <- function(pokemon_details, stat){
  
  iv = pokemon_details[paste("iv_",stat,sep="")]
  base = pokemon_details[paste("base_",stat,sep="")]
  ev = pokemon_details[paste("ev_",stat,sep="")]
  level = pokemon_details["level"]
  nature = pokemon_details[paste("nat_",stat,sep="")]
  
  stat_result = floor((((iv+2*base+floor(ev/4))*level/100)+5)*nature)
  pokemon_details[stat] = floor((((iv+2*base+floor(ev/4))*level/100)+5)*nature)
  pokemon_details[paste(stat,"_no_ev",sep="")] = iv*nature*level/100 + 2*base*nature*level/100 + 5*nature
  pokemon_details[paste(stat,"_plus_ev",sep="")] = floor(pokemon_details[paste(stat,"_no_ev",sep="")] + floor(ev/4)*nature*level/100)
  
  return(pokemon_details)
}

calc_all_stats <- function(pokemon_details){
  
  for(stat_func in actual_stats){
    if(stat_func == "hp"){
      pokemon_details = calc_hp(pokemon_details)
    }else{
      pokemon_details = calc_stat(pokemon_details,stat_func)
    }
  }
  
  return(pokemon_details)
}

get_stats <- function(pokemon_details,stat_type){
  
  if(stat_type == "actual"){
    return_cols = actual_stats
  }else if(stat_type == "iv"){
    return_cols = actual_iv
  }else if(stat_type == "ev"){
    return_cols = actual_ev
  }else if(stat_type == "no_ev"){
    return_cols = stat_no_ev
  }else if(stat_type == "plus_ev"){
    return_cols = stat_plus_ev
  }else{
    return(pokemon_details[pokemon_info])
  }
  
  return(pokemon_details[c(pokemon_info,return_cols)])
  
}



get_bulk_split <- function(pokemon_details,num_stat_points,defences_equal="Y",eviolite="N"){
  
  ii_max = num_stat_points
  k = 20000
  loop = c(0:ii_max)
  counter = 0
  df = data.frame("counter"= counter, "bulk" = 0, "hp_inc" = 0, "def_inc" = 0, "sdef_inc" = 0, "act_hp" = 0, "act_def" = 0, "act_sdef" = 0, "def_diff" = 9999)
  hp = pokemon_details["hp"]
  def = pokemon_details["defence"]
  sdef = pokemon_details["special_defence"]
  ev_hp = pokemon_details["ev_hp"]
  ev_def = pokemon_details["ev_defence"]
  ev_sdef = pokemon_details["ev_special_defence"]
  nat_def = pokemon_details["nat_defence"]
  nat_sdef = pokemon_details["nat_special_defence"]
  if(eviolite=="Y"){eviolite_mult = 1.5}else{eviolite_mult=1}
  
  for(i in loop){
    hp_inc = i
    def_sdef = ii_max - i
    loop2 = c(0:def_sdef)

    for(j in loop2){
      counter = counter + 1
      def_inc = j
      sdef_inc = def_sdef - j
      
      act_hp = hp + hp_inc
      act_def = (def + def_inc*nat_def)*eviolite_mult
      act_sdef = (sdef + sdef_inc*nat_sdef)*eviolite_mult
      def_diff = abs(floor(act_def)-floor(act_sdef))

      harm = (k*(act_def+act_sdef)+4*act_def*act_sdef)/(act_hp*act_def*act_sdef)
      bulk = (hp_inc + hp)*(def_inc*nat_def + sdef_inc*nat_sdef + 0.5*(def + sdef))
      temp_df = data.frame("counter"= counter,"bulk" = 1/harm[[1]], "hp_inc" = hp_inc, "def_inc" = def_inc, "sdef_inc" = sdef_inc, "act_hp" = act_hp[[1]], "act_def" = act_def[[1]], "act_sdef" = act_sdef[[1]], "def_diff" = def_diff[[1]])
      
      
      df = rbind(df,temp_df)

    }
  }

  df = df[ev_hp[[1]]+4*df["hp_inc"]<253,]
  df = df[ev_def[[1]]+4*df["def_inc"]<253,]
  df = df[ev_sdef[[1]]+4*df["sdef_inc"]<253,]

  if(defences_equal=="Y"){
    min_diff = min(df["def_diff"])
    df = df[df["def_diff"]==min_diff,]
  }

  max_val = max(df["bulk"])
  max_def_evs = df[df["bulk"]==max_val,]

  if(nrow(max_def_evs)>1){
    max_hp = max(max_def_evs["hp_inc"])
    max_def_evs = max_def_evs[max_def_evs["hp_inc"]==max_hp,]
  }

  hp_evs = 4 * max_def_evs["hp_inc"]
  def_evs = 4 * max_def_evs["def_inc"]
  sdef_evs = 4 * max_def_evs["sdef_inc"]
  
  pokemon_details["ev_hp"] = pokemon_details["ev_hp"] + hp_evs
  pokemon_details["ev_defence"] = pokemon_details["ev_defence"] + def_evs
  pokemon_details["ev_special_defence"] = pokemon_details["ev_special_defence"] + sdef_evs

  pokemon_details = calc_all_stats(pokemon_details)
  return(pokemon_details)
}

reset_evs <- function(pokemon_details){
  
  for(stat in actual_ev){
    pokemon_details[stat] = 0
  }
  
  pokemon_details = calc_all_stats(pokemon_details)

  return(pokemon_details)
  
}

get_total_evs <- function(pokemon_details){
  
  total_evs = 0

  for(stat in actual_ev){
    total_evs = total_evs + pokemon_details[stat][[1]]
  }
  
  return(total_evs)
  
}





#####                                ##################################################
##### Set functions for Web Scraping ##################################################
#####                                ##################################################
get_pokemon_name_from_dextab <- function(input_dextab){

  pokemon_name_html = html_nodes(input_dextab,'b')
  pokemon_name = html_text(pokemon_name_html)
  pokemon_name = tolower(substr(pokemon_name,7,nchar(pokemon_name)))
  
  return(pokemon_name)
}

get_pokemon_base_stats_from_dextable <- function(input_dextable){
  
  stats_tr = html_nodes(input_dextable,'tr')
  stats_base_tr = stats_tr[3]
  
  stats_base_td = html_nodes(stats_base_tr,'td')
  stats_base = html_text(stats_base_td)
  
  return(stats_base)
}

get_index_from_string <- function(html_code,string){
  
  for(i in 1:length(html_code)){
    pos = regexpr(string, html_code[i])
    
    if(pos!=-1){
      index=i
    }
  }

  return(index)
}

get_pokemon_details <- function(end_num,start_num=0){
  
  #Specifying the url for desired website to be scrapped
  if(start_num==0){start_num=end_num}
  base_url = "https://www.serebii.net/"
  game = "pokedex-sm/"
  df = data.frame("pokemon_number"="","pokemon_name"="","base_total"="","base_total_sum"="","difference"="","base_hp"="","base_attack"="","base_defence"="","base_special_attack"="","base_special_defence"="","base_speed"="",stringsAsFactors = FALSE)
  
  
  for(i in start_num:end_num){
    number = substring(paste0("000",i),nchar(i)+1)
    url = paste(base_url,game,number,".shtml",sep="")

    #Reading the HTML code from the website
    webpage = read_html(url)
    
    # Get Pokemon data from this webpage
    web_data_html = html_nodes(webpage,'div')
    pokemon_data_html = web_data_html[8]

    pokemon_dextab = html_nodes(pokemon_data_html,'table.dextab')
    pokemon_dextable = html_nodes(pokemon_data_html,'table.dextable')

    # Set table variables
    name_dextab = pokemon_dextab[1]
    pokedex_dextab = pokemon_dextab[2]

    stats_index = get_index_from_string(pokemon_dextable,"<b>Stats</b>")
    stats_dextable = pokemon_dextable[stats_index]
    
    # Get values
    pokemon_name = get_pokemon_name_from_dextab(name_dextab)
    stats_base = get_pokemon_base_stats_from_dextable(stats_dextable)

    # Create pokemon record
    base_total = as.numeric(substring(stats_base[1],21))
    base_total_sum = as.numeric(stats_base[2])+as.numeric(stats_base[3])+as.numeric(stats_base[4])+as.numeric(stats_base[5])+as.numeric(stats_base[6])+as.numeric(stats_base[7])
    record = data.frame("pokemon_number"=number,"pokemon_name"=pokemon_name,"base_total"=base_total,"base_total_sum"=base_total_sum,"difference"=total_stats-total_stats_sum,"base_hp"=stats_base[2],"base_attack"=stats_base[3],"base_defence"=stats_base[4],"base_special_attack"=stats_base[5],"base_special_defence"=stats_base[6],"base_speed"=stats_base[7],stringsAsFactors = FALSE)
    
    # Append to full record
    df = rbind(df,record)
  }
  
  # Remove first row and reindex
  df = df[-c(1), ]
  rownames(df) <- 1:nrow(df)

  return(df)
}