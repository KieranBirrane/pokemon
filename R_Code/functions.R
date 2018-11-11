###
### Pokemon Functions
###

#####
##### Set working directory
#####
getwd()
wd = "E:/Pokemon"
setwd(wd)

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



#####
##### Set functions
#####
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



get_bulk_split <- function(pokemon_details,num_stat_points,defences_equal="Y"){
  
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
  
  for(i in loop){
    hp_inc = i
    def_sdef = ii_max - i
    loop2 = c(0:def_sdef)

    for(j in loop2){
      counter = counter + 1
      def_inc = j
      sdef_inc = def_sdef - j
      
      act_hp = hp + hp_inc
      act_def = def + def_inc*nat_def
      act_sdef = sdef + sdef_inc*nat_sdef
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