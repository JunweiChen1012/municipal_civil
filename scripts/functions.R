
#Write latex tables with notes:
write_latex_placebo = function(star_table, note_text, out_path, scale) {
  
  table_note = note_text %>% 
    str_replace_all("\n", " ")
  
  preamble = c( "\\scalebox{", scale, "}{ 
                \\begin{threeparttable}")
  
  out_table = append(star_table, 
                     preamble,
                     str_detect(star_table, "centering") %>% which) %>%
    append(.,
           c("\\begin{tablenotes}", "\\small", 
             paste0("\\item \\textit{Note}: $^{*}$p$<$0.05; $^{**}$p$<$0.01.", table_note), 
             "\\end{tablenotes}", "\\end{threeparttable}}"),
           str_detect(., "[\\\\]end\\{tabular\\}") %>% which)
  
  cat(out_table, sep = '\n', file = out_path)
}




prop.wide.job.stack <- function(long_data, var1b, var2b=NULL, var3b = NULL, subsetb = NULL){
  jobs = c("blue_collar", "white_collar", "fireman", "police")
  for(i in 1:length(jobs)){
    tmp_data = long_data %>% filter(occ_recode == jobs[i])
    tmp_data = prop.wide(tmp_data, var1 = var1b, var2 = var2b, var3 = var3b, subset = subsetb) %>%
      mutate(occ = jobs[i]) %>%
      rename(govt_occ_total = n_total)
    if(i == 1){
      out = tmp_data
    } else{out = bind_rows(out, tmp_data)}
  }
  return(out)
}


prop.wide <- function(long_data, var1, var2=NULL, var3 = NULL, subset=NULL){
  # takes arguments long_data is a long dataframe
  # var1 is the first grouping variable
  # var2 is default NULL, if you want 2 grouping variables add another here
  # subset is a string of dplyr filter code create a subset (e.g. "race_recode=="black"")
  
  if(!is.null(subset)){
    long_data <- long_data %>%
      filter_(subset)}
  
  if(is.null(var2)){
    out <- long_data %>%
      rename_('groupvar1' = var1) %>%
      group_by(city, YEAR, groupvar1) %>%
      summarise(n = sum(num, na.rm = T)) %>%
      ungroup() %>%
      group_by(city, YEAR) %>%
      mutate(n_total = sum(n, na.rm = T),
             prop = n/n_total) %>%
      mutate(prop = ifelse(is.nan(prop), 0, prop)) %>%
      ungroup() %>%
      select(city, YEAR, n_total, groupvar1, prop) %>%
      spread(key = groupvar1, prop)
  } 
  if(!is.null(var2)){
    
    # 2 variables
    out <- long_data %>%
      rename_('groupvar1' = var1, 'groupvar2' = var2) %>%
      group_by(city, YEAR, groupvar1, groupvar2) %>%
      summarise(n = sum(num, na.rm = T)) %>%
      ungroup() %>%
      group_by(city, YEAR) %>%
      mutate(n_total = sum(n, na.rm = T),
             prop = n/n_total) %>%
      ungroup() %>%
      mutate(keyvar = paste0(groupvar1, '_x_', groupvar2)) %>%
      select(city, YEAR, n_total, keyvar, prop) %>%
      spread(key = keyvar, prop)}
  
  if(!is.null(var3)){
    
    # 2 variables
    out <- long_data %>%
      rename_('groupvar1' = var1, 'groupvar2' = var2, 'groupvar3'= var3) %>%
      group_by(city, YEAR, groupvar1, groupvar2, groupvar3) %>%
      summarise(n = sum(num, na.rm = T)) %>%
      ungroup() %>%
      group_by(city, YEAR) %>%
      mutate(n_total = sum(n, na.rm = T),
             prop = n/n_total) %>%
      ungroup() %>%
      mutate(keyvar = paste0(groupvar1, '_x_', groupvar2, '_x_', groupvar3)) %>%
      select(city, YEAR, n_total, keyvar, prop) %>%
      spread(key = keyvar, prop)}
  
  
  return(out)              
}


