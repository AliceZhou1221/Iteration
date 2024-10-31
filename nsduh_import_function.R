nsduh_import = function(html, nth_table, table_name) {
  
  out_df = 
    html %>% 
    html_table() %>% 
    nth(nth_table) %>% 
    slice(-1) %>% 
    mutate(drug = table_name)
  
  return(out_df)
}