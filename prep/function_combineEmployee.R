## Combine most up-to-date unique employees 
# from multiple employee files

combineEmployee <- function(df1, df2, ... ){
  # Continuing employees (get most recent values)
  cont <- df2 %>% semi_join(df1, by = "ProviderNum")
  # New employees
  new <- df2 %>% anti_join(df1, by = "ProviderNum")
  # Old employees
  old <- df1 %>% anti_join(df2, by = "ProviderNum")
  
  df <- rbind(cont, new, old)
  
  return(df)
}