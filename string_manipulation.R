# Input:
# [1] "1 800 CONTACTS INC (De-listed 09/2007)"                                                          
# [2] "CENTRAL WEALTH GROUP HOLDINGS LTD (China Soft Power Technology Holdings Ltd prior to 02/2019)"   
# [4] "GLEACHER & COMPANY INC (Broadpoint Gleacher Securities Group Inc prior to 05/2010) (De-listed 07/2014)"
# [5] "FAR LTD"                                                                                         
# [6] "FIRST AVIATION SERVICES INC (De-listed 07/2007)"                                                 
# [7] "FIRST BANCORP NC"


BoardName_woDetails = str_extract(bx_comp_visible_withAddrOrURL$BoardName, "(.(?!\\())+")
# [1] "1 800 CONTACTS INC"               
# [2] "CENTRAL WEALTH GROUP HOLDINGS LTD"
# [4] "GLEACHER & COMPANY INC"           
# [5] "FAR LTD"                          
# [6] "FIRST AVIATION SERVICES INC"      
# [7] "FIRST BANCORP NC"

com_history_name <- str_trim(str_extract(bx_comp_visible_withAddrOrURL$BoardName, "(?<=\\()[^()]+(?=([:space:]){0,2}prior to)"), "both")
# [1] NA                                        
# [2] "China Soft Power Technology Holdings Ltd"
# [3] NA                                        
# [4] "Broadpoint Gleacher Securities Group Inc"

com_history_name_date <- str_trim(str_extract_all(bx_comp_visible_withAddrOrURL$BoardName, "(?<=prior to([:space:]){0,2})[^()]+(?=\\))"), "both")
# [1] "character(0)" 
# [2] "02/2019"      
# [3] "character(0)" 
# [4] "05/2010" 

de_listed_history <- str_trim(str_extract(bx_comp_visible_withAddrOrURL$BoardName, "(?<=\\(De-listed([:space:]){0,2})[^()]+(?=\\))"), "both")
# [1] "09/2007" 
# [2] NA        
# [3] NA        
# [4] "07/2014" 
