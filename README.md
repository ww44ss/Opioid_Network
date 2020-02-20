# Opioid_Network
 Network analysis of Wash Post Opioid Data  
   
# Based on the Washington Post Opioid Data for 2006 to 2012  
 
# Programs and Files
 network_summary.csv : summarized dataset for network analysis
         file size: 93MB
         file length: 1,048,576 lines
         Columns:
         1. REPORTER_STATE (two letter postal code)
         2. REPORTER_NAME (text)
         3. BUYER_STATE (two letter postal code)
         4. YEAR: (text)
         5. TOTAL_WT_IN_GM: numerical
         
 summary__data_all_city_month_year.R : program which produced network_summary.csv from the raw Wash Post data
 
 Network.RMD is an analysis notebook.
 
 Directory: Interactive Network
 Network Viewer Interactive.r : a shiny app for tuning network visualization
