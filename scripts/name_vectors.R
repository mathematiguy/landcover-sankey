geography_names <- c("New Zealand", "Northland", "Auckland",
   "Waikato", "Bay of Plenty", "Gisborne", "Hawke\'s Bay", "Taranaki",
   "Manawatu-Wanganui", "Wellington", "Tasman", "Nelson", "Marlborough",
   "West Coast", "Canterbury", "Otago", "Southland")

pal_detailed <- filter(colours, class_type=="Detailed")
pal_detailed <- list(range=pal_detailed$colour, domain=pal_detailed$class_name)

pal_medium <- filter(colours, class_type=="Medium")
pal_medium <- list(range=pal_medium$colour, domain=pal_medium$class_name)

pal_broad <- filter(colours, class_type=="Broad")
pal_broad <- list(range=pal_broad$colour, domain=pal_broad$class_name)