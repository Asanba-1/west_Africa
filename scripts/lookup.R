###############################################################################
# MASTER LOOKUP TABLES
# West Africa HDSS Sites
###############################################################################

centre_lookup <- tibble(
  
  centreid = c(
    "BF021","BF031","BF041",
    "CI011",
    "GH011","GH021","GH031",
    "GM011",
    "NG011",
    "SN011","SN012","SN013"
  ),
  
  centre_name = c(
    "Nanoro",
    "Nouna",
    "Ouagadougou",
    "Taabo",
    "Navrongo",
    "Kintampo",
    "Dodowa",
    "Farafenni",
    "Nahuche",
    "Bandafassi",
    "Mlomp",
    "Niakhar"
  ),
  
  country = c(
    "Burkina Faso",
    "Burkina Faso",
    "Burkina Faso",
    "Côte d'Ivoire",
    "Ghana",
    "Ghana",
    "Ghana",
    "The Gambia",
    "Nigeria",
    "Senegal",
    "Senegal",
    "Senegal"
  ),
  
  climate_zone = c(
    "Sudano-Sahelian",
    "Sudano-Sahelian",
    "Sudano-Sahelian",
    "Forest",
    "Guinea Savannah",
    "Forest-Savannah Transition",
    "Coastal Savannah",
    "Sudano-Sahelian",
    "Sudano-Sahelian",
    "Sudano-Sahelian",
    "Guinea Savannah",
    "Guinea Savannah"
  )
  
) %>%
  
  mutate(
    
    ##########################################################################
    # Publication labels
    ##########################################################################
    
    centre_label =
      paste0(
        centre_name,
        " (",
        country,
        ")"
      ),
    
    ##########################################################################
    # Short labels for figures
    ##########################################################################
    
    short_label =
      centre_name,
    
    ##########################################################################
    # Country abbreviations
    ##########################################################################
    
    country_short =
      c(
        "Burkina Faso",
        "Burkina Faso",
        "Burkina Faso",
        "Côte d'Ivoire",
        "Ghana",
        "Ghana",
        "Ghana",
        "The Gambia",
        "Nigeria",
        "Senegal",
        "Senegal",
        "Senegal"
      ),
    
    ##########################################################################
    # Plot labels
    ##########################################################################
    
    plot_label =
      paste0(
        centre_name,
        "\n(",
        country,
        ")"
      )
    
  )




centre_lookup <- centre_lookup %>%
  
  mutate(
    
    centre_label = factor(
      centre_label,
      levels = c(
        
        "Nouna (Burkina Faso)",
        "Nanoro (Burkina Faso)",
        "Ouagadougou (Burkina Faso)",
        
        "Taabo (Côte d'Ivoire)",
        
        "Farafenni (The Gambia)",
        
        "Navrongo (Ghana)",
        "Kintampo (Ghana)",
        "Dodowa (Ghana)",
        
        "Nahuche (Nigeria)",
        
        "Bandafassi (Senegal)",
        "Mlomp (Senegal)",
        "Niakhar (Senegal)"
      )
    )
    
  )