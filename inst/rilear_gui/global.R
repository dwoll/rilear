library(terra)

trimWS <- function(x, side="both")  {
    side <- match.arg(side, c("left", "right", "both"))
    pattern <- switch(side, left="^\\s+", right="\\s+$", both="^\\s+|\\s+$")
    gsub(pattern, "", x)
}

path_map <- "d:/rilear/inst/rilear_gui/data/"
l_map <- list(ger_fedstate    =vect(paste0(path_map, "bundeslaender.gpkg")),
              # ger_district    =vect(paste0(path_map, "landkreise3.gpkg"))
              ger_district    =vect(paste0(path_map, "landkreise_simplify200.geojson")) #,
              # ger_municipality=vect(paste0(path_map, "gemeinden_simplify200.geojson"))
              )

ld_region <- list(ger_fedstate=data.frame(DEBKG_ID=l_map$ger_fedstate$DEBKG_ID,
                                          OBJID   =l_map$ger_fedstate$OBJID,
                                          AGS     =l_map$ger_fedstate$AGS,
                                          GEN     =l_map$ger_fedstate$GEN),
                  ger_district=data.frame(DEBKG_ID=l_map$ger_district$DEBKG_ID,
                                          OBJID   =l_map$ger_district$OBJID,
                                          AGS     =l_map$ger_district$AGS,
                                          GEN     =l_map$ger_district$GEN))

## TODO: some AGS don't match with pop dataset
## in map, not in pop: "16056" Eisenach
## in pop, not in map:
## 03241001 "Hannover, Landeshauptstadt"       
## 05334002 "Aachen, kreisfreie Stadt"         
## 10041100 "Saarbrücken, Landeshauptstadt"    
## 11001001 "Berlin-Mitte"                     
## 11002002 "Berlin-Friedrichshain-Kreuzberg"  
## 11003003 "Berlin-Pankow"                    
## 11004004 "Berlin-Charlottenburg-Wilmersdorf"
## 11005005 "Berlin-Spandau"                   
## 11006006 "Berlin-Steglitz-Zehlendorf"       
## 11007007 "Berlin-Tempelhof-Schöneberg"      
## 11008008 "Berlin-Neukölln"                  
## 11009009 "Berlin-Treptow-Köpenick"          
## 11010010 "Berlin-Marzahn-Hellersdorf"       
## 11011011 "Berlin-Lichtenberg"               
## 11012012 "Berlin-Reinickendorf"

l_region <- list(ger_fedstate    =setNames(l_ger_map[["fedstate"]]$GEN,
                                           l_ger_map[["fedstate"]]$AGS),
                 ger_district    =setNames(l_ger_map[["district"]]$GEN,
                                           l_ger_map[["district"]]$AGS) # ,
                 # ger_municipality=setNames(l_ger_map[["municipality"]]$GEN,
                 #                           l_ger_map[["municipality"]]$AGS)
                 )

l_region_inv <- list(ger_fedstate    =setNames(l_ger_map[["fedstate"]]$AGS,
                                               l_ger_map[["fedstate"]]$GEN),
                     ger_district    =setNames(l_ger_map[["district"]]$AGS,
                                               l_ger_map[["district"]]$GEN) # ,
                     # ger_municipality=setNames(l_ger_map[["municipality"]]$AGS,
                     #                           l_ger_map[["municipality"]]$GEN)
                     )
