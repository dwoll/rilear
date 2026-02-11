library(terra)

trimWS <- function(x, side="both")  {
    side <- match.arg(side, c("left", "right", "both"))
    pattern <- switch(side, left="^\\s+", right="\\s+$", both="^\\s+|\\s+$")
    gsub(pattern, "", x)
}

# path_map <- "d:/rilear/inst/rilear_gui/data/"
f_ger_c  <- path_map <- system.file("rilear_gui/data/ger.gpkg",                       package="rilear")
f_ger_fs <- path_map <- system.file("rilear_gui/data/bundeslaender.gpkg",             package="rilear")
f_ger_d  <- path_map <- system.file("rilear_gui/data/landkreise_simplify200.geojson", package="rilear")
f_ger_m  <- path_map <- system.file("rilear_gui/data/gemeinden_simplify200.geojson",  package="rilear")

l_map <- list(ger_country     =vect(f_ger_c),
              ger_fedstate    =vect(f_ger_fs),
              ger_district    =vect(f_ger_d) #,
              # ger_municipality=vect(f_ger_m)
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

l_region <- list(ger_fedstate    =setNames(l_map[["ger_fedstate"]]$GEN,
                                           l_map[["ger_fedstate"]]$AGS),
                 ger_district    =setNames(l_map[["ger_district"]]$GEN,
                                           l_map[["ger_district"]]$AGS) # ,
                 # ger_municipality=setNames(l_map[["ger_municipality"]]$GEN,
                 #                           l_map[["ger_municipality"]]$AGS)
                 )

l_region_inv <- list(ger_fedstate    =setNames(l_map[["ger_fedstate"]]$AGS,
                                               l_map[["ger_fedstate"]]$GEN),
                     ger_district    =setNames(l_map[["ger_district"]]$AGS,
                                               l_map[["ger_district"]]$GEN) # ,
                     # ger_municipality=setNames(l_map[["ger_municipality"]]$AGS,
                     #                           l_map[["ger_municipality"]]$GEN)
                     )
