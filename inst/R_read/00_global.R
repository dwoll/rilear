#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## helper objects and functions for reading data
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(stringr)

#####---------------------------------------------------------------------------
## general functions
#####---------------------------------------------------------------------------

## last observation carried forward
locf_value <- function(x) {
    l <- !is.na(x)
    rep(c(NA, x[l]), diff(c(1L, which(l), length(x) + 1L)))
}

replace_umlauts <- function(x) {
    str_replace_all(x, c('ü'='ue', 'ä'='ae', 'ö'='oe',
                         'Ü'='Ue', 'Ä'='Ae', 'Ö'='Oe',
                         'ß'='ss'))
}

make_header_string <- function(x, sep=" ") {
    v0 <- x %>%
        unlist(use.names=FALSE) %>%
        word(start=1, end=1, sep=sep)

    replace_umlauts(v0)
}

make_header_string2 <- function(x) {
    v0 <- x %>%
        unlist(use.names=FALSE)
    
    replace_umlauts(v0)
}

## Gompertz-Makeham law of mortality
## q (force of mortality = hazard)
gompertz <- function(x, alpha, beta, lambda) {
    1 - exp(-lambda - (alpha/beta)*(exp(beta*(x+1)) - exp(beta*x)))
}

#####---------------------------------------------------------------------------
## age functions - groups to numeric
#####---------------------------------------------------------------------------

get_age_mw <- function(s, end_max=105L, out=c("mid", "width")) {
    out   <- match.arg(out)
    age_n <- as.numeric(s)
    start <- age_n
    end   <- age_n

    ## determine last interval
    idx_na     <- is.na(start)
    start_last <- start[idx_na]
    end_last   <- end[idx_na]
    if(length(start_last) > 0L) {
        start_last <- as.numeric(gsub("^([[:digit:]]+)\\+$", "\\1", s[idx_na]))
    }
    
    if(length(end_last) > 0L) {
        end_last[] <- end_max
    }
    
    start[idx_na] <- start_last
    end[idx_na]   <- end_last
    width         <- end - start + 1
    mid           <- start + 0.5*width
    
    if(out == "mid") {
        mid
    } else if(out == "width") {
        width
    } else {
        stop("wrong choice")
    }
}

## given integer age range "05-14", determine interval width
get_age_grp_mw <- function(s, end_max=105L, out=c("mid", "width")) {
    out   <- match.arg(out)
    s_spl <- strsplit(s, "-")
    start <- as.numeric(unlist(Map(head, s_spl, n=1L)))
    end   <- as.numeric(unlist(Map(tail, s_spl, n=1L)))
    
    ## determine last interval
    idx_na     <- is.na(start)
    start_last <- start[idx_na]
    end_last   <- end[idx_na]
    if(length(start_last) > 0L) {
        start_last <- as.numeric(gsub("^([[:digit:]]+)\\+$", "\\1", s[idx_na]))
    }
    
    if(length(end_last) > 0L) {
        end_last[] <- end_max
    }
    
    start[idx_na] <- start_last
    end[idx_na]   <- end_last
    width         <- end - start + 1
    mid           <- start + 0.5*width
    
    if(out == "mid") {
        mid
    } else if(out == "width") {
        width
    } else {
        stop("wrong choice")
    }
}

#####---------------------------------------------------------------------------
## TODO age >= 100 -> 17900 total in 2024, 83.8% female
## proportion per 10000
## https://www.destatis.de/DE/Presse/Pressemitteilungen/2025/09/PD25_N052_12.html
#####---------------------------------------------------------------------------

age_100_propf_country <- 0.838
age_100_fedstate <- c("BW"=2.00,
                      "BY"=1.80,
                      "BE"=2.23,
                      "BB"=1.96,
                      "HB"=1.90,
                      "HH"=2.85,
                      "HE"=2.20,
                      "MV"=2.08,
                      "NI"=2.23,
                      "NW"=2.17,
                      "RP"=2.20,
                      "SL"=2.46,
                      "SN"=2.58,
                      "ST"=2.33,
                      "SH"=2.42,
                      "TH"=2.13)

#####---------------------------------------------------------------------------
## age groups
## different age group definitions depending on source
#####---------------------------------------------------------------------------

age_grp_cancer_rki <- c( "0 - 4" ="00-04",
                         "5 - 9" ="05-09",
                        "10 - 14"="10-14",
                        "15 - 19"="15-19",
                        "20 - 24"="20-24",
                        "25 - 29"="25-29",
                        "30 - 34"="30-34",
                        "35 - 39"="35-39",
                        "40 - 44"="40-44",
                        "45 - 49"="45-49",
                        "50 - 54"="50-54",
                        "55 - 59"="55-59",
                        "60 - 64"="60-64",
                        "65 - 69"="65-69",
                        "70 - 74"="70-74",
                        "75 - 79"="75-79",
                        "80 - 84"="80-84",
                        "85"     ="85+")

age_grp_cancer_by <- c("0-4"  ="00-04",
                       "5-9"  ="05-09",
                       "10-14"="10-14",
                       "15-19"="15-19",
                       "20-24"="20-24",
                       "25-29"="25-29",
                       "30-34"="30-34",
                       "35-39"="35-39",
                       "40-44"="40-44",
                       "45-49"="45-49",
                       "50-54"="50-54",
                       "55-59"="55-59",
                       "60-64"="60-64",
                       "65-69"="65-69",
                       "70-74"="70-74",
                       "75-79"="75-79",
                       "80-84"="80-84",
                       "85+"  ="85+")

age_grp_pop_district <- c("unter 1 Jahr"         ="00",
                          "1 bis unter 2 Jahre"  ="01",
                          "2 bis unter 3 Jahre"  ="02",
                          "3 bis unter 4 Jahre"  ="03",
                          "4 bis unter 5 Jahre"  ="04",
                          "5 bis unter 6 Jahre"  ="05",
                          "6 bis unter 7 Jahre"  ="06",
                          "7 bis unter 8 Jahre"  ="07",
                          "8 bis unter 9 Jahre"  ="08",
                          "9 bis unter 10 Jahre" ="09",
                          "10 bis unter 11 Jahre"="10",
                          "11 bis unter 12 Jahre"="11",
                          "12 bis unter 13 Jahre"="12",
                          "13 bis unter 14 Jahre"="13",
                          "14 bis unter 15 Jahre"="14",
                          "15 bis unter 16 Jahre"="15",
                          "16 bis unter 17 Jahre"="16",
                          "17 bis unter 18 Jahre"="17",
                          "18 bis unter 19 Jahre"="18",
                          "19 bis unter 20 Jahre"="19",
                          "20 bis unter 21 Jahre"="20",
                          "21 bis unter 22 Jahre"="21",
                          "22 bis unter 23 Jahre"="22",
                          "23 bis unter 24 Jahre"="23",
                          "24 bis unter 25 Jahre"="24",
                          "25 bis unter 26 Jahre"="25",
                          "26 bis unter 27 Jahre"="26",
                          "27 bis unter 28 Jahre"="27",
                          "28 bis unter 29 Jahre"="28",
                          "29 bis unter 30 Jahre"="29",
                          "30 bis unter 31 Jahre"="30",
                          "31 bis unter 32 Jahre"="31",
                          "32 bis unter 33 Jahre"="32",
                          "33 bis unter 34 Jahre"="33",
                          "34 bis unter 35 Jahre"="34",
                          "35 bis unter 36 Jahre"="35",
                          "36 bis unter 37 Jahre"="36",
                          "37 bis unter 38 Jahre"="37",
                          "38 bis unter 39 Jahre"="38",
                          "39 bis unter 40 Jahre"="39",
                          "40 bis unter 41 Jahre"="40",
                          "41 bis unter 42 Jahre"="41",
                          "42 bis unter 43 Jahre"="42",
                          "43 bis unter 44 Jahre"="43",
                          "44 bis unter 45 Jahre"="44",
                          "45 bis unter 46 Jahre"="45",
                          "46 bis unter 47 Jahre"="46",
                          "47 bis unter 48 Jahre"="47",
                          "48 bis unter 49 Jahre"="48",
                          "49 bis unter 50 Jahre"="49",
                          "50 bis unter 51 Jahre"="50",
                          "51 bis unter 52 Jahre"="51",
                          "52 bis unter 53 Jahre"="52",
                          "53 bis unter 54 Jahre"="53",
                          "54 bis unter 55 Jahre"="54",
                          "55 bis unter 56 Jahre"="55",
                          "56 bis unter 57 Jahre"="56",
                          "57 bis unter 58 Jahre"="57",
                          "58 bis unter 59 Jahre"="58",
                          "59 bis unter 60 Jahre"="59",
                          "60 bis unter 61 Jahre"="60",
                          "61 bis unter 62 Jahre"="61",
                          "62 bis unter 63 Jahre"="62",
                          "63 bis unter 64 Jahre"="63",
                          "64 bis unter 65 Jahre"="64",
                          "65 bis unter 66 Jahre"="65",
                          "66 bis unter 67 Jahre"="66",
                          "67 bis unter 68 Jahre"="67",
                          "68 bis unter 69 Jahre"="68",
                          "69 bis unter 70 Jahre"="69",
                          "70 bis unter 71 Jahre"="70",
                          "71 bis unter 72 Jahre"="71",
                          "72 bis unter 73 Jahre"="72",
                          "73 bis unter 74 Jahre"="73",
                          "74 bis unter 75 Jahre"="74",
                          "75 bis unter 80 Jahre"="75-79",
                          "80 bis unter 85 Jahre"="80-84",
                          "85 bis unter 90 Jahre"="85-89",
                          "90 Jahre und mehr"    ="90+")

age_grp_mort_fedstate <- c("unter 1 Jahr"         ="00-00",
                           "1 bis unter 5 Jahre"  ="01-04",
                           "5 bis unter 10 Jahre" ="05-09",
                           "10 bis unter 15 Jahre"="10-14",
                           "15 bis unter 20 Jahre"="15-19",
                           "20 bis unter 25 Jahre"="20-24",
                           "25 bis unter 30 Jahre"="25-29",
                           "30 bis unter 35 Jahre"="30-34",
                           "35 bis unter 40 Jahre"="35-39",
                           "40 bis unter 45 Jahre"="40-44",
                           "45 bis unter 50 Jahre"="45-49",
                           "50 bis unter 55 Jahre"="50-54",
                           "55 bis unter 60 Jahre"="55-59",
                           "60 bis unter 65 Jahre"="60-64",
                           "65 bis unter 70 Jahre"="65-69",
                           "70 bis unter 75 Jahre"="70-74",
                           "75 bis unter 80 Jahre"="75-79",
                           "80 bis unter 85 Jahre"="80-84",
                           "85 Jahre und mehr"    ="85+")

age_grp_pop_country <- setNames(c("00", sprintf("%.2d", 1:99), "100+"),
                                c("unter 1 Jahr", paste0(1:99, "-Jährige"), "100 Jahre und mehr"))

age_grp_pop_fedstate <- setNames(c("00", sprintf("%.2d", 1:89), "90+"),
                                 c("unter 1 Jahr", paste0(1:89, "-Jährige"), "90 Jahre und mehr"))

age_grp_lifetable <- setNames(sprintf("%.2d", 0:100),
                              paste0(0:100, c(" Jahre", " Jahr", rep(" Jahre", 98))))

## convert age groups
age2joint_country <- c("00"  ="00",
                       "01"  ="01",
                       "02"  ="02",
                       "03"  ="03",
                       "04"  ="04",
                       "05"  ="05",
                       "06"  ="06",
                       "07"  ="07",
                       "08"  ="08",
                       "09"  ="09",
                       "10"  ="10",
                       "11"  ="11",
                       "12"  ="12",
                       "13"  ="13",
                       "14"  ="14",
                       "15"  ="15",
                       "16"  ="16",
                       "17"  ="17",
                       "18"  ="18",
                       "19"  ="19",
                       "20"  ="20",
                       "21"  ="21",
                       "22"  ="22",
                       "23"  ="23",
                       "24"  ="24",
                       "25"  ="25",
                       "26"  ="26",
                       "27"  ="27",
                       "28"  ="28",
                       "29"  ="29",
                       "30"  ="30",
                       "31"  ="31",
                       "32"  ="32",
                       "33"  ="33",
                       "34"  ="34",
                       "35"  ="35",
                       "36"  ="36",
                       "37"  ="37",
                       "38"  ="38",
                       "39"  ="39",
                       "40"  ="40",
                       "41"  ="41",
                       "42"  ="42",
                       "43"  ="43",
                       "44"  ="44",
                       "45"  ="45",
                       "46"  ="46",
                       "47"  ="47",
                       "48"  ="48",
                       "49"  ="49",
                       "50"  ="50",
                       "51"  ="51",
                       "52"  ="52",
                       "53"  ="53",
                       "54"  ="54",
                       "55"  ="55",
                       "56"  ="56",
                       "57"  ="57",
                       "58"  ="58",
                       "59"  ="59",
                       "60"  ="60",
                       "61"  ="61",
                       "62"  ="62",
                       "63"  ="63",
                       "64"  ="64",
                       "65"  ="65",
                       "66"  ="66",
                       "67"  ="67",
                       "68"  ="68",
                       "69"  ="69",
                       "70"  ="70",
                       "71"  ="71",
                       "72"  ="72",
                       "73"  ="73",
                       "74"  ="74",
                       "75"  ="75",
                       "76"  ="76",
                       "77"  ="77",
                       "78"  ="78",
                       "79"  ="79",
                       "80"  ="80",
                       "81"  ="81",
                       "82"  ="82",
                       "83"  ="83",
                       "84"  ="84",
                       "85"  ="85",
                       "86"  ="86",
                       "87"  ="87",
                       "88"  ="88",
                       "89"  ="89",
                       "90"  ="90+",
                       "91"  ="90+",
                       "92"  ="90+",
                       "93"  ="90+",
                       "94"  ="90+",
                       "95"  ="90+",
                       "96"  ="90+",
                       "97"  ="90+",
                       "98"  ="90+",
                       "99"  ="90+",
                       "90+" ="90+",
                       "100+"="90+")

age2joint_fedstate <- c("00-00"="00-00",
                        "00"   ="00-00",
                        "01-04"="01-04",
                        "01"   ="01-04",
                        "02"   ="01-04",
                        "03"   ="01-04",
                        "04"   ="01-04",
                        "05-09"="05-09",
                        "05"   ="05-09",
                        "06"   ="05-09",
                        "07"   ="05-09",
                        "08"   ="05-09",
                        "09"   ="05-09",
                        "10-14"="10-14",
                        "10"   ="10-14",
                        "11"   ="10-14",
                        "12"   ="10-14",
                        "13"   ="10-14",
                        "14"   ="10-14",
                        "15-19"="15-19",
                        "15"   ="15-19",
                        "16"   ="15-19",
                        "17"   ="15-19",
                        "18"   ="15-19",
                        "19"   ="15-19",
                        "20-24"="20-24",
                        "20"   ="20-24",
                        "21"   ="20-24",
                        "22"   ="20-24",
                        "23"   ="20-24",
                        "24"   ="20-24",
                        "25-29"="25-29",
                        "25"   ="25-29",
                        "26"   ="25-29",
                        "27"   ="25-29",
                        "28"   ="25-29",
                        "29"   ="25-29",
                        "30-34"="30-34",
                        "30"   ="30-34",
                        "31"   ="30-34",
                        "32"   ="30-34",
                        "33"   ="30-34",
                        "34"   ="30-34",
                        "35-39"="35-39",
                        "35"   ="35-39",
                        "36"   ="35-39",
                        "37"   ="35-39",
                        "38"   ="35-39",
                        "39"   ="35-39",
                        "40-44"="40-44",
                        "40"   ="40-44",
                        "41"   ="40-44",
                        "42"   ="40-44",
                        "43"   ="40-44",
                        "44"   ="40-44",
                        "45-49"="45-49",
                        "45"   ="45-49",
                        "46"   ="45-49",
                        "47"   ="45-49",
                        "48"   ="45-49",
                        "49"   ="45-49",
                        "50-54"="50-54",
                        "50"   ="50-54",
                        "51"   ="50-54",
                        "52"   ="50-54",
                        "53"   ="50-54",
                        "54"   ="50-54",
                        "55-59"="55-59",
                        "55"   ="55-59",
                        "56"   ="55-59",
                        "57"   ="55-59",
                        "58"   ="55-59",
                        "59"   ="55-59",
                        "60-64"="60-64",
                        "60"   ="60-64",
                        "61"   ="60-64",
                        "62"   ="60-64",
                        "63"   ="60-64",
                        "64"   ="60-64",
                        "65-69"="65-69",
                        "65"   ="65-69",
                        "66"   ="65-69",
                        "67"   ="65-69",
                        "68"   ="65-69",
                        "69"   ="65-69",
                        "70-74"="70-74",
                        "70"   ="70-74",
                        "71"   ="70-74",
                        "72"   ="70-74",
                        "73"   ="70-74",
                        "74"   ="70-74",
                        "75-79"="75-79",
                        "75"   ="75-79",
                        "76"   ="75-79",
                        "77"   ="75-79",
                        "78"   ="75-79",
                        "79"   ="75-79",
                        "80-84"="80-84",
                        "80"   ="80-84",
                        "81"   ="80-84",
                        "82"   ="80-84",
                        "83"   ="80-84",
                        "84"   ="80-84",
                        "85+"  ="85+",
                        "85-89"="85+",
                        "85"   ="85+",
                        "86"   ="85+",
                        "87"   ="85+",
                        "88"   ="85+",
                        "89"   ="85+",
                        "90"   ="85+",
                        "90+"  ="85+",
                        "91"   ="85+",
                        "92"   ="85+",
                        "93"   ="85+",
                        "94"   ="85+",
                        "95"   ="85+",
                        "96"   ="85+",
                        "97"   ="85+",
                        "98"   ="85+",
                        "99"   ="85+",
                        "100+" ="85+")

age2joint_district <- age2joint_fedstate

age2joint_c44 <- c("00-04"="00-04",
                   "00"   ="00-04",
                   "01"   ="00-04",
                   "02"   ="00-04",
                   "03"   ="00-04",
                   "04"   ="00-04",
                   "05-09"="05-09",
                   "05"   ="05-09",
                   "06"   ="05-09",
                   "07"   ="05-09",
                   "08"   ="05-09",
                   "09"   ="05-09",
                   "10-14"="10-14",
                   "10"   ="10-14",
                   "11"   ="10-14",
                   "12"   ="10-14",
                   "13"   ="10-14",
                   "14"   ="10-14",
                   "15-19"="15-19",
                   "15"   ="15-19",
                   "16"   ="15-19",
                   "17"   ="15-19",
                   "18"   ="15-19",
                   "19"   ="15-19",
                   "20-24"="20-24",
                   "20"   ="20-24",
                   "21"   ="20-24",
                   "22"   ="20-24",
                   "23"   ="20-24",
                   "24"   ="20-24",
                   "25-29"="25-29",
                   "25"   ="25-29",
                   "26"   ="25-29",
                   "27"   ="25-29",
                   "28"   ="25-29",
                   "29"   ="25-29",
                   "30-34"="30-34",
                   "30"   ="30-34",
                   "31"   ="30-34",
                   "32"   ="30-34",
                   "33"   ="30-34",
                   "34"   ="30-34",
                   "35-39"="35-39",
                   "35"   ="35-39",
                   "36"   ="35-39",
                   "37"   ="35-39",
                   "38"   ="35-39",
                   "39"   ="35-39",
                   "40-44"="40-44",
                   "40"   ="40-44",
                   "41"   ="40-44",
                   "42"   ="40-44",
                   "43"   ="40-44",
                   "44"   ="40-44",
                   "45-49"="45-49",
                   "45"   ="45-49",
                   "46"   ="45-49",
                   "47"   ="45-49",
                   "48"   ="45-49",
                   "49"   ="45-49",
                   "50-54"="50-54",
                   "50"   ="50-54",
                   "51"   ="50-54",
                   "52"   ="50-54",
                   "53"   ="50-54",
                   "54"   ="50-54",
                   "55-59"="55-59",
                   "55"   ="55-59",
                   "56"   ="55-59",
                   "57"   ="55-59",
                   "58"   ="55-59",
                   "59"   ="55-59",
                   "60-64"="60-64",
                   "60"   ="60-64",
                   "61"   ="60-64",
                   "62"   ="60-64",
                   "63"   ="60-64",
                   "64"   ="60-64",
                   "65-69"="65-69",
                   "65"   ="65-69",
                   "66"   ="65-69",
                   "67"   ="65-69",
                   "68"   ="65-69",
                   "69"   ="65-69",
                   "70-74"="70-74",
                   "70"   ="70-74",
                   "71"   ="70-74",
                   "72"   ="70-74",
                   "73"   ="70-74",
                   "74"   ="70-74",
                   "75-79"="75-79",
                   "80-84"="80-84",
                   "85+"  ="85+",
                   "85-89"="85+",
                   "90+"  ="85+")

#####---------------------------------------------------------------------------
## federal states
#####---------------------------------------------------------------------------

states_ags <- c("08"="Baden-Wuerttemberg",
                "09"="Bayern",
                "11"="Berlin",
                "12"="Brandenburg",
                "04"="Bremen",
                "02"="Hamburg",
                "06"="Hessen",
                "13"="Mecklenburg-Vorpommern",
                "03"="Niedersachsen",
                "05"="Nordrhein-Westfalen",
                "07"="Rheinland-Pfalz",
                "10"="Saarland",
                "14"="Sachsen",
                "15"="Sachsen-Anhalt",
                "01"="Schleswig-Holstein",
                "16"="Thueringen")

states_abbr <- c("BW"="Baden-Wuerttemberg",
                 "BY"="Bayern",
                 "BE"="Berlin",
                 "BB"="Brandenburg",
                 "HB"="Bremen",
                 "HH"="Hamburg",
                 "HE"="Hessen",
                 "MV"="Mecklenburg-Vorpommern",
                 "NI"="Niedersachsen",
                 "NW"="Nordrhein-Westfalen",
                 "RP"="Rheinland-Pfalz",
                 "SL"="Saarland",
                 "SN"="Sachsen",
                 "ST"="Sachsen-Anhalt",
                 "SH"="Schleswig-Holstein",
                 "TH"="Thueringen")

states_ags_inv  <- setNames(names(states_ags),  unname(states_ags))
states_abbr_inv <- setNames(names(states_abbr), unname(states_abbr))
