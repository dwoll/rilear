#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Statistisches Bundesamt - lifetable, sex specific
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(readxl)

#####---------------------------------------------------------------------------
## read lifetable data
## destatis 12621-0001
#####---------------------------------------------------------------------------

read_lifetable_ger <- function(f, sex=c("f", "m"), age_grp) {
    sex <- match.arg(sex)
    
    d0 <- read_xlsx(f, col_names=FALSE, skip=8) |>
        tibble::remove_rownames()
    
    header <- c("age",
                "q", "q_e",
                "p", "p_e",
                "I", "I_e",
                "d", "d_e",
                "L", "L_e",
                "T", "T_e",
                "e", "e_e")

    dW <- d0 |>
        setNames(header) |>
        filter(!is.na(L)) |>
        select(!matches("^.+_e$")) |>
        mutate(age_f=factor(age,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               age_n=as.numeric(as.character(age_f)),
               sex=factor(sex, levels=c("f", "m"))) |>
        select(-age)
    dW
}
