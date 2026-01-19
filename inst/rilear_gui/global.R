trimWS <- function(x, side="both")  {
    side <- match.arg(side, c("left", "right", "both"))
    pattern <- switch(side, left="^\\s+", right="\\s+$", both="^\\s+|\\s+$")
    gsub(pattern, "", x)
}

constrOut <- c("patID"="1", "structure"="2", "constraint"="3", "observed"="4",
               "compliance"="5", "deltaV"="6", "deltaVpc"="7", "deltaD"="8",
               "deltaDpc"="9", "dstMin"="10", "dstMinRel"="11",
               "ptMinD"="12", "ptMinV"="13")
constrOutInv <- c("1"="patID", "2"="structure", "3"="constraint", "4"="observed",
                  "5"="compliance", "6"="deltaV", "7"="deltaVpc", "8"="deltaD",
                  "9"="deltaDpc", "10"="dstMin", "11"="dstMinRel",
                  "12"="ptMinD", "13"="ptMinV")
