#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## risk models from Life Span Study, different sources
## all solid cancer, leukemia & lymphoma, breast
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## Sasaki et al. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213
## based on https://github.com/JapanHealthPhysicsSociety/SUMRAY/
#####---------------------------------------------------------------------------

## incidence
## CAVE: sex -> 1:m 2:f
rm_solid_incid_sumray <- function() {
    list(
    err=list(param=c(d10col= 0.452246303147881,
                     e30   =-0.189230676147737,
                     lage70=-1.74186812049818,
                     msex  = 0.250755761008251),
             varm=structure(c(0.00195131904979306,   0.000413004853015443,  0.00671838485961983, -0.000156670327057745,
                              0.000413004853015443,  0.00444394708519854,  -0.0127818040921709,  -0.000421103579208728,
                              0.00671838485961983,  -0.0127818040921709,    0.103802938359842,    0.00284023304347374,
                             -0.000156670327057745, -0.000421103579208728,  0.00284023304347374,  0.00491548662758437),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 beta[1]*dose * exp(beta[2]*((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }),
    ear=list(param=c(d10col= 0.00505108100884472,
                     e30   =-0.287599669064682,
                     lage70= 2.37403004629745,
                     msex  = 0.171952011757523),
             varm=structure(c( 2.44198984049119e-07,  5.36502503488343e-06,  6.21799394209355e-05, -1.29885990096665e-05,
                               5.36502503488343e-06,  0.00400540779279526,  -0.0105161313375575,   -0.000145731631208299,
                               6.21799394209355e-05, -0.0105161313375575,    0.0747308867068616,   -0.00275992238361282,
                              -1.29885990096665e-05, -0.000145731631208299, -0.00275992238361282,   0.00505962128686142),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 beta[1]*dose * exp(beta[2] * ((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }))
}

## mortality
rm_solid_mort_sumray <- function() {
    list(
    err=list(param=c(d10col= 0.422650,
                     e30   =-0.345890,
                     lage70=-0.857428,
                     msex  = 0.344079),
             varm=structure(c( 0.00253235,  0.00140692,  0.00692967, -0.00028615,
                               0.00140692,  0.00661873, -0.01588490, -0.00101646,
                               0.00692967, -0.01588490,  0.17912000,  0.00354537,
                              -0.00028615, -0.00101646,  0.00354537,  0.00771445),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 beta[1]*dose * exp(beta[2]*((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }),
    ear=list(param=c(d10col= 0.00263965,
                     e30   =-0.21346100,
                     lage70= 3.38439000,
                     msex  = 0.06769100),
             varm=structure(c( 9.87492e-08,  6.41078e-06,  4.17608e-05, -1.25134e-05,
                               6.41078e-06,  5.12184e-03, -1.22614e-02, -2.25000e-05,
                               4.17608e-05, -1.22614e-02,  1.33872e-01, -5.84032e-03,
                              -1.25134e-05, -2.25000e-05, -5.84032e-03,  9.31031e-03),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 beta[1]*dose * exp(beta[2]*((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }))
}

#####---------------------------------------------------------------------------
## Walsh et al. Radiation and Environmental Biophysics (2021) 60:213-231
## https://doi.org/10.1007/s00411-021-00910-0
## CAVE
## EAR is given per 10000 PY in publication -> rescale coefficients, variances
#####---------------------------------------------------------------------------

## CAVE: sex -> 1:m 2:f
rm_solid_incid_walsh2021 <- function() {
    list(
    err=list(param=c(d10col= 0.5024,
                     e30   =-0.2133,
                     lage70=-1.567,
                     msex  = 0.2864),
             varm=structure(c( 0.00191,   0.00105,  0.00282, -0.000275,
                               0.00105,   0.00261, -0.00500, -0.000309,
                               0.00282,  -0.00500,  0.0557,   0.00163,
                              -0.000275, -0.000309, 0.00163,  0.00345),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 tse <- age - agex # time since exposure
                 beta[1]*dose * exp(beta[2]*((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }),
    ear=list(param=c(d10col= 53.31 / 10000,
                     e30   =-0.3194,
                     lage70= 2.35,
                     msex  = 0.1385),
             varm=structure(c( 22.8   / (10000^2),  0.116 / 10000,  0.290 / 10000, -0.113 / 10000,
                                0.116 / 10000,      0.00259,       -0.00465,       -0.000132,
                                0.290 / 10000,     -0.00465,        0.0440,        -0.00236,
                               -0.113 / 10000,     -0.000132,      -0.00236,        0.00387),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10col", "e30", "lage70", "msex"),
                                           c("d10col", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 tse <- age - agex # time since exposure
                 beta[1]*dose * exp(beta[2] * ((agex - 30)/10) + beta[3]*log(age/70)) *
                     (1 + beta[4]*c(-1, 1)[sex]) }))
}

#####---------------------------------------------------------------------------
## Walsh et al. Radiation and Environmental Biophysics (2021) 60:213-231
## https://doi.org/10.1007/s00411-021-00910-0
## CAVE
## EAR is given per 10000 PY in publication -> rescale coefficients, variances
#####---------------------------------------------------------------------------

## CAVE: sex -> 1:m 2:f
rm_breast_incid_walsh2021 <- function() {
    list(
    err=list(param=c(d10col= 0.8776,
                     e30   =-0.004856,
                     lage70=-2.224),
             varm=structure(c(0.0429,   0.00333,  0.0879,
                              0.00333,  0.0186,  -0.0522,
                              0.0879,  -0.0522,   0.494),
                            .Dim=c(3L, 3L),
                            .Dimnames=list(c("d10col", "e30", "lage70"),
                                           c("d10col", "e30", "lage70"))),
             f   =function(beta, dose, agex, age, sex) {
                 err <- beta[1]*dose * exp(beta[2]*((agex-30)/10) + beta[3]*log(age/70))
                 c(0, 1)[sex]*err  # make sure 0 for male
             }),
    ear=list(param=c(d10col= 9.257 / 10000,
                     e30   =-0.4543,
                     lage70= 1.725),
             varm=structure(c( 2.49   / (10000^2),  0.0453 / 10000,  0.274 / 10000,
                               0.0453 / 10000,      0.0146,         -0.0346,
                               0.274  / 10000,     -0.0346,          0.205),
                            .Dim=c(3L, 3L),
                            .Dimnames=list(c("d10col", "e30", "lage70"),
                                           c("d10col", "e30", "lage70"))),
             ## argument sex necessary for consistent interface, but ignored here
             f   =function(beta, dose, agex, age, sex) {
                 ear <- beta[1]*dose * exp(beta[2] * ((agex-30)/10) + beta[3]*log(age/70))
                 c(0, 1)[sex]*ear  # make sure 0 for male
             }))
}

#####---------------------------------------------------------------------------
## Walsh et al. Radiation and Environmental Biophysics (2021) 60:213-231
## https://doi.org/10.1007/s00411-021-00910-0
## CAVE
## EAR is given per 10000 PY in publication -> rescale coefficients, variances
#####---------------------------------------------------------------------------

## CAVE: sex -> 1:m 2:f
rm_leuk_incid_walsh2021 <- function() {
    list(
    err=list(param=c(d10rbm  = 0.7899,
                     d10rbmSq= 0.9501,
                     e30     =-0.8075,
                     lage70  =-1.09),
             varm=structure(c(  0.218,   -0.0555,  0.0287,  0.00466,
                               -0.0555,   0.123,   0.0242,  0.0396,
                                0.0287,   0.0242,  0.0671, -0.0555,
                                0.00466,  0.0396, -0.0555,  0.197),
                            .Dim=c(4L, 4L),
                            .Dimnames=list(c("d10rbm", "d10rbmSq", "e30", "lage70"),
                                           c("d10rbm", "d10rbmSq", "e30", "lage70"))),
             ## argument sex necessary for consistent interface, but ignored here
             f   =function(beta, dose, agex, age, sex) {
                 ## no downward interpolation below 5 years since exposure
                 ## no downward interpolation below 5 years attained age
                 age[age < 5] <- 5
                 tse <- age-agex # time since exposure
                 tse[tse < 5] <- 5
                 (beta[1]*dose + beta[2]*(dose^2)) * exp(beta[3]*(tse/40) + beta[4]*log(age/70)) }),
    ear=list(param=c(d10rbm  = 1.059 / 10000,
                     d10rbmSq= 1.086 / 10000,
                     e30     = 0.4118,
                     lage70  =-1.447,
                     fsex    =-0.421),
             varm=structure(c(  0.302  / (10000^2),  -0.0746 / (10000^2), -0.0204 / 10000,   0.0821 / 10000,  -0.0393 / 10000,
                               -0.0746 / (10000^2),   0.175  / (10000^2), -0.0114 / 10000,   0.0545 / 10000,  -0.0273 / 10000,
                               -0.0204 / 10000,      -0.0114 / 10000,      0.0116,          -0.0288,           0.00162,
                                0.0821 / 10000,       0.0545 / 10000,     -0.0288,           0.112,           -0.00620,
                               -0.0393 / 10000,      -0.0273 / 10000,      0.00162,         -0.00620,          0.0551),
                            .Dim=c(5L, 5L),
                            .Dimnames=list(c("d10rbm", "d10rbmSq", "e30", "lage70", "msex"),
                                           c("d10rbm", "d10rbmSq", "e30", "lage70", "msex"))),
             f   =function(beta, dose, agex, age, sex) {
                 ## no downward interpolation below 5 years since exposure
                 ## no downward interpolation below 5 years attained age
                 age[age < 5] <- 5
                 tse <- age-agex # time since exposure
                 tse[tse < 5] <- 5
                 (beta[1]*dose + beta[2]*(dose^2)) * exp(beta[3]*(tse/40) + beta[4]*log(age/70)) *
                     exp(beta[5]*c(0, 1)[sex]) }))
}
