# ============================================================================= =
# prosail
# Lib_PROSAIL.R
# ============================================================================= =
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr >
# Florian de BOISSIEU <fdeboiss@gmail.com >
# copyright 2019 / 11 Jean-Baptiste FERET
# ============================================================================= =
# This Library includes functions dedicated to PROSAIL simulation
# SAIL versions available are 4SAIL and 4SAIL2
# ============================================================================= =

#" computes bidirectional reflectance factor based on outputs from PROSAIL and sun position
#"
#" The direct and diffuse light are taken into account as proposed by:
#" Francois et al. (2002) conversion of 400-1100 nm vegetation albedo
#" measurements into total shortwave broadband albedo using a canopy
#" radiative transfer model,  Agronomie
#" Es = direct
#" Ed = diffuse
#"
#" @param rdot numeric. Hemispherical-directional reflectance factor in viewing direction
#" @param rsot numeric. Bi-directional reflectance factor
#" @param tts numeric. Solar zenith angle
#" @param specatm_sensor list. direct and diffuse radiation for clear conditions
#" @return brf numeric. Bidirectional reflectance factor
#" @export
compute_brf  <- function(rdot, rsot, tts, specatm_sensor) {

  ############################## #
  ##	direct  /  diffuse light	##
  ############################## #
  es <- specatm_sensor$Direct_Light
  ed <- specatm_sensor$Diffuse_Light
  rd <- pi / 180
  skyl <- 0.847 - 1.61 * sin((90 - tts) * rd) +  1.04 * sin((90 - tts) * rd) * sin((90 - tts) * rd) # diffuse radiation (Francois et al.,  2002)
  pardiro <- (1 - skyl) * es
  pardifo <- skyl * ed
  brf <- (rdot * pardifo + rsot * pardiro) / (pardiro + pardifo)
  return(brf)
}

#" Performs PROSAIL simulation based on a set of combinations of input parameters
#" @param spec_sensor list. Includes optical constants required for PROSPECT
#" refractive index,  specific absorption coefficients and corresponding spectral bands
#" @param input_prospect  list. PROSPECT input variables
#" @param n numeric. Leaf structure parameter
#" @param chl numeric. chlorophyll content (microg.cm-2)
#" @param car numeric. carotenoid content (microg.cm-2)
#" @param ant numeric. anthocyain content (microg.cm-2)
#" @param brown numeric. brown pigment content (Arbitrary units)
#" @param ewt numeric. Equivalent Water Thickness (g.cm-2)
#" @param lma numeric. Leaf Mass per Area (g.cm-2)
#" @param prot numeric. protein content  (g.cm-2)
#" @param cbc numeric. nonprotcarbon-based constituent content (g.cm-2)
#" @param alpha numeric. Solid angle for incident light at surface of leaf (simulation of roughness)
#" @param typelidf numeric. Type of leaf inclination distribution function
#" @param lidfa numeric.
#" if typelidf  == 1,  controls the average leaf slope
#" if typelidf  == 2,  corresponds to average leaf angle
#" @param lidfb numeric.
#" if typelidf  == 1,  unused
#" if typelidf  == 2,  controls the distribution"s bimodality
#" @param lai numeric. Leaf Area Index
#" @param q numeric. Hot Spot parameter
#" @param tts numeric. Sun zeith angle
#" @param tto numeric. Observer zeith angle
#" @param psi numeric. Azimuth Sun  /  Observer
#" @param rsoil numeric. Soil reflectance
#" @param fraction_brown numeric. Fraction of brown leaf area
#" @param diss numeric. Layer dissociation factor
#" @param cv numeric. vertical crown cover percentage
#" = % ground area covered with crowns as seen from nadir direction
#" @param zeta numeric. Tree shape factor
#" = ratio of crown diameter to crown height
#" @param sailversion character. choose between 4SAIL and 4SAIL2
#" @param brownvegetation list. Defines optical properties for brown vegetation,  if not nULL
#" - WVL,  reflectance,  Transmittance
#" - Set to nULL if use PROSPECT to generate it
#"
#" @return list. rdot, rsot, rddt, rsdt
#" rdot: hemispherical-directional reflectance factor in viewing direction
#" rsot: bi-directional reflectance factor
#" rsdt: directional-hemispherical reflectance factor for solar incident flux
#" rddt: bi-hemispherical reflectance factor
#" @import prospect
#" @export
pro4sail  <- function(spec_sensor, input_prospect = nULL, n = 1.5, chl = 40.0,
                     car = 8.0, ant = 0.0, brown = 0.0, ewt = 0.01,
                     lma = 0.008, prot = 0.0, cbc = 0.0, alpha = 40.0,
                     typelidf = 2, lidfa = nULL, lidfb = nULL, lai = nULL,
                     q = nULL, tts = nULL, tto = nULL, psi = nULL, rsoil = nULL,
                     fraction_brown = 0.0,  diss = 0.0,  cv = 1, zeta = 1,
                     sailversion = "4SAIL", brownvegetation = nULL) {

  ############################ #
  #	LEAF OPTICAL PROPERTIES	##
  ############################ #
  if (is.null(input_prospect)) {
    input_prospect <- data.frame("chl" = chl, "car" = car, "ant" = ant, "brown" = brown, "ewt" = ewt,
                                "lma" = lma, "prot" = prot, "cbc" = cbc, "n" = n, "alpha" = alpha)
  }
  greenvegetation <- prospect::PROSPECT(SpecPROSPECT = spec_sensor,
                                        n = input_prospect$n[1],
                                        chl = input_prospect$chl[1],
                                        car = input_prospect$car[1],
                                        ant = input_prospect$ant[1],
                                        brown = input_prospect$brown[1],
                                        ewt = input_prospect$ewt[1],
                                        lma = input_prospect$lma[1],
                                        prot = input_prospect$prot[1],
                                        cbc = input_prospect$cbc[1],
                                        alpha = input_prospect$alpha[1])

  if (sailversion  ==  "4SAIL2") {
    # 4SAIL2 requires one of the following combination of input parameters
    # Case #1: valid optical properties for brown vegetation
    if (!is.null(brownvegetation)) {
      # need to define reflectance and Transmittance for brownvegetation
      if (length(grep("reflectance", names(brownvegetation))) == 0 || length(grep("Transmittance", names(brownvegetation))) == 0) {
        message("Please define brownvegetation as a list including reflectance and Transmittance")
        stop()
      }
      # check if spectral domain for optical properties of brown vegetation match
      # with spectral domain for optical properties of green vegetation
      if (length(setdiff(spec_sensor$lambda, brownvegetation$wvl)) > 0) {
        message("Please define same spectral domain for brownvegetation and SpecPROSPECT")
        stop()
      }
      if (length(unique(lengths(input_prospect))) == 1) {
        if (!unique(lengths(input_prospect)) == 1) {
          message("brownvegetation defined along with multiple leaf chemical properties")
          message("Only first set of leaf chemical properties will be used to simulate green vegetation")
        }
      }
    # if no leaf optical properties brown vegetation defined
    } else if (is.null(brownvegetation)) {
      # if all PROSPECT input parameters have the same length
      if (length(unique(lengths(input_prospect))) == 1) {
        # if all PROSPECT input parameters are unique (no possibility to simulate 2 types of leaf optics)
        if (unique(lengths(input_prospect)) == 1) {
          # if fraction_brown set to 0,  then assign green vegetation optics to brown vegetation optics
          if (fraction_brown == 0) {
            brownvegetation <- greenvegetation
          # else run 4SAIL
          } else {
            message("4SAIL2 needs two sets of optical properties for green and brown vegetation")
            message("Currently one set is defined. will run 4SAIL instead of 4SAIL2")
            sailversion <- "4SAIL"
          }
        # if all PROSPECT parameters have at least 2 elements
        } else if (unique(lengths(input_prospect)) >= 2) {
          # compute leaf optical properties
          brownvegetation <- prospect::PROSPECT(SpecPROSPECT = spec_sensor,
                                                n = input_prospect$n[2],
                                                chl = input_prospect$chl[2],
                                                car = input_prospect$car[2],
                                                ant = input_prospect$ant[2],
                                                brown = input_prospect$brown[2],
                                                ewt = input_prospect$ewt[2],
                                                lma = input_prospect$lma[2],
                                                prot = input_prospect$prot[2],
                                                cbc = input_prospect$cbc[2],
                                                alpha = input_prospect$alpha[2])
          if (unique(lengths(input_prospect)) > 2) {
            message("4SAIL2 needs two sets of optical properties for green and brown vegetation")
            message("Currently more than 2 sets are defined. will only use the first 2")
          }
        }
      }
    }
  }
  if (sailversion  ==  "4SAIL") {
    if (length(unique(lengths(input_prospect))) == 1) {
      if (unique(lengths(input_prospect)) > 1) {
        message("4SAIL needs only one set of optical properties")
        message("Currently more than one set of leaf chemical constituents is defined.")
        message("Will run 4SAIL with the first set of leaf chemical constituents")
      }
    }
  }

  if (sailversion  ==  "4SAIL") {
    # run 4SAIL
    ref <- foursail(leafoptics = greenvegetation,
                    typelidf,  lidfa,  lidfb,  lai,  q,  tts,  tto,  psi,  rsoil)
  } else if (sailversion  ==  "4SAIL2") {
    # run 4SAIL2
    ref <- foursail2(leafgreen = greenvegetation,  leafbrown = brownvegetation,
                     typelidf,  lidfa,  lidfb,  lai,  q,  tts,  tto,  psi,  rsoil,
                     fraction_brown,  diss,  cv,  zeta)
  }
  return(ref)
}

#" Performs PROSAIL simulation based on a set of combinations of input parameters
#" @param leafoptics list. Includes leaf optical properties (reflectance and transmittance)
#" and corresponding spectral bands
#" @param typelidf numeric. Type of leaf inclination distribution function
#" @param lidfa numeric.
#" if typelidf  == 1,  controls the average leaf slope
#" if typelidf  == 2,  corresponds to average leaf angle
#" @param lidfb numeric.
#" if typelidf  == 1,  unused
#" if typelidf  == 2,  controls the distribution"s bimodality
#" @param lai numeric. Leaf Area Index
#" @param q numeric. Hot Spot parameter
#" @param tts numeric. Sun zeith angle
#" @param tto numeric. Observer zeith angle
#" @param psi numeric. Azimuth Sun  /  Observer
#" @param rsoil numeric. Soil reflectance
#"
#" @return list. rdot, rsot, rddt, rsdt
#" rdot: hemispherical-directional reflectance factor in viewing direction
#" rsot: bi-directional reflectance factor
#" rsdt: directional-hemispherical reflectance factor for solar incident flux
#" rddt: bi-hemispherical reflectance factor
#" @export

foursail  <- function(leafoptics,  typelidf = 2,  lidfa = nULL,  lidfb = nULL,  lai = nULL,
                      q = nULL,  tts = nULL,  tto = nULL,  psi = nULL,  rsoil = nULL) {

  ############################## #
  #	LEAF OPTICAL PROPERTIES	##
  ############################## #
  rho <- leafoptics$Reflectance
  tau <- leafoptics$Transmittance

  #	Geometric quantities
  rd <- pi / 180
  cts <- cos(rd * tts)
  cto <- cos(rd * tto)
  ctscto <- cts * cto
  tants <- tan(rd * tts)
  tanto <- tan(rd * tto)
  cospsi <- cos(rd * psi)
  dso <- sqrt(tants * tants + tanto * tanto - 2. * tants * tanto * cospsi)

  #	Generate leaf angle distribution from average leaf angle (ellipsoidal) or (a, b) parameters
  if (typelidf == 1) {
    foliar_distrib <- dladgen(lidfa, lidfb)
    lidf <- foliar_distrib$lidf
    litab <- foliar_distrib$litab

  } else if (typelidf == 2) {
    foliar_distrib <- campbell(lidfa)
    lidf <- foliar_distrib$lidf
    litab <- foliar_distrib$litab
  }

  # angular distance,  compensation of shadow length
  #	Calculate geometric factors associated with extinction and scattering
  #	Initialise sums
  ks <- 0
  ko <- 0
  bf <- 0
  sob <- 0
  sof <- 0

  #	Weighted sums over LIDF
  na <- length(litab)
  for (i in 1:na) {
    ttl <- litab[i]	    # leaf inclination discrete values
    ctl <- cos(rd * ttl)
    #	SAIL volume scattering phase function gives interception and portions to be
    #	multiplied by rho and tau
    resvolscatt <- volscatt(tts, tto, psi, ttl)
    chi_s <- resvolscatt$chi_s
    chi_o <- resvolscatt$chi_o
    frho <- resvolscatt$frho
    ftau <- resvolscatt$ftau

    #********************************************************************************
    #*                   SUITS SYSTEM coEFFICIEnTS
    #*
    #*	ks  : Extinction coefficient for direct solar flux
    #*	ko  : Extinction coefficient for direct observed flux
    #*	att : Attenuation coefficient for diffuse flux
    #*	sigb : Backscattering coefficient of the diffuse downward flux
    #*	sigf : Forwardscattering coefficient of the diffuse upward flux
    #*	sf  : Scattering coefficient of the direct solar flux for downward diffuse flux
    #*	sb  : Scattering coefficient of the direct solar flux for upward diffuse flux
    #*	vf   : Scattering coefficient of upward diffuse flux in the observed direction
    #*	vb   : Scattering coefficient of downward diffuse flux in the observed direction
    #*	w   : Bidirectional scattering coefficient
    #********************************************************************************

    #	Extinction coefficients
    ksli <- chi_s / cts
    koli <- chi_o / cto

    #	Area scattering coefficient fractions
    sobli <- frho * pi / ctscto
    sofli <- ftau * pi / ctscto
    bfli <- ctl * ctl
    ks <- ks + ksli * lidf[i]
    ko <- ko + koli * lidf[i]
    bf <- bf + bfli * lidf[i]
    sob <- sob + sobli * lidf[i]
    sof <- sof + sofli * lidf[i]
  }

  #	Geometric factors to be used later with rho and tau
  sdb <- 0.5 * (ks + bf)
  sdf <- 0.5 * (ks - bf)
  dob <- 0.5 * (ko + bf)
  dof <- 0.5 * (ko - bf)
  ddb <- 0.5 * (1. + bf)
  ddf <- 0.5 * (1. - bf)

  #	Here rho and tau come in
  sigb <- ddb * rho + ddf * tau
  sigf <- ddf * rho + ddb * tau
  att <- 1 - sigf
  m2 <- (att + sigb) * (att - sigb)
  m2[which(m2 <= 0)] <- 0
  m <- sqrt(m2)

  sb <- sdb * rho + sdf * tau
  sf <- sdf * rho + sdb * tau
  vb <- dob * rho + dof * tau
  vf <- dof * rho + dob * tau
  w <- sob * rho + sof * tau

  #	Here the LAI comes in
  #   Outputs for the case LAI = 0
  if (lai < 0) {
    tss <- 1
    too <- 1
    tsstoo <- 1
    rdd <- 0
    tdd <- 1
    rsd <- 0
    tsd <- 0
    rdo <- 0
    tdo <- 0
    rso <- 0
    rsos <- 0
    rsod <- 0

    rddt <- rsoil
    rsdt <- rsoil
    rdot <- rsoil
    rsodt <- 0 * rsoil
    rsost <- rsoil
    rsot <- rsoil
  } else {
    #	Other cases (LAI  >  0)
    e1 <- exp(-m * lai)
    e2 <- e1 * e1
    rinf <- (att - m) / sigb
    rinf2 <- rinf * rinf
    re <- rinf * e1
    denom <- 1. - rinf2 * e2

    j1ks <- jfunc1(ks, m, lai)
    j2ks <- jfunc2(ks, m, lai)
    j1ko <- jfunc1(ko, m, lai)
    j2ko <- jfunc2(ko, m, lai)

    ps <- (sf + sb * rinf) * j1ks
    qs <- (sf * rinf + sb) * j2ks
    pv <- (vf + vb * rinf) * j1ko
    qv <- (vf * rinf + vb) * j2ko

    rdd <- rinf * (1. - e2) / denom
    tdd <- (1. - rinf2) * e1 / denom
    tsd <- (ps - re * qs) / denom
    rsd <- (qs - re * ps) / denom
    tdo <- (pv - re * qv) / denom
    rdo <- (qv - re * pv) / denom

    tss <- exp(-ks * lai)
    too <- exp(-ko * lai)
    z <- jfunc3(ks, ko, lai)
    g1 <- (z - j1ks * too) / (ko + m)
    g2 <- (z - j1ko * tss) / (ks + m)

    tv1 <- (vf * rinf + vb) * g1
    tv2 <- (vf + vb * rinf) * g2
    t1 <- tv1 * (sf + sb * rinf)
    t2 <- tv2 * (sf * rinf + sb)
    t3 <- (rdo * qs + tdo * ps) * rinf

    #	Multiple scattering contribution to bidirectional canopy reflectance
    rsod <- (t1 + t2 - t3) / (1. - rinf2)

    #	Treatment of the hotspot-effect
    alf <- 1e6
    #	Apply correction 2 / (K + k) suggested by F.-M. Breon
    if (q > 0) {
      alf <- (dso / q) * 2. / (ks + ko)
    }
    if (alf > 200) {
      # inserted H. Bach 1 / 3 / 04
      alf <- 200
    }
    if (alf == 0) {
      #	The pure hotspot - no shadow
      tsstoo <- tss
      sumint <- (1 - tss) / (ks * lai)
    } else {
      #	Outside the hotspot
      fhot <- lai * sqrt(ko * ks)
      #	Integrate by exponential Simpson method in 20 steps
      #	the steps are arranged according to equal partitioning
      #	of the slope of the joint probability function
      x1 <- 0
      y1 <- 0
      f1 <- 1
      fint <- (1. - exp(-alf)) * 0.05
      sumint <- 0
      for (i in 1:20) {
        if (i < 20) {
          x2 <- -log(1. - i * fint) / alf
        } else {
          x2 <- 1
        }
        y2 <- -(ko + ks) * lai * x2 + fhot * (1. - exp(-alf * x2)) / alf
        f2 <- exp(y2)
        sumint <- sumint + (f2 - f1) * (x2 - x1) / (y2 - y1)
        x1 <- x2
        y1 <- y2
        f1 <- f2
      }
      tsstoo <- f1
    }
    #	Bidirectional reflectance
    #	Single scattering contribution
    rsos <- w * lai * sumint
    #	Total canopy contribution
    rso <- rsos + rsod
    #	Interaction with the soil
    dn <- 1. - rsoil * rdd
    # rddt: bi-hemispherical reflectance factor
    rddt <- rdd + tdd * rsoil * tdd / dn
    # rsdt: directional-hemispherical reflectance factor for solar incident flux
    rsdt <- rsd + (tsd + tss) * rsoil * tdd / dn
    # rdot: hemispherical-directional reflectance factor in viewing direction
    rdot <- rdo + tdd * rsoil * (tdo + too) / dn
    # rsot: bi-directional reflectance factor
    rsodt <- rsod + ((tss + tsd) * tdo + (tsd + tss * rsoil * rdd) * too) * rsoil / dn
    rsost <- rsos + tsstoo * rsoil
    rsot <- rsost + rsodt
  }
  my_list <- list("rdot" = rdot, "rsot" = rsot, "rddt" = rddt, "rsdt" = rsdt)
  return(my_list)
}

#" Performs pro4sail2 simulation based on a set of combinations of input parameters
#" @param leafgreen list. includes relfectance and transmittance for vegetation #1 (e.g. green vegetation)
#" @param leafbrown list. includes relfectance and transmittance for vegetation #2 (e.g. brown vegetation)
#" @param typelidf numeric. Type of leaf inclination distribution function
#" @param lidfa numeric.
#" if typelidf  == 1,  controls the average leaf slope
#" if typelidf  == 2,  corresponds to average leaf angle
#" @param lidfb numeric.
#" if typelidf  == 1,  unused
#" if typelidf  == 2,  controls the distribution"s bimodality
#" @param lai numeric. Leaf Area Index
#" @param hot numeric. Hot Spot parameter = ratio of the correlation length of leaf projections in the horizontal plane and the canopy height (doi:10.1016 / j.rse.2006.12.013)
#" @param tts numeric. Sun zeith angle
#" @param tto numeric. Observer zeith angle
#" @param psi numeric. Azimuth Sun  /  Observer
#" @param rsoil numeric. Soil reflectance
#" @param fraction_brown numeric. Fraction of brown leaf area
#" @param diss numeric. Layer dissociation factor
#" @param cv numeric. vertical crown cover percentage
#" = % ground area covered with crowns as seen from nadir direction
#" @param zeta numeric. Tree shape factor
#" = ratio of crown diameter to crown height
#"
#" @return list. rdot, rsot, rddt, rsdt
#" rdot: hemispherical-directional reflectance factor in viewing direction
#" rsot: bi-directional reflectance factor
#" rsdt: directional-hemispherical reflectance factor for solar incident flux
#" rddt: bi-hemispherical reflectance factor
#" alfast: canopy absorptance for direct solar incident flux
#" alfadt: canopy absorptance for hemispherical diffuse incident flux
#" @export

foursail2  <- function(leafgreen,  leafbrown,
                       typelidf = 2, lidfa = nULL, lidfb = nULL,
                       lai = nULL,  hot = nULL, tts = nULL, tto = nULL, psi = nULL, rsoil = nULL,
                       fraction_brown = 0.5,  diss = 0.5,  cv = 1, zeta = 1) {

  #	This version does not include non-Lambertian soil properties.
  #	original codes do,  and only need to add the following variables as input
  rddsoil <- rdosoil <- rsdsoil <- rsosoil <- rsoil

  #	Geometric quantities
  rd <- pi / 180

  #	Generate leaf angle distribution from average leaf angle (ellipsoidal) or (a, b) parameters
  if (typelidf == 1) {
    foliar_distrib <- dladgen(lidfa, lidfb)
    lidf <- foliar_distrib$lidf
    litab <- foliar_distrib$litab

  } else if (typelidf == 2) {
    foliar_distrib <- campbell(lidfa)
    lidf <- foliar_distrib$lidf
    litab <- foliar_distrib$litab
  }

  if (lai < 0) {
    message("Please define positive LAI value")
    rddt <- rsdt <- rdot <- rsost <- rsot <- rsoil
    alfast <- alfadt <- 0 * rsoil
  } else if (lai == 0) {
    tss <- too <- tsstoo <- tdd <- 1.0
    rdd <- rsd <- tsd <- rdo <- tdo <- 0.0
    rso <- rsos <- rsod <- rsodt <- 0.0
    rddt <- rsdt <- rdot <- rsost <- rsot <- rsoil
    alfast <- alfadt <- 0 * rsoil
  } else if (lai > 0) {
    cts <- cos(rd * tts)
    cto <- cos(rd * tto)
    ctscto <- cts * cto
    tants <- tan(rd * tts)
    tanto <- tan(rd * tto)
    cospsi <- cos(rd * psi)
    dso <- sqrt(tants * tants + tanto * tanto - 2.0 * tants * tanto * cospsi)

    # Clumping effects
    cs <- co <- 1.0
    if (cv <= 1.0) {
      cs <- 1.0 - (1.0 - cv)^(1.0 / cts)
      co <- 1.0 - (1.0 - cv)^(1.0 / cto)
    }
    overlap <- 0.0
    if (zeta > 0.0) {
      overlap <- min(cs * (1.0 - co), co * (1.0 - cs)) * exp(-dso / zeta)
    }
    fcd <- cs * co + overlap
    fcs <- (1.0 - cs) * co - overlap
    fod <- cs * (1.0 - co) - overlap
    fos <- (1.0 - cs) * (1.0 - co) + overlap
    fcdc <- 1.0 - (1.0 - fcd)^(0.5 / cts + 0.5 / cto)

    #	Part depending on diss,  fraction_brown,  and leaf optical properties
    #	First save the input fraction_brown as the old fraction_brown,  as the following change is only artificial
    # Better define an fraction_brown that is actually used: fb,  so that the input is not modified!

    fb <- fraction_brown
    # if only green leaves
    if (fraction_brown == 0.0) {
      fb <- 0.5
      leafbrown$Reflectance <- leafgreen$Reflectance
      leafbrown$Transmittance <- leafgreen$Transmittance
    }
    if (fraction_brown == 1.0) {
      fb <- 0.5
      leafgreen$Reflectance <- leafbrown$Reflectance
      leafgreen$Transmittance <- leafbrown$Transmittance
    }
    s <- (1.0 - diss) * fb * (1.0 - fb)
    # rho1 && tau1 : green foliage
    # rho2 && tau2 : brown foliage (bottom layer)
    rho1 <- ((1 - fb - s) * leafgreen$Reflectance + s * leafbrown$Reflectance) / (1 - fb)
    tau1 <- ((1 - fb - s) * leafgreen$Transmittance + s * leafbrown$Transmittance) / (1 - fb)
    rho2 <- (s * leafgreen$Reflectance + (fb - s) * leafbrown$Reflectance) / fb
    tau2 <- (s * leafgreen$Transmittance + (fb - s) * leafbrown$Transmittance) / fb

    # angular distance,  compensation of shadow length
    #	Calculate geometric factors associated with extinction and scattering
    #	Initialise sums
    ks <- ko <- bf <- sob <- sof <- 0

    # Weighted sums over LIDF

    for (i in 1:seq_along(litab)) {
      ttl <- litab[i]
      ctl <- cos(rd * ttl)
      # SAIL volscatt function gives interception coefficients
      # and two portions of the volume scattering phase function to be
      # multiplied by rho and tau,  respectively
      resvolscatt <- volscatt(tts, tto, psi, ttl)
      chi_s <- resvolscatt$chi_s
      chi_o <- resvolscatt$chi_o
      frho <- resvolscatt$frho
      ftau <- resvolscatt$ftau
      # Extinction coefficients
      ksli <- chi_s / cts
      koli <- chi_o / cto
      # Area scattering coefficient fractions
      sobli <- frho * pi / ctscto
      sofli <- ftau * pi / ctscto
      bfli <- ctl * ctl
      ks <- ks + ksli * lidf[i]
      ko <- ko + koli * lidf[i]
      bf <- bf + bfli * lidf[i]
      sob <- sob + sobli * lidf[i]
      sof <- sof + sofli * lidf[i]
    }
    # Geometric factors to be used later in combination with rho and tau
    sdb <- 0.5 * (ks + bf)
    sdf <- 0.5 * (ks - bf)
    dob <- 0.5 * (ko + bf)
    dof <- 0.5 * (ko - bf)
    ddb <- 0.5 * (1. + bf)
    ddf <- 0.5 * (1. - bf)

    # LAIs in two layers
    lai1 <- (1 - fb) * lai
    lai2 <- fb * lai

    tss <- exp(-ks * lai)
    ck <- exp(-ks * lai1)
    alf <- 1e6
    if (hot > 0.0) {
      alf <- (dso / hot) * 2.0 / (ks + ko)
    }
    if (alf > 200.0) {
      alf <- 200.0     # inserted H. Bach 1 / 3 / 04
    }
    if (alf == 0.0) {
      # The pure hotspot
      tsstoo <- tss
      s1 <- (1 - ck) / (ks * lai)
      s2 <- (ck - tss) / (ks * lai)
    } else {
      # Outside the hotspot
      fhot <- lai * sqrt(ko * ks)
      # Integrate 2 layers by exponential simpson method in 20 steps
      # the steps are arranged according to equal partitioning
      # of the derivative of the joint probability function
      x1 <- y1 <- 0.0
      f1 <- 1.0
      ca <- exp(alf * (fb - 1.0))
      fint <- (1.0 - ca) * .05
      s1 <- 0.0
      for (istep in 1:20) {
        if (istep < 20) {
          x2 <- -log(1. - istep * fint) / alf
        } else {
          x2 <- 1. - fb
        }
        y2 <- -(ko + ks) * lai * x2 + fhot * (1.0 - exp(-alf * x2)) / alf
        f2 <- exp(y2)
        s1 <- s1 + (f2 - f1) * (x2 - x1) / (y2 - y1)
        x1 <- x2
        y1 <- y2
        f1 <- f2
      }
      fint <- (ca - exp(-alf)) * .05
      s2 <- 0.0
      for (istep in 1:20) {
        if (istep < 20) {
          x2 <- -log(ca - istep * fint) / alf
        } else {
          x2 <- 1.0
        }
        y2 <- -(ko + ks) * lai * x2 + fhot * (1.0 - exp(-alf * x2)) / alf
        f2 <- exp(y2)
        s2 <- s2 + (f2 - f1) * (x2 - x1) / (y2 - y1)
        x1 <- x2
        y1 <- y2
        f1 <- f2
      }
      tsstoo <- f1
    }

    # Calculate reflectances and transmittances
    # Bottom layer
    tss <- exp(-ks * lai2)
    too <- exp(-ko * lai2)
    sb <- sdb * rho2 + sdf * tau2
    sf <- sdf * rho2 + sdb * tau2

    vb <- dob * rho2 + dof * tau2
    vf <- dof * rho2 + dob * tau2

    w2 <- sob * rho2 + sof * tau2

    sigb <- ddb * rho2 + ddf * tau2
    sigf <- ddf * rho2 + ddb * tau2
    att <- 1.0 - sigf
    m2 <- (att + sigb) * (att - sigb)
    m2[m2 < 0] <- 0
    m <- sqrt(m2)
    which_ncs <- which(m > 0.01)
    which_cs <- which(m <= 0.01)

    tdd <- rdd <- tsd <- rsd <- tdo <- rdo <- 0 * m
    rsod <- 0 * m
    if (length(which_ncs) > 0) {
      resncs <- nonconservativescattering(m[which_ncs], lai2, att[which_ncs], sigb[which_ncs],
                                          ks, ko, sf[which_ncs], sb[which_ncs], vf[which_ncs], vb[which_ncs], tss, too)
      tdd[which_ncs] <- resncs$tdd
      rdd[which_ncs] <- resncs$rdd
      tsd[which_ncs] <- resncs$tsd
      rsd[which_ncs] <- resncs$rsd
      tdo[which_ncs] <- resncs$tdo
      rdo[which_ncs] <- resncs$rdo
      rsod[which_ncs] <- resncs$rsod
    }
    if (length(which_cs) > 0) {
      rescs <- conservativescattering(m[which_cs], lai2, att[which_cs], sigb[which_cs],
                                      ks, ko, sf[which_cs], sb[which_cs], vf[which_cs], vb[which_cs], tss, too)
      tdd[which_cs] <- rescs$tdd
      rdd[which_cs] <- rescs$rdd
      tsd[which_cs] <- rescs$tsd
      rsd[which_cs] <- rescs$rsd
      tdo[which_cs] <- rescs$tdo
      rdo[which_cs] <- rescs$rdo
      rsod[which_cs] <- rescs$rsod
    }

    # Set background properties equal to those of the bottom layer on a black soil
    rddb <- rdd
    rsdb <- rsd
    rdob <- rdo
    rsodb <- rsod
    tddb <- tdd
    tsdb <- tsd
    tdob <- tdo
    toob <- too
    tssb <- tss
    # Top layer
    tss <- exp(-ks * lai1)
    too <- exp(-ko * lai1)

    sb <- sdb * rho1 + sdf * tau1
    sf <- sdf * rho1 + sdb * tau1

    vb <- dob * rho1 + dof * tau1
    vf <- dof * rho1 + dob * tau1

    w1 <- sob * rho1 + sof * tau1

    sigb <- ddb * rho1 + ddf * tau1
    sigf <- ddf * rho1 + ddb * tau1
    att <- 1.0 - sigf

    m2 <- (att + sigb) * (att - sigb)
    m2[m2 < 0] <- 0
    m <- sqrt(m2)
    which_ncs <- which(m > 0.01)
    which_cs <- which(m <= 0.01)

    tdd <- rdd <- tsd <- rsd <- tdo <- rdo <- 0 * m
    rsod <- 0 * m
    if (length(which_ncs) > 0) {
      resncs <- nonconservativescattering(m[which_ncs], lai1, att[which_ncs], sigb[which_ncs],
                                          ks, ko, sf[which_ncs], sb[which_ncs], vf[which_ncs], vb[which_ncs], tss, too)
      tdd[which_ncs] <- resncs$tdd
      rdd[which_ncs] <- resncs$rdd
      tsd[which_ncs] <- resncs$tsd
      rsd[which_ncs] <- resncs$rsd
      tdo[which_ncs] <- resncs$tdo
      rdo[which_ncs] <- resncs$rdo
      rsod[which_ncs] <- resncs$rsod
    }
    if (length(which_cs) > 0) {
      rescs <- conservativescattering(m[which_cs], lai1, att[which_cs], sigb[which_cs],
                                      ks, ko, sf[which_cs], sb[which_cs], vf[which_cs], vb[which_cs], tss, too)
      tdd[which_cs] <- rescs$tdd
      rdd[which_cs] <- rescs$rdd
      tsd[which_cs] <- rescs$tsd
      rsd[which_cs] <- rescs$rsd
      tdo[which_cs] <- rescs$tdo
      rdo[which_cs] <- rescs$rdo
      rsod[which_cs] <- rescs$rsod
    }

    # combine with bottom layer reflectances and transmittances (adding method)
    rn <- 1.0 - rdd * rddb
    tup <- (tss * rsdb + tsd * rddb) / rn
    tdn <- (tsd + tss * rsdb * rdd) / rn
    rsdt <- rsd + tup * tdd
    rdot <- rdo + tdd * (rddb * tdo + rdob * too) / rn
    rsodt <- rsod + (tss * rsodb + tdn * rdob) * too + tup * tdo

    rsost <- (w1 * s1 + w2 * s2) * lai

    rsot <- rsost + rsodt

    # Diffuse reflectances at the top and the bottom are now different
    rddt_t <- rdd + tdd * rddb * tdd / rn
    rddt_b <- rddb + tddb * rdd * tddb / rn

    # Transmittances of the combined canopy layers
    tsst <- tss * tssb
    toot <- too * toob
    tsdt <- tss * tsdb + tdn * tddb
    tdot <- tdob * too + tddb * (tdo + rdd * rdob * too) / rn
    tddt <- tdd * tddb / rn

    # Apply clumping effects to vegetation layer
    rddcb <- cv * rddt_b
    rddct <- cv * rddt_t
    tddc <- 1 - cv + cv * tddt
    rsdc <- cs * rsdt
    tsdc <- cs * tsdt
    rdoc <- co * rdot
    tdoc <- co * tdot
    tssc <- 1 - cs + cs * tsst
    tooc <- 1 - co + co * toot

    # new weight function fcdc for crown contribution (W. Verhoef,  22-05-08)
    rsoc <- fcdc * rsot
    tssooc <- fcd * tsstoo + fcs * toot + fod * tsst + fos
    # Canopy absorptance for black background (W. Verhoef,  02-03-04)
    alfas <- 1. - tssc - tsdc - rsdc
    alfad <- 1. - tddc - rddct
    # Add the soil background
    rn <- 1 - rddcb * rddsoil
    tup <- (tssc * rsdsoil + tsdc * rddsoil) / rn
    tdn <- (tsdc + tssc * rsdsoil * rddcb) / rn

    rddt <- rddct + tddc * rddsoil * tddc / rn
    rsdt <- rsdc + tup * tddc
    rdot <- rdoc + tddc * (rddsoil * tdoc + rdosoil * tooc) / rn
    rsot <- rsoc + tssooc * rsosoil + tdn * rdosoil * tooc + tup * tdoc

    # Effect of soil background on canopy absorptances (W. Verhoef,  02-03-04)
    alfast <- alfas + tup * alfad
    alfadt <- alfad * (1. + tddc * rddsoil / rn)
  }
  my_list <- list("rdot" = rdot, "rsot" = rsot, "rddt" = rddt, "rsdt" = rsdt,
                  "alfast" = alfast,  "alfadt" = alfadt)
  return(my_list)
}



#" computes non conservative scattering conditions
#" @param m numeric.
#" @param lai numeric. Leaf Area Index
#" @param att numeric.
#" @param sigb numeric.
#" @param ks numeric.
#" @param ko numeric.
#" @param sf numeric.
#" @param sb numeric.
#" @param vf numeric.
#" @param vb numeric.
#" @param tss numeric.
#" @param too numeric.
#"
#" @return list. tdd,  rdd,  tsd,  rsd,  tdo,  rdo,  rsod
#"
#" @export
nonconservativescattering <- function(m, lai, att, sigb, ks, ko, sf, sb, vf, vb, tss, too) {

  e1 <- exp(-m * lai)
  e2 <- e1 * e1
  rinf <- (att - m) / sigb
  rinf2 <- rinf * rinf
  re <- rinf * e1
  denom <- 1. - rinf2 * e2

  j1ks <- jfunc1(ks, m, lai)
  j2ks <- jfunc2(ks, m, lai)
  j1ko <- jfunc1(ko, m, lai)
  j2ko <- jfunc2(ko, m, lai)

  ps <- (sf + sb * rinf) * j1ks
  qs <- (sf * rinf + sb) * j2ks
  pv <- (vf + vb * rinf) * j1ko
  qv <- (vf * rinf + vb) * j2ko

  tdd <- (1. - rinf2) * e1 / denom
  rdd <- rinf * (1. - e2) / denom
  tsd <- (ps - re * qs) / denom
  rsd <- (qs - re * ps) / denom
  tdo <- (pv - re * qv) / denom
  rdo <- (qv - re * pv) / denom

  z <- jfunc2(ks, ko, lai)
  g1 <- (z - j1ks * too) / (ko + m)
  g2 <- (z - j1ko * tss) / (ks + m)

  tv1 <- (vf * rinf + vb) * g1
  tv2 <- (vf + vb * rinf) * g2

  t1 <- tv1 * (sf + sb * rinf)
  t2 <- tv2 * (sf * rinf + sb)
  t3 <- (rdo * qs + tdo * ps) * rinf

  # Multiple scattering contribution to bidirectional canopy reflectance
  rsod <- (t1 + t2 - t3) / (1. - rinf2)
  my_list <- list("tdd" = tdd,  "rdd" = rdd,  "tsd" = tsd,
                  "rsd" = rsd,  "tdo" = tdo,  "rdo" = rdo,
                  "rsod" = rsod)
  return(my_list)
}

#" computes conservative scattering conditions
#" @param m numeric.
#" @param lai numeric. Leaf Area Index
#" @param att numeric.
#" @param sigb numeric.
#" @param ks numeric.
#" @param ko numeric.
#" @param sf numeric.
#" @param sb numeric.
#" @param vf numeric.
#" @param vb numeric.
#" @param tss numeric.
#" @param too numeric.
#"
#" @return list. tdd,  rdd,  tsd,  rsd,  tdo,  rdo,  rsod
#"
#" @export
conservativescattering <- function(m, lai, att, sigb, ks, ko, sf, sb, vf, vb, tss, too) {

  # near or complete conservative scattering
  j4 <- jfunc4(m, lai)
  amsig <- att - sigb
  apsig <- att + sigb
  rtp <- (1 - amsig * j4) / (1 + amsig * j4)
  rtm <- (-1 + apsig * j4) / (1 + apsig * j4)
  rdd <- 0.5 * (rtp + rtm)
  tdd <- 0.5 * (rtp - rtm)

  dns <- ks * ks - m * m
  dno <- ko * ko - m * m
  cks <- (sb * (ks - att) - sf * sigb) / dns
  cko <- (vb * (ko - att) - vf * sigb) / dno
  dks <- (-sf * (ks + att) - sb * sigb) / dns
  dko <- (-vf * (ko + att) - vb * sigb) / dno
  ho <- (sf * cko + sb * dko) / (ko + ks)

  rsd <- cks * (1 - tss * tdd) - dks * rdd
  rdo <- cko * (1 - too * tdd) - dko * rdd
  tsd <- dks * (tss - tdd) - cks * tss * rdd
  tdo <- dko * (too - tdd) - cko * too * rdd
  # Multiple scattering contribution to bidirectional canopy reflectance
  rsod <- ho * (1 - tss * too) - cko * tsd * too - dko * rsd

  my_list <- list("tdd" = tdd,  "rdd" = rdd,  "tsd" = tsd,
                  "rsd" = rsd,  "tdo" = tdo,  "rdo" = rdo,
                  "rsod" = rsod)
  return(my_list)
}






#" computes the leaf angle distribution function value (freq)
#"
#" Ellipsoidal distribution function characterised by the average leaf
#" inclination angle in degree (ala)
#" Campbell 1986
#" @param ala average leaf angle
#" @return foliar_distrib list. lidf and litab
#" @export
campbell  <- function(ala) {

  tx1 <- c(10., 20., 30., 40., 50., 60., 70., 80., 82., 84., 86., 88., 90.)
  tx2 <- c(0., 10., 20., 30., 40., 50., 60., 70., 80., 82., 84., 86., 88.)
  litab <- (tx2 + tx1) / 2
  n <- length(litab)
  tl1 <- tx1 * (pi / 180)
  tl2 <- tx2 * (pi / 180)
  excent <- exp(-1.6184e-5 * ala**3 + 2.1145e-3 * ala**2 - 1.2390e-1 * ala + 3.2491)
  sum0 <- 0

  freq <- c()
  for (i in 1:n) {
    x1 <- excent / (sqrt(1. + excent**2. * tan(tl1[i])**2))
    x2 <- excent / (sqrt(1. + excent**2. * tan(tl2[i])**2))
    if (excent == 1) {
      freq[i] <- abs(cos(tl1[i]) - cos(tl2[i]))
    } else {
      alpha <- excent / sqrt(abs(1 - excent**2))
      alpha2 <- alpha**2
      x12 <- x1**2
      x22 <- x2**2
      alpx1 <- 0 * alpha2
      alpx2 <- 0 * alpha2
      almx1 <- 0 * alpha2
      almx2 <- 0 * alpha2
      if (excent > 1) {
        alpx1 <- sqrt(alpha2[excent > 1] + x12[excent > 1])
        alpx2[excent > 1] <- sqrt(alpha2[excent > 1] + x22[excent > 1])
        dum <- x1 * alpx1 + alpha2 * log(x1 + alpx1)
        freq[i] <- abs(dum - (x2 * alpx2 + alpha2 * log(x2 + alpx2)))
      } else {
        almx1 <- sqrt(alpha2 - x12)
        almx2 <- sqrt(alpha2 - x22)
        dum <- x1 * almx1 + alpha2 * asin(x1 / alpha)
        freq[i] <- abs(dum - (x2 * almx2 + alpha2 * asin(x2 / alpha)))
      }
    }
  }
  sum0 <- sum(freq)
  freq0 <- freq / sum0
  foliar_distrib <- list("lidf" = freq0, "litab" = litab)
  return(foliar_distrib)
}

#" computes the leaf angle distribution function value (freq)
#"
#" Using the original bimodal distribution function initially proposed in SAIL
#"  references
#"  ----------
#"  (Verhoef1998) Verhoef,  Wout. Theory of radiative transfer models applied
#"  in optical remote sensing of vegetation canopies.
#"  nationaal Lucht en Ruimtevaartlaboratorium,  1998.
#"  http: /  / library.wur.nl / WebQuery / clc / 945481.
#" @param a controls the average leaf slope
#" @param b controls the distribution"s bimodality
#" LIDF type 		  a 		b
#" Planophile 	  1		  0
#" Erectophile    -1	 	0
#" Plagiophile 	  0		  -1
#" Extremophile 	0		  1
#" Spherical 	    -0.35 -0.15
#" Uniform        0     0
#" requirement: ||lidfa||  +  ||lidfb|| < 1
#"
#" @return foliar_distrib list. lidf and litab
#" @export
dladgen  <- function(a, b) {
  litab <- c(5., 15., 25., 35., 45., 55., 65., 75., 81., 83., 85., 87., 89.)
  freq <- c()
  for (i1 in 1:8) {
    t <- i1 * 10
    freq[i1] <- dcum(a, b, t)
  }
  for (i2 in 9:12) {
    t <- 80. + (i2 - 8) * 2.
    freq[i2] <- dcum(a, b, t)
  }
  freq[13] <- 1
  for (i in 13:2) {
    freq[i] <- freq[i] - freq[i - 1]
  }
  foliar_distrib <- list("lidf" = freq, "litab" = litab)
  return(foliar_distrib)
}

#" dcum function
#" @param a numeric. controls the average leaf slope
#" @param b numeric. controls the distribution"s bimodality
#" @param t numeric. angle
#" @return f
#" @export
dcum <- function(a, b, t) {
  rd <- pi / 180
  if (a >= 1) {
    f <- 1 - cos(rd * t)
  } else {
    eps <- 1e-8
    delx <- 1
    x <- 2 * rd * t
    p <- x
    while (delx  >= eps) {
      y <- a * sin(x) + .5 * b * sin(2. * x)
      dx <- .5 * (y - x + p)
      x <- x + dx
      delx <- abs(dx)
    }
    f <- (2. * y + p) / pi
  }
  return(f)
}

#" J1 function with avoidance of singularity problem
#"
#" @param k numeric. Extinction coefficient for direct (solar or observer) flux
#" @param l numeric.
#" @param t numeric. Leaf Area Index
#" @return jout numeric.
#" @export
jfunc1 <- function(k, l, t) {
  # J1 function with avoidance of singularity problem
  del <- (k - l) * t
  jout <- 0 * l
  jout[which(abs(del) > 1e-3)] <- (exp(-l[which(abs(del) > 1e-3)] * t) - exp(-k * t)) / (k - l[which(abs(del) > 1e-3)])
  jout[which(abs(del) <= 1e-3)] <- 0.5 * t * (exp(-k * t) + exp(-l[which(abs(del) <= 1e-3)] * t)) * (1 - del[which(abs(del) <= 1e-3)] * del[which(abs(del) <= 1e-3)] / 12)
  return(jout)
}

#" J2 function with avoidance of singularity problem
#"
#" @param k numeric. Extinction coefficient for direct (solar or observer) flux
#" @param l numeric.
#" @param t numeric. Leaf Area Index
#" @return jout numeric.
#" @export
jfunc2 <- function(k, l, t) {
  #	J2 function
  jout <- (1. - exp(-(k + l) * t)) / (k + l)
  return(jout)
}

#" J3 function with avoidance of singularity problem
#"
#" @param k numeric. Extinction coefficient for direct (solar or observer) flux
#" @param l numeric.
#" @param t numeric. Leaf Area Index
#" @return jout numeric.
#" @export
jfunc3 <- function(k, l, t) {
  out <- (1. - exp(-(k + l) * t)) / (k + l)
  return(out)
}


#" j4 function for treating (near) conservative scattering
#"
#" @param m numeric. Extinction coefficient for direct (solar or observer) flux
#" @param t numeric. Leaf Area Index
#" @return jout numeric.
#" @export
jfunc4 <- function(m, t) {

  del <- m * t
  out <- 0 * del
  out[del > 1e-3] <- (1 - exp(-del)) / (m * (1 + exp(-del)))
  out[del <= 1e-3] <- 0.5 * t * (1. - del * del / 12.)
  return(out)
}


#" compute volume scattering functions and interception coefficients
#" for given solar zenith,  viewing zenith,  azimuth and leaf inclination angle.
#"
#" @param tts numeric. solar zenith
#" @param tto numeric. viewing zenith
#" @param psi numeric. azimuth
#" @param ttl numeric. leaf inclination angle
#" @return res list. includes chi_s,  chi_o,  frho,  ftau
#" @export
volscatt  <- function(tts, tto, psi, ttl) {
  #********************************************************************************
  #*	chi_s	= interception functions
  #*	chi_o	= interception functions
  #*	frho	= function to be multiplied by leaf reflectance rho
  #*	ftau	= functions to be multiplied by leaf transmittance tau
  #********************************************************************************
  #	Wout Verhoef,  april 2001,  for CROMA

  rd <- pi / 180
  costs <- cos(rd * tts)
  costo <- cos(rd * tto)
  sints <- sin(rd * tts)
  sinto <- sin(rd * tto)
  cospsi <- cos(rd * psi)
  psir <- rd * psi
  costl <- cos(rd * ttl)
  sintl <- sin(rd * ttl)
  cs <- costl * costs
  co <- costl * costo
  ss <- sintl * sints
  so <- sintl * sinto

  #c ..............................................................................
  #c     betas -bts- and betao -bto- computation
  #c     Transition angles (beta) for solar (betas) and view (betao) directions
  #c     if thetav + thetal > pi / 2,  bottom side of the leaves is observed for leaf azimut
  #c     interval betao + phi<leaf azimut<2pi-betao + phi.
  #c     if thetav + thetal<pi / 2,  top side of the leaves is always observed,  betao=pi
  #c     same consideration for solar direction to compute betas
  #c ..............................................................................

  cosbts <- 5
  if (abs(ss) > 1e-6) {
    cosbts <- -cs / ss
  }
  cosbto <- 5
  if (abs(so) > 1e-6) {
    cosbto <- -co / so
  }

  if (abs(cosbts) < 1) {
    bts <- acos(cosbts)
    ds <- ss
  } else {
    bts <- pi
    ds <- cs
  }
  chi_s <- 2. / pi * ((bts - pi * .5) * cs + sin(bts) * ss)
  if (abs(cosbto) < 1) {
    bto <- acos(cosbto)
    doo <- so
  } else if (tto < 90) {
    bto <- pi
    doo <- co
  } else {
    bto <- 0
    doo <- -co
  }
  chi_o <- 2. / pi * ((bto - pi * .5) * co + sin(bto) * so)

  #c ..............................................................................
  #c   computation of auxiliary azimut angles bt1,  bt2,  bt3 used
  #c   for the computation of the bidirectional scattering coefficient w
  #c .............................................................................

  btran1 <- abs(bts - bto)
  btran2 <- pi - abs(bts + bto - pi)

  if (psir <= btran1) {
    bt1 <- psir
    bt2 <- btran1
    bt3 <- btran2
  } else {
    bt1 <- btran1
    if (psir <= btran2) {
      bt2 <- psir
      bt3 <- btran2
    } else {
      bt2 <- btran2
      bt3 <- psir
    }
  }
  t1 <- 2. * cs * co + ss * so * cospsi
  t2 <- 0
  if (bt2 > 0) {
    t2 <- sin(bt2) * (2. * ds * doo + ss * so * cos(bt1) * cos(bt3))
  }

  denom <- 2. * pi * pi
  frho <- ((pi - bt2) * t1 + t2) / denom
  ftau <- (-bt2 * t1 + t2) / denom

  if (frho < 0) {
    frho <- 0
  }
  if (ftau < 0) {
    ftau <- 0
  }
  res <- list("chi_s" = chi_s, "chi_o" = chi_o, "frho" = frho, "ftau" = ftau)
  return(res)
}
