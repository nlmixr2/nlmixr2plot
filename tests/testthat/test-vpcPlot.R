# vpc and tidyvpc build their figures in frames that bind their whole VPC
# object, the simulation included, and a saved figure writes out every frame
# it keeps.  vpcPlot() hands them only what their plots read; these tests pin
# that the figures do not change and that they no longer grow with the
# simulation.

# What a figure draws: the built layers, panels, labels, scales, facet and
# theme
.vpcFigureBuild <- function(fig) {
  .b <- ggplot2::ggplot_build(fig)
  list(
    data = .b$data,
    layout = .b$layout$layout,
    labels = .b$plot$labels,
    geoms = vapply(fig$layers, function(l) class(l$geom)[1], character(1)),
    aesParams = lapply(fig$layers, function(l) l$aes_params),
    scales = vapply(
      .b$plot$scales$scales,
      function(s) paste(class(s)[1], paste(s$aesthetics, collapse = "/")),
      character(1)
    ),
    facet = class(fig$facet)[1],
    facetVars = fig$facet$vars(),
    theme = fig$theme
  )
}

# vpc's example data, observations only, with `nSim` simulated replicates
.vpcExampleData <- function(nSim) {
  .obs <- vpc::simple_data$obs
  .sim <- vpc::simple_data$sim
  list(
    obs = .obs[.obs$MDV == 0, ],
    sim = .sim[.sim$MDV == 0 & .sim$REP <= nSim, ]
  )
}

# tidyvpc's example data, observations only, with `nSim` simulated replicates;
# the observed data gets the population predictions of the first replicate,
# as tidyvpc's examples do for predcorrect()
.tidyvpcExampleData <- function(nSim) {
  .obs <- as.data.frame(tidyvpc::obs_data)
  .sim <- as.data.frame(tidyvpc::sim_data)
  .obs$PRED <- .sim$PRED[.sim$REP == 1]
  list(
    obs = .obs[.obs$MDV == 0, ],
    sim = .sim[.sim$MDV == 0 & .sim$REP <= nSim, ]
  )
}

test_that(".vpcDbFigure() draws what vpc draws", {
  skip_if_not_installed("vpc")
  .d <- .vpcExampleData(20)
  # arguments of vpc::vpc_vpc(); smooth, log_y, title and vpc_theme are also
  # what vpc_vpc() passes to vpc::plot_vpc()
  .cases <- list(
    default = list(smooth = TRUE, log_y = FALSE, title = NULL, vpc_theme = NULL),
    observed_points = list(
      stratify = "ISM",
      show = list(obs_dv = TRUE),
      smooth = FALSE,
      log_y = TRUE,
      title = "VPC",
      xlab = "Time (h)",
      ylab = "Concentration",
      vpc_theme = vpc::new_vpc_theme(list(sim_pi_fill = "#aa3377", obs_color = "#0077bb"))
    ),
    loq_columns = list(
      stratify = "ISM",
      facet = "columns",
      lloq = 5,
      show = list(pi_as_area = TRUE),
      smooth = TRUE,
      log_y = FALSE,
      title = NULL,
      vpc_theme = NULL
    )
  )
  for (.nm in names(.cases)) {
    .args <- c(list(sim = .d$sim, obs = .d$obs), .cases[[.nm]])
    .theirs <- do.call(vpc::vpc_vpc, .args)
    .db <- do.call(vpc::vpc_vpc, c(.args, list(vpcdb = TRUE)))
    .ours <- .vpcDbFigure(
      .db,
      vpc_theme = .args$vpc_theme,
      smooth = .args$smooth,
      log_y = .args$log_y,
      title = .args$title
    )
    expect_equal(.vpcFigureBuild(.ours), .vpcFigureBuild(.theirs), info = .nm)
  }

  # vpc_cens() draws its vpcdb with log_y = FALSE
  .theirs <- vpc::vpc_cens(sim = .d$sim, obs = .d$obs, lloq = 20, title = "BLQ")
  .db <- vpc::vpc_cens(sim = .d$sim, obs = .d$obs, lloq = 20, title = "BLQ", vpcdb = TRUE)
  .ours <- .vpcDbFigure(.db, vpc_theme = NULL, smooth = TRUE, log_y = FALSE, title = "BLQ")
  expect_equal(.vpcFigureBuild(.ours), .vpcFigureBuild(.theirs))
})

test_that(".vpcDbTrim() keeps only the observed columns vpc draws", {
  skip_if_not_installed("vpc")
  .d <- .vpcExampleData(5)
  .db <- vpc::vpc_vpc(sim = .d$sim, obs = .d$obs, stratify = "ISM", vpcdb = TRUE)
  .trim <- .vpcDbTrim(.db)
  expect_equal(nrow(.trim$sim), 0L)
  expect_named(.trim$sim, names(.db$sim))
  expect_equal(nrow(.trim$obs), 0L)
  expect_named(.trim$obs, names(.db$obs))
  expect_identical(.trim[c("vpc_dat", "aggr_obs", "bins")], .db[c("vpc_dat", "aggr_obs", "bins")])

  .db <- vpc::vpc_vpc(sim = .d$sim, obs = .d$obs, stratify = "ISM", show = list(obs_dv = TRUE), vpcdb = TRUE)
  .trim <- .vpcDbTrim(.db)
  expect_equal(nrow(.trim$sim), 0L)
  expect_named(.trim$obs, c("idv", "dv", "ISM"))
  expect_equal(.trim$obs$dv, .db$obs$dv)
})

test_that(".vpcDbFigure() does not grow with the number of simulations", {
  skip_if_not_installed("vpc")
  # the larger simulation is drawn first so that any one-time growth of
  # ggplot2's objects on first use can only make the second figure larger
  .sizes <- vapply(
    c(50, 5),
    function(n) {
      .d <- .vpcExampleData(n)
      .db <- vpc::vpc_vpc(sim = .d$sim, obs = .d$obs, vpcdb = TRUE)
      .figureSizeWithoutData(.vpcDbFigure(.db, vpc_theme = NULL, smooth = TRUE, log_y = FALSE, title = NULL))
    },
    numeric(1)
  )
  expect_lte(.sizes[1] - .sizes[2], 1024)
})

test_that(".tidyvpcFigure() draws what tidyvpc draws", {
  skip_if_not_installed("tidyvpc")
  skip_if_not_installed("xgxr")
  .d <- .tidyvpcExampleData(10)
  .obs <- tidyvpc::simulated(tidyvpc::observed(.d$obs, x = TIME, y = DV), .d$sim, y = DV)

  .stats <- tidyvpc::vpcstats(tidyvpc::binning(.obs, bin = NTIME))
  expect_equal(
    .vpcFigureBuild(.tidyvpcFigure(.stats, xlab = NULL, ylab = NULL, title = NULL, log_y = FALSE)),
    .vpcFigureBuild(plot(.stats))
  )

  .stats <- tidyvpc::vpcstats(
    tidyvpc::predcorrect(tidyvpc::binning(tidyvpc::stratify(.obs, ~GENDER), bin = NTIME), pred = PRED)
  )
  expect_false(is.null(.stats$strat.split))
  expect_equal(
    .vpcFigureBuild(.tidyvpcFigure(.stats, xlab = "Time (h)", ylab = "Concentration", title = "VPC", log_y = TRUE)),
    .vpcFigureBuild(
      plot(.stats) +
        ggplot2::xlab("Time (h)") +
        ggplot2::ylab("Concentration") +
        ggplot2::ggtitle("VPC") +
        xgxr::xgx_scale_y_log10()
    )
  )

  skip_if_not_installed("quantreg")
  # quantreg warns about the small example while fitting the binless quantiles
  .stats <- suppressWarnings(tidyvpc::vpcstats(tidyvpc::binless(.obs)))
  expect_equal(
    .vpcFigureBuild(.tidyvpcFigure(.stats, xlab = NULL, ylab = NULL, title = NULL, log_y = FALSE)),
    .vpcFigureBuild(plot(.stats))
  )
})

test_that(".tidyvpcFigure() does not grow with the number of simulations", {
  skip_if_not_installed("tidyvpc")
  .sizes <- vapply(
    c(50, 5),
    function(n) {
      .d <- .tidyvpcExampleData(n)
      .obs <- tidyvpc::simulated(tidyvpc::observed(.d$obs, x = TIME, y = DV), .d$sim, y = DV)
      .stats <- tidyvpc::vpcstats(tidyvpc::binning(.obs, bin = NTIME))
      .figureSizeWithoutData(.tidyvpcFigure(.stats, xlab = NULL, ylab = NULL, title = NULL, log_y = FALSE))
    },
    numeric(1)
  )
  expect_lte(.sizes[1] - .sizes[2], 1024)
})

test_that("VPC figures keep neither the fit nor the simulation", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("vpc")
  skip_if_not_installed("tidyvpc")

  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  censData <- nlmixr2data::theo_md
  censData$CENS[censData$DV < 3 & censData$AMT == 0] <- 1
  censData$CENS[censData$DV >= 3 & censData$AMT == 0] <- 0
  censData$DV[censData$CENS == 1] <- 3
  censData$SEX <- censData$ID %% 2L
  fit <- try(
    suppressMessages(
      nlmixr2est::nlmixr(
        one.cmt,
        censData,
        est = "focei",
        control = nlmixr2est::foceiControl(print = 0, eval.max = 10),
        table = list(keep = "SEX")
      )
    ),
    silent = TRUE
  )
  skip_if(inherits(fit, "try-error"))

  .cases <- list(
    "vpcPlot(method = 'vpc')" = list(vpcPlot, list(method = "vpc")),
    "vpcPlot(method = 'tidyvpc')" = list(vpcPlot, list(method = "tidyvpc")),
    "vpcPlotTad(method = 'vpc')" = list(vpcPlotTad, list(method = "vpc")),
    "vpcCens(method = 'vpc')" = list(vpcCens, list(method = "vpc")),
    "vpcCens(method = 'tidyvpc')" = list(vpcCens, list(method = "tidyvpc")),
    # the stratification formula is kept by tidyvpc's facet and strata
    "vpcPlot(method = 'tidyvpc', stratify = 'SEX')" = list(vpcPlot, list(method = "tidyvpc", stratify = "SEX"))
  )
  for (.nm in names(.cases)) {
    .f <- .cases[[.nm]][[1]]
    .args <- .cases[[.nm]][[2]]
    # the larger simulation first, as above
    .big <- suppressWarnings(do.call(.f, c(list(fit, n = 20), .args)))
    .small <- suppressWarnings(do.call(.f, c(list(fit, n = 2), .args)))
    expect_s3_class(.big, "ggplot")
    expect_lte(
      .figureSizeWithoutData(.big) - .figureSizeWithoutData(.small),
      1024,
      label = sprintf("growth of %s from 2 to 20 simulations apart from its data (bytes)", .nm)
    )
    expect_false(
      any(grepl("<nlmixr2FitData>|<nlmixr2vpcSim>", .figureHeldData(.big))),
      label = sprintf("%s holds the fit or the simulation", .nm)
    )
  }

  # vpcdb = TRUE still returns vpc's whole vpcdb, simulation included
  .db <- suppressWarnings(vpcPlot(fit, n = 2, method = "vpc", vpcdb = TRUE))
  expect_s3_class(.db, "vpcdb")
  expect_gt(nrow(.db$sim), nrow(.db$obs))
  .db <- suppressWarnings(vpcCens(fit, n = 2, method = "vpc", vpcdb = TRUE))
  expect_s3_class(.db, "vpcdb")
  expect_gt(nrow(.db$sim), nrow(.db$obs))
})
