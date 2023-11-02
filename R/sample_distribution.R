source("R/setup.R")

summary.ext <- function(kpi, name) {

  if ( missing(name) ) { kpiname <- deparse(substitute(kpi)) } else { kpiname <- name }

  temp <- data.frame(t(data.frame( unclass(summary(as.numeric(kpi))), check.names = FALSE, stringsAsFactors = FALSE)))
  colnames(temp) <- c("Min","Pct25","Median","Mean","Pct75","Max")
  row.names(temp) <- NULL

  return(data.frame(Variable=kpiname, N=length(as.numeric(kpi)), temp, StD=stats::sd(as.numeric(kpi)), Var=stats::var(as.numeric(kpi)), Skewness=moments::skewness(as.numeric(kpi)) , Kurtosis=moments::kurtosis(as.numeric(kpi)) ))

}


plot.distribution <- function(sample.dataset, dist.stats, output) {

  dist.table <- gridExtra::tableGrob(dist.stats, rows = NULL) #formattable::formattable(dist.stats)

  dist.hist <- ggplot2::ggplot(sample.dataset, ggplot2::aes(x=X)) +
    ggplot2::geom_histogram(binwidth=nrow(sample.dataset)/(nrow(sample.dataset)*.3),fill='lightblue') +
    ggplot2::ggtitle("Histogram (Frequency)") + labs(y="") + ggplot2::theme_minimal()

  dist.chart <- ggplot2::ggplot(sample.dataset, ggplot2::aes(x=X,y=ggplot2::after_stat(density))) +
    ggplot2::geom_density(fill='lightblue') + ggplot2::ggtitle("Histogram (Density)") + labs(y="") + ggplot2::theme_minimal()

  dist.density <- ggplot2::ggplot(sample.dataset, ggplot2::aes(x=X)) + ggplot2::stat_ecdf(geom = "step", linewidth = 0.5, color=palette[1]) +
    ggplot2::ggtitle("Cumulative Density Function") + labs(y="") + ggplot2::theme_minimal()

  dist.violin <- ggplot2::ggplot(sample.dataset, ggplot2::aes(x="",y=X)) + ggplot2::geom_violin(trim = FALSE, fill="lightblue") +
    ggplot2::geom_boxplot(width=0.3)  + ggplot2::stat_summary(fun=median, geom="point", size=2, color="black") +
    ggplot2::ggtitle("Box/Violin Plot") +  labs(y="",x="X") + theme_minimal()

  #plots <-  (dist.hist + dist.chart) / (dist.violin + dist.density)

  grDevices::pdf(paste0(output, "/", gsub(" ", "_",dist.stats$Variable),".pdf"), width = 12.5, height = 7)

  p <- gridExtra::grid.arrange(dist.table, dist.hist, dist.chart, dist.violin , dist.density, nrow = 3,
                          layout_matrix = rbind(c(1), c(2,3), c(4,5) ))
  p
  dev.off()

}



#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
normal_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the Normal Distribution")

  mean <- base::sample(1:n,1)
  sd <- mean*0.15
  sample.dataset <- data.frame(X=stats::rnorm( n , mean = mean, sd = sd ))
  dist.stats <- summary.ext(sample.dataset$X,"Normal Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}


#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
exponential_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the Exponential Distribution")

  sample.dataset <- data.frame(X=stats::rexp( n , rate = base::sample(1:n,1)/100 ))
  dist.stats <- summary.ext(sample.dataset$X, "Exponential Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}


#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
chi_square_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the Chi-square Distribution")

  sample.dataset <- data.frame(X=stats::rchisq( n , df = base::sample(1:n,1) ))
  dist.stats <- summary.ext(sample.dataset$X, "Chi-square Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}

#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
logistic_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the Logistic Distribution")

  sample.dataset <- data.frame(X=stats::rlogis( n ))
  dist.stats <- summary.ext(sample.dataset$X, "Logistic Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}


#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
poisson_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the Poisson Distribution")

  sample.dataset <- data.frame(X=stats::rpois( n , lambda = base::sample(1:n,1) ))
  dist.stats <- summary.ext(sample.dataset$X, "Poisson Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}


#' Title
#'
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
t_student_distribution <- function( n=1000, output ) {

  #base::set.seed(1000)
  #base::print("Hello, world!. This is the t-student Distribution")

  sample.dataset <- data.frame(X=stats::rt( n , df = base::sample(1:n,1) ))
  dist.stats <- summary.ext(sample.dataset$X, "t-student Distribution")

  plot.distribution(sample.dataset, dist.stats, output)

}



#' Title
#'
#' @param dist.list List of distributions to simulate
#' @param n N Observations
#' @param output Path to store plots
#'
#' @return
#' @export
#'
#' @examples
sample_distribution <- function(dist.list=c("normal"), n=1000, output="/Users/joaocaldeira/Desktop") {

  message("\nValid options for distributions are: normal, exponential, chi-square, logistic, poisson, t-student.")
  message("More options are being developed.\n")

  if ("normal" %in% dist.list) pmopsR::normal_distribution(n, output)
  if ("exponential" %in% dist.list) pmopsR::exponential_distribution(n, output)
  if ("chi-square" %in% dist.list) pmopsR::chi_square_distribution(n, output)
  if ("logistic" %in% dist.list) pmopsR::logistic_distribution(n, output)
  if ("poisson" %in% dist.list) pmopsR::poisson_distribution(n, output)
  if ("t-student" %in% dist.list) pmopsR::t_student_distribution(n, output)

}


#library(reactablefmtr)
#reactable(rexp( 1000 , rate = 1),defaultColDef = colDef(cell = data_bars(df, box_shadow = TRUE, round_edges = TRUE,
#                                                                         text_position = "outside-base",
#                                                                         fill_color = c("#e81cff", "#40c9ff"),
#                                                                         background = "#e5e5e5",fill_gradient = TRUE)))

