# Bass Model Functions

# f(t): share of the market that adopts at time t
bass.f <- function(t, p, q){
  ((p+q)^2 / p) * exp(-(p + q) * t) / (1 + (q / p) * exp(-(p + q) * t))^2
}

# F(t): share of market that has adopted up to and including time t
bass.F <- function(t, p, q){
  (1 - exp(-(p + q) * t)) / (1 + (q / p) * exp(- (p + q) * t))
}

# Colors and Plot Theme for Figures
col_main <- "#052470"   # dark blue
col_accent <- "#E07A3F"   # orange
col_light  <- "#8FA9C4"   # light blue
col_grey   <- "#6B7280"   # grey text

theme_bass <- function(){
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face="bold", size=14, colour=col_main),
      plot.subtitle = element_text(colour=col_grey),
      axis.title = element_text(colour=col_grey),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# Number Formatting
fmt <- function(x, digits = 0){
  format(round(x, digits), big.mark=',', nsmall=digits, trim=TRUE)
}