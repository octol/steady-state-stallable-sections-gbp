# Compute contour plot of required section sizes for the steady state scenario.

# > pbirthday(10000000*0.25, classes = 10000000/1000, coincident = ceiling(1000/3))
# [1] 0.002485384

num_of_nodes <- 10000000

vuln_for_section_size <- function(section_size, attacker_fraction) {
    pbirthday(
        num_of_nodes*attacker_fraction,
        classes = num_of_nodes/section_size,
        coincident = ceiling(section_size/3)
    )
}

section_size <- seq(10, 1000, 5)
attacker <- seq(0.05, 0.30, 0.001)

risk <- matrix(nrow = length(section_size), ncol = length(attacker))

for (si in 1:length(section_size)) {
  for (ai in 1:length(attacker)) {
    risk[si, ai] <- vuln_for_section_size(section_size[si],attacker[ai])
  }
}

library(plotly)
p <- plot_ly(x=attacker, y=section_size, z=risk, type = "contour", colorscale='Jet') %>%
  layout(
    title = "Probability of stallable section in the steady-state scenario (N=10M)",
    xaxis = list(title = "Fraction of network resources controlled by adversary"),
    yaxis = list(title = "Section size")
  )
