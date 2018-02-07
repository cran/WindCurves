# Click functions
unittrim <- function(unit) {
  sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\1", as.character(unit))
}

do.click.value <- function(unit, value) {
  click.locn <- grid.locator(unit)
  grid.segments(unit.c(click.locn$x, unit(0, "npc")),
                unit.c(unit(0, "npc"), click.locn$y),
                click.locn$x, click.locn$y,
                gp=gpar(lty="dashed", col="grey"))
  grid.points(click.locn$x, click.locn$y, pch=16, size=unit(1, "mm"))
  clickx <- unittrim(click.locn$x)
  clicky <- unittrim(click.locn$y)
  grid.text(paste0("(", clickx, ", ", clicky, ")"),
            click.locn$x + unit(2, "mm"), click.locn$y,
            just="left")
  n <- readline(prompt="Enter the value: ")
  return(c(click.locn$x, click.locn$y, as.integer(n)))
}
