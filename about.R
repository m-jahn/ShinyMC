helpbox <- function(width=6) {
  column(width=width, 
    h4("INFO & HELP"),
    wellPanel(
      h4("HOW TO"),
      p("The app loads *csv tables with cultivation data derived from batch or continuous cultivation.
        The basic features are plotting the optical density, growth rate µ calculated by a set of different 
        functions, retention and doubling time that are derived from the calculated growth rate, temperature
        in the reactor, and external CO2 measurements.
        The mode of cultivation suggest different ways to calculate the growth rate and doubling time:"),
      tags$ul(
        tags$li("batch mode - a sliding window of different length (2-10 measurements) is used to determine µ"), 
        tags$li("conti dilution - the number of dilutions per time unit and the dilution volume is used to calculate µ"),
        tags$li("conti interval - the recommended option for turbidostat, µ is calculated from the increase in 
          OD between two dilution events. An additional option allows to set the number of measurements 
          to automatic (takes all points between local min and max), min-max (only slope between min and max),
          or a user-specified value (points between max, and max minus the user specified value)")
      ),
      h4("REFERENCES"),
      p(""),
      p("The source code for this R shiny app is available on ", 
        a(href="https://github.com/m-jahn", target= '_blank', 'github/m-jahn')
      ),
      #
      h4("CONTACT"),
      p("For questions or reporting issues, contact 
        Michael Jahn, Science For Life Lab - Royal Technical University (KTH), Stockholm"), 
      p(
        a(href="mailto:michael.jahn@scilifelab.se", target= '_blank', 'email: Michael Jahn')
      )
    )
  )
}
