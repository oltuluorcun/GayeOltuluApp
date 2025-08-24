# modules/previousDiets.R

previousDietsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(h1("Kullanıcı girişi gelecek")),
    fluidRow(
      column(width = 6, h1("Geçmiş Veriler")),
      column(width = 6, h1("Notlar gelecek"))
    )
  )
}

previousDietsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Buraya ileride geçmiş diyet verileri eklenecek
  })
}
