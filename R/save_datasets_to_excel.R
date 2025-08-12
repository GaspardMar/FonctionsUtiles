#' Save datasets to excel
#'
#' @param datasets a list of several datasets
#' @param file_name The name of the final dataset you want to save in your working directory
#'
#' @return an excel file saved in your working directory with several windows. One window
#' by saved dataset
#' @author Gaspard Martet
#' @export
#'
#' @examples
#' save_datasets_to_excel(datasets = list("iris" = iris, "mtcars" = mtcars),
#' file_name = "datasets.xlsx")
#'
save_datasets_to_excel <- function(datasets, file_name) {
  # Vérifie que c'est une liste
  if (!is.list(datasets)) {
    stop("datasets doit être une liste de data.frames")
  }

  # Crée le workbook
  wb <- openxlsx::createWorkbook()

  # Parcours de la liste
  for (name in names(datasets)) {
    openxlsx::addWorksheet(wb, name)               # ajoute une feuille avec le nom du dataset
    openxlsx::writeData(wb, name, datasets[[name]]) # écrit les données
  }

  # Sauvegarde
  openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
  message("ichier Excel sauvegardé : ", file_name)
}
