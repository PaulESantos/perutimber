
#' @keywords internal
# perutimber_especies list ------------------------------------------------
#' lista de especies presentes en la base de datos perutimber
#' se debe conciderar que las combinaciones de input_genus, input_epitheton,
#' input_subspecies_epitheton son unicos,
#' los valores de  accepted_name pueden repetirse
#'
#' pt_df <- readxl::read_xlsx("tab_perutimber.xlsx") |>
#'   dplyr::mutate(scientific_name = paste0(input_genus, " ",
#'                                          input_epitheton)) |>
#'   dplyr::mutate(scientific_name = dplyr::case_when(
#'     rank == "subspecies" ~ paste0(input_genus, " ",
#'                                   input_epitheton, " ",
#'                                   "subsp. ",
#'                                   input_subspecies_epitheton),
#'     rank == "variety" ~ paste0(input_genus, " ",
#'                                input_epitheton, " ",
#'                                "var. ",
#'                                input_subspecies_epitheton),
#'     TRUE ~ scientific_name
#'   )) |>
#'   dplyr::mutate(scientific_name = dplyr::case_when(
#'     !is.na(accepted_name_author) ~ paste0(scientific_name, " ",
#'                                           accepted_name_author),
#'     TRUE ~ scientific_name
#'   )) |>
#'   dplyr::arrange(accepted_family, input_genus) |>
#'   dplyr::mutate(id_cat = paste0( dplyr::row_number(),
#'                                  stringr::str_sub(input_genus, 1, 2),
#'                                  stringr::str_sub(input_epitheton, 1, 2)) |>
#'                   toupper())
#' pt_df
#' # revisa que los nombres completos sean unicos
#'
#' pt_df |>
#'   dplyr::add_count(scientific_name) |>
#'   dplyr::filter(n > 1)
#'
#' # TAB_PERUTIMBER ----------------------------------------------------------
#' #' esta base de datos debe ser guardada en una base de datos de tipo
#' #' data.frame
#' names_tab_perutimber <- c("id_cat", "input_genus", "input_epitheton",
#'                           "rank", "input_subspecies_epitheton", "taxonomic_status",
#'                           "accepted_name", "accepted_family", "accepted_name_author",
#'                           "tnrs_name_id", "accepted_name_url", "source")
#' names_tab_perutimber |> length()
#'
#' tab_perutimber <- pt_df |>
#'   dplyr::select( dplyr::all_of(names_tab_perutimber)) |>
#'   as.data.frame()
#' tab_perutimber
#'
#' # PERUTIMBER SPS CLASS ----------------------------------------------------
#' #' esta debe almacenarce como matrx o array
#' names_sps_class <- c("species", "genus", "epithet", "author",
#'                      "subspecies", "variety",
#'                      "subvariety", "forma", "subforma", "id")
#'
#' perutimber_sps_class <- pt_df |>
#'   dplyr::select(species = scientific_name,
#'                 genus = input_genus,
#'                 epithet = input_epitheton,
#'                 author = accepted_name_author,
#'                 input_subspecies_epitheton,
#'                 rank) |>
#'   dplyr::mutate(subspecies =  dplyr::case_when(
#'     rank == "subspecies" ~ input_subspecies_epitheton,
#'     TRUE ~ ""
#'   ),
#'   variety =  dplyr::case_when(
#'     rank == "variety" ~ input_subspecies_epitheton,
#'     TRUE ~ ""
#'   ),
#'   subvariety = "",
#'   forma = "",
#'   subforma = "",
#'   id =  dplyr::row_number()) |>
#'   dplyr::select( dplyr::all_of(names_sps_class)) |>
#'   dplyr::mutate_all(~as.character(.) |>
#'                       toupper()) |>
#'   as.matrix.data.frame()
#'
#' perutimber_sps_class |>
#'   class()
#'
#' # TAB POSSITION -----------------------------------------------------------
#' #' esta debe ser guardada como data.frame
#' #' perutimber::tab_perutimber_position
#' names_posistion = c("position", "triphthong", "genus")
#'
#' tab_perutimber_position <- pt_df |>
#'   dplyr::select(genus = input_genus) |>
#'   dplyr::mutate(id =  dplyr::row_number(),
#'                 triphthong = stringr::str_sub(genus, 1, 3)) |>
#'   dplyr::mutate_if(is.character, ~toupper(.)) |>
#'   dplyr::group_by(genus, triphthong) |>
#'   dplyr::summarise(position = min(id),
#'                    .groups = "drop") |>
#'   dplyr::arrange(position) |>
#'   as.data.frame()
#'
#' tab_perutimber_position
#'
#' # save clean data ---------------------------------------------------------
#' tab_perutimber |>
#'   save(file = "data/tab_perutimber.rda")
#' perutimber_sps_class |>
#'   save(file = "data/perutimber_sps_class.rda")
#' tab_perutimber_position |>
#'   save(file = "data/tab_perutimber_position.rda")
#'
