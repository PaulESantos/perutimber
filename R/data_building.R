
#' @keywords internal
# perutimber_especies list ------------------------------------------------
#' lista de especies presentes en la base de datos perutimber
#' se debe conciderar que las combinaciones de input_genus, input_epitheton,
#' input_subspecies_epitheton son unicos,
#' los valores de  accepted_name pueden repetirse
#'
#comentario#-  pt_df <- readxl::read_xlsx("tab_perutimber.xlsx") |>
#comentario#-    dplyr::mutate(scientific_name = paste0(input_genus, " ",
#comentario#-                                           input_epitheton)) |>
#comentario#-    dplyr::mutate(scientific_name = dplyr::case_when(
#comentario#-      rank == "subspecies" ~ paste0(input_genus, " ",
#comentario#-                                    input_epitheton, " ",
#comentario#-                                    "subsp. ",
#comentario#-                                    input_subspecies_epitheton),
#comentario#-      rank == "variety" ~ paste0(input_genus, " ",
#comentario#-                                 input_epitheton, " ",
#comentario#-                                 "var. ",
#comentario#-                                 input_subspecies_epitheton),
#comentario#-      TRUE ~ scientific_name
#comentario#-    )) |>
#comentario#-    dplyr::mutate(scientific_name = dplyr::case_when(
#comentario#-      !is.na(accepted_name_author) ~ paste0(scientific_name, " ",
#comentario#-                                            accepted_name_author),
#comentario#-      TRUE ~ scientific_name
#comentario#-    )) |>
#comentario#-    dplyr::arrange(accepted_family, input_genus) |>
#comentario#-    dplyr::mutate(id_cat = paste0( dplyr::row_number(),
#comentario#-                                   stringr::str_sub(input_genus, 1, 2),
#comentario#-                                   stringr::str_sub(input_epitheton, 1, 2)) |>
#comentario#-                    toupper())
#comentario#-  pt_df
#comentario#-  # revisa que los nombres completos sean unicos
#comentario#-
#comentario#-  pt_df |>
#comentario#-    dplyr::add_count(scientific_name) |>
#comentario#-    dplyr::filter(n > 1)
#comentario#-
#comentario#-  # TAB_PERUTIMBER ----------------------------------------------------------
#comentario#-  #' esta base de datos debe ser guardada en una base de datos de tipo
#comentario#-  #' data.frame
#comentario#-  names_tab_perutimber <- c("id_cat", "input_genus", "input_epitheton",
#comentario#-                            "rank", "input_subspecies_epitheton", "taxonomic_status",
#comentario#-                            "accepted_name", "accepted_family", "accepted_name_author",
#comentario#-                            "tnrs_name_id", "accepted_name_url", "source")
#comentario#-  names_tab_perutimber |> length()
#comentario#-
#comentario#-  tab_perutimber <- pt_df |>
#comentario#-    dplyr::select( dplyr::all_of(names_tab_perutimber)) |>
#comentario#-    as.data.frame()
#comentario#-  tab_perutimber
#comentario#-
#comentario#-  # PERUTIMBER SPS CLASS ----------------------------------------------------
#comentario#-  #' esta debe almacenarce como matrx o array
#comentario#-  names_sps_class <- c("species", "genus", "epithet", "author",
#comentario#-                       "subspecies", "variety",
#comentario#-                       "subvariety", "forma", "subforma", "id")
#comentario#-
#comentario#-  perutimber_sps_class <- pt_df |>
#comentario#-    dplyr::select(species = scientific_name,
#comentario#-                  genus = input_genus,
#comentario#-                  epithet = input_epitheton,
#comentario#-                  author = accepted_name_author,
#comentario#-                  input_subspecies_epitheton,
#comentario#-                  rank) |>
#comentario#-    dplyr::mutate(subspecies =  dplyr::case_when(
#comentario#-      rank == "subspecies" ~ input_subspecies_epitheton,
#comentario#-      TRUE ~ ""
#comentario#-    ),
#comentario#-    variety =  dplyr::case_when(
#comentario#-      rank == "variety" ~ input_subspecies_epitheton,
#comentario#-      TRUE ~ ""
#comentario#-    ),
#comentario#-    subvariety = "",
#comentario#-    forma = "",
#comentario#-    subforma = "",
#comentario#-    id =  dplyr::row_number()) |>
#comentario#-    dplyr::select( dplyr::all_of(names_sps_class)) |>
#comentario#-    dplyr::mutate_all(~as.character(.) |>
#comentario#-                        toupper()) |>
#comentario#-    as.matrix.data.frame()
#comentario#-
#comentario#-  perutimber_sps_class |>
#comentario#-    class()
#comentario#-
#comentario#-  # TAB POSSITION -----------------------------------------------------------
#comentario#-  #' esta debe ser guardada como data.frame
#comentario#-  #' perutimber::tab_perutimber_position
#comentario#-  names_posistion = c("position", "triphthong", "genus")
#comentario#-
#comentario#-  tab_perutimber_position <- pt_df |>
#comentario#-    dplyr::select(genus = input_genus) |>
#comentario#-    dplyr::mutate(id =  dplyr::row_number(),
#comentario#-                  triphthong = stringr::str_sub(genus, 1, 3)) |>
#comentario#-    dplyr::mutate_if(is.character, ~toupper(.)) |>
#comentario#-    dplyr::group_by(genus, triphthong) |>
#comentario#-    dplyr::summarise(position = min(id),
#comentario#-                     .groups = "drop") |>
#comentario#-    dplyr::arrange(position) |>
#comentario#-    as.data.frame() |>
#comentario#-    dplyr::select(dplyr::all_of(names_posistion))
#comentario#-
#comentario#-  tab_perutimber_position |>  head()
#comentario#-
#comentario#-  # save clean data ---------------------------------------------------------
#comentario#-  tab_perutimber |>
#comentario#-    save(file = "data/tab_perutimber.rda")
#comentario#-  perutimber_sps_class |>
#comentario#-    save(file = "data/perutimber_sps_class.rda")
#comentario#-  tab_perutimber_position |>
#comentario#-    save(file = "data/tab_perutimber_position.rda")
#comentario#-
