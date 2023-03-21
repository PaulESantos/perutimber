library(here)
library(tidyverse)
library(tabulizer)

file_path <- "pdf/51277_Artículo_[RFP]_VF.pdf"

text <- tabulizer::extract_text(file = file_path,
                                pages = c(17:134),
                                encoding = "UTF-8")
text <- text |>
  stringr::str_split("\n|\r") |>
  unlist()

texto_1 <- text[nchar(text) != 0]

tbl_texto_1 <- tibble::tibble(especies = texto_1)

new_df <- tbl_texto_1 |>
  mutate(especies = str_squish(especies) |> str_trim()) |>
  filter(!str_detect(especies,
                     paste0(c("^[A-Z]{1,}CEAE",
                              "[A-Z]{1,}MAS",
                              "Vol.",
                              "Catálogo",
                              "Diciembre 2022",
                              "Revista Forestal del Perú",
                              "de la Amazonía y la Yunga Peruana"),
                            collapse = "|"))) |>
  filter(!str_detect(especies,
                     paste0("^",seq(21, 138, 1), "$", collapse = "|")))

new_df

new_df |>
  mutate(especies = str_squish(especies) |> str_trim()) |>
  filter(str_detect(especies,
                    paste0(c("^[A-Z]{1}[a-z]{1,} [a-z]{1,} ",
                             "^[A-Z]{1}[a-z]{1,} [a-z]{1,}-[a-z]{1,} "
                             ), collapse = "|"))) |>
  mutate(id = row_number())


species_list <- readxl::read_xlsx("new_especies_catalogo.xlsx") |>
  mutate(new_especie = paste0("paul start ", id, " ", especies))

species_list


new_df |>
  left_join(species_list) |>
  mutate(group = if_else(
    !is.na(id),
    paste0("especie ", id),
    as.character(id)
    )) |>
  fill(group, .direction = "down") |>
  mutate(row_id = row_number()) |>
  relocate(row_id)


# -------------------------------------------------------------------------

catalogo <- readxl::read_excel("catalogo_v1.xlsx")
catalogo |>
  count(group)

by_especie <- catalogo  |>
  group_nest(group)

by_especie
# funciones ---------------------------------------------------------------

change_split_word <- function(x){
  split_word <- str_extract_all(x,
                                "[a-z]{1,}-\\s[a-z]{1,}")

  mgsub::mgsub(
    x,
    unlist(split_word),
    str_replace_all(split_word |>  unlist(), "-\\s", "")
               )
}


change_split_num_interval <- function(x){
  num_interval <- stringr::str_extract_all(x,
                              "[0-9]{1,}-\\s[0-9]{1,}")
  mgsub::mgsub(
    x ,
    unlist(num_interval),
    stringr::str_replace_all(num_interval |>  unlist(), "-\\s", "-")
  )
}

get_tbl_catl_new <- function(x){

  get_usos <- function(x){
    if(grepl("Categ. UICN:", x) == FALSE){
      usos <- gsub(".*Usos:", "\\1", x)

    }
    else if(grepl("Categ. UICN:", x) == TRUE){
      usos <- gsub(".*Usos: (.+) Categ. UICN:.*", "\\1", x)
    }
    return(usos)
  }

  get_uicn_text <- function(x){
    if(grepl("Categ. UICN:", x) == FALSE){
      uicn <- NA_character_

    }
    else if(grepl("Categ. UICN:", x) == TRUE){
      uicn <- gsub(".*Categ. UICN:", "\\1", x)
    }
    return(uicn)
  }

  #new_x <- change_split_num_interval(x) |>
  # change_split_word()


  return(tibble::tibble(
    species =  gsub("N\\.Ver\\:.*", "\\1", x),
    nombre_comun =  gsub(".*N\\.Ver\\: (.+) Hábito:.*", "\\1", x),
    habito = gsub(".*Hábito: (.+) Col Ref:.*", "\\1", x),
    collecciom = gsub(".*Col Ref: (.+) Región\\(es\\):.*", "\\1", x),
    distribucion = gsub(".*Región\\(es\\): (.+) Usos:.*", "\\1", x),
    usos = get_usos(x),
    categ_uicn = get_uicn_text(x)
  ) |>
    dplyr::mutate_all(~stringr::str_trim(.) |>
                        stringr::str_squish() ))

}


# -------------------------------------------------------------------------

new_catalog <- catalogo |>
  group_by(group) |>
  summarise(new_text = paste0(especies, collapse = " "),
            .groups = "drop") |>
  select(-group)
new_catalog_vect <- new_catalog |>  pull(new_text)

clean_df <- map_dfr(new_catalog_vect,
                    ~get_tbl_catl_new(.))

#' > clean_df |> names()
#' [1] "species"  "nombre_comun" "habito" "collecciom"   "distribucion" "usos"
#' [7] "categ_uicn"

clean_df1 <- clean_df |>
  mutate(species = change_split_word(species)) |>
  mutate(nombre_comun = change_split_word(nombre_comun)) |>
  mutate(collecciom = change_split_word(collecciom)) |>
  mutate(distribucion = change_split_word(distribucion)) |>
  mutate(distribucion = change_split_num_interval(distribucion)) |>
  mutate(usos = change_split_word(usos))


clean_df2 <- clean_df1 |>
  mutate(tag_subsp = case_when(
    str_detect(species, "subsp\\.") ~ "subsp.",
    str_detect(species, "var\\.") ~ "var.",
    TRUE ~ NA_character_
  )) |>
  mutate(genus_ephitethon = stringr::word(species, 1, 1),
         species_ephitethon = stringr::word(species, 2, 2),
         subspecies_ephitethon = case_when(
         tag_subsp == "var." ~ stringr::str_extract(species, "(?<=var\\. )(\\w+)"),
         tag_subsp == "subsp." ~ stringr::str_extract(species, "(?<=subsp\\. )(\\w+)"),
         TRUE ~ NA_character_
         )) |>
  mutate(tipo = stringr.plus::str_extract_before(habito, "\\s[0-9]{1,}"),
         altura = stringr.plus::str_extract_after(habito, "[a-z]{1,}\\s")) |>
  mutate(colector =  stringr.plus::str_extract_before(collecciom,
                                                      "\\s[0-9]{1,}"),
         id_coleccion = stringr::str_extract(collecciom, "[0-9]{1,}"),
         deposito = stringr::str_extract(collecciom, "\\([^()]+\\)")
         ) |>
  mutate(regiones = stringr.plus::str_extract_before(distribucion,
                                                     "\\sAlt\\(m\\.\\):"),
         elevacion = stringr.plus::str_extract_after(distribucion,
                                                     "\\sAlt\\(m\\.\\): ")) |>
  mutate(usos_2 = gsub("\\[.*?\\]", "", usos),
         endemismo = if_else(str_detect(usos, "Dist\\. Endémico\\."),
                             "Endémico",
                             NA_character_),
         categ_uicn = tm::removePunctuation(categ_uicn))
clean_df2 |>
  writexl::write_xlsx("catalogo_ver_2.xlsx")
