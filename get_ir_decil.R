#### library ####

library(purrr)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)
library(janitor)


#### 1. FUNCOES ####

# montar urls


read_ir_url <- function() {
  # pra puxar as urls
  
  sufixos <-
    c(
      "20210321%20Centil2007_AC2006", #2006
      "20210321%20Centil2008_AC2007", #2007
      "20210321%20Centil2009_AC2008", #2008
      "20210321%20Centil2010_AC2009", #2009
      "20210321%20Centil2011_AC2010", #2010
      "20210321%20Centil2012_AC2011", #2011
      "20210321%20Centil2013_AC2012", #2012
      "20210321%20Centil2014_AC2013", #2013
      "20210321%20Centil2015_AC2014", #2014
      "20210321%20Centil2016_AC2015", #2015
      "20210325%20Centil2017_AC2016", #2016
      "20210321%20Centil2018_AC2017", #2017
      "20210321%20Centil2019_AC2018", #2018
      "20210326%20Centil2020_AC2019", #2019
      "20210829%20Centil%20AC%202020%20(2)" #2020
    )
  
  ano <- 2006:2020
  
  url_ir <- paste0(
    "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/estudos/distribuicao-da-renda/distribuicao-da-renda-por-centis-",
    ano, "/@@download/file/", sufixos, ".xlsx"
  ) %>%
    as.list() %>%
    set_names(ano)

  }


# baixar arquivos em pasta temporaria

donwload_ir <- function(url_ir) {
  
  purrr::walk2(
    url_ir, # as urls
    url_ir %>% names(), # os nomes dos itens nas listas que são os anos
    ~ download.file(.x, # passa cada item do primeiro vetor
                    destfile = paste0(tempdir(), "\\ir", .y, ".xlsx"), # segundo item do vetor
                    mode = "wb"
    )
  ) # modo
  
  # caminho pastas
  path <- paste0(tempdir(), "\\ir", 2006:2020, ".xlsx")
}
  

  #### lendo sheets ####
  
read_ir_clean <- function(path, sheet) {

  # lê uma aba do excel

  df <- suppressWarnings(
    suppressMessages(
      read_excel(path,
        col_names = TRUE,
        sheet = sheet,
        skip = 5,
        col_types = rep("numeric", 24)
      )
    )
  ) %>%
    janitor::clean_names() %>%
    rename(
      centil_1 = "x1",
      centil_2 = "x2",
      centil_3 = "x3",
      limite_superior_do_centil = 5,
      soma_do_centil = 6,
      acumulado_do_centil = 7,
      media_do_centil = 8,
      qtd_contribuintes = "x4",
      rendim_tribut_exclusiva = "x9",
      imposto_devido = "x19",
      divids_onus = "x24"
    ) %>%
    slice(2:n()) %>%
    mutate(
      centil_2 = 0.1 * centil_2 + 99,
      centil_3 = 0.1 * centil_3 + 99
    )

  return(df)
}

read_ir_sheets <- function(path) {

  # lê todas as abas do excel

  sheets <- readxl::excel_sheets(path)

  df_ano <- purrr::map2(path, sheets, read_ir_clean) %>%
    set_names(sheets) %>%
    purrr::map2(sheets, ~ mutate(.x,
      tabela = case_when(
        str_length(.y) == 3 ~ "RTB",
        str_length(.y) == 4 ~ "RB1",
        str_length(.y) == 5 ~ "RB2"
      ),
      estado = str_sub(.y, start = 0, end = 2)
    )) %>%
    bind_rows()

  return(df_ano)
}

read_all_ir <- function(path) {

  # lê todas as abas e todos os arquivos

  ir_db <- purrr::map(path, read_ir_sheets) %>%
    purrr::set_names(2006:2020) %>%
    purrr::map2(2006:2020, ~ mutate(.x, ano = .y)) %>%
    bind_rows()
}

#### 1. PIPELINE #####

ir_db <- read_ir_url() %>%
  donwload_ir() %>%
  read_all_ir()


