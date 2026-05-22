#' Upload de Diretório Local para o Google Drive Mantendo a Estrutura
#'
#' Esta função espelha a hierarquia de pastas de um diretório local em uma pasta
#' de destino no Google Drive, criando subpastas recursivamente conforme necessário
#' e enviando os arquivos de forma iterativa com barra de progresso.
#'
#' @param pasta_local Caractere. Caminho absoluto ou relativo para o diretório local.
#' @param id_pasta_raiz_drive Caractere. ID da pasta raiz no Google Drive.
#' @param overwrite Lógico. Se \code{TRUE}, substitui arquivos no Drive se eles forem enviados.
#' @param skip_existing Lógico. Se \code{TRUE} (padrão), pula arquivos que já existem no Drive, economizando tempo de upload.
#' @param verbose Lógico. Se \code{TRUE} (padrão), exibe a barra de progresso e mensagens.
#'
#' @return Retorna \code{TRUE} de forma invisível em caso de sucesso na varredura.
#'
#' @importFrom googledrive as_id drive_ls drive_mkdir drive_upload
#' @importFrom progress progress_bar
#'
#' @export
sync_dir_to_drive <- function(pasta_local, id_pasta_raiz_drive, overwrite = TRUE, skip_existing = TRUE, verbose = TRUE) {

  if (!dir.exists(pasta_local)) {
    stop("The provided local directory does not exist or is inaccessible: ", pasta_local)
  }

  if (missing(id_pasta_raiz_drive) || !is.character(id_pasta_raiz_drive)) {
    stop("The 'id_pasta_raiz_drive' parameter must be provided as a valid character string.")
  }

  # Normaliza o caminho
  pasta_local <- normalizePath(pasta_local, winslash = "/")
  pasta_local <- sub("/$", "", pasta_local)

  # Lista todos os caminhos
  todos_caminhos <- list.files(path = pasta_local, full.names = TRUE, recursive = TRUE)

  # Coleta informações dos arquivos de forma segura
  info_arquivos <- file.info(todos_caminhos)
  eh_diretorio <- info_arquivos$isdir

  # Identifica arquivos problemáticos
  arquivos_problematicos <- todos_caminhos[is.na(eh_diretorio)]

  if (length(arquivos_problematicos) > 0) {
    warning("\nO R não conseguiu acessar os seguintes arquivos (verifique se o caminho é muito longo ou se há caracteres especiais):\n",
            paste(head(arquivos_problematicos, 10), collapse = "\n"),
            if(length(arquivos_problematicos) > 10) "\n... e outros.")
  }

  # Filtra garantindo que não é diretório E não é NA
  arquivos_validos <- todos_caminhos[!is.na(eh_diretorio) & !eh_diretorio]

  total_arquivos <- length(arquivos_validos)

  if (total_arquivos == 0) {
    if (verbose) message("No valid files found in the specified directory.")
    return(invisible(FALSE))
  }

  # Configura a barra de progresso
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "  Uploading [:bar] :percent | ETA: :eta | File :current of :total",
      total = total_arquivos,
      clear = FALSE,
      width = 80
    )
    options(googledrive_quiet = TRUE)
    on.exit(options(googledrive_quiet = FALSE))
  }

  # Processo de organização
  for (arquivo in arquivos_validos) {

    arq_norm <- normalizePath(arquivo, winslash = "/")
    caminho_relativo <- sub(paste0(pasta_local, "/"), "", arq_norm, fixed = TRUE)
    partes <- strsplit(caminho_relativo, "/")[[1]]

    nome_do_arquivo <- partes[length(partes)]
    pasta_destino <- id_pasta_raiz_drive

    # Criação/Navegação pelas subpastas
    if (length(partes) > 1) {
      for (i in 1:(length(partes) - 1)) {
        nome_subpasta <- partes[i]

        conteudo_pasta_atual <- googledrive::drive_ls(path = googledrive::as_id(pasta_destino), type = "folder")
        subpasta_encontrada <- conteudo_pasta_atual[conteudo_pasta_atual$name == nome_subpasta, ]

        if (nrow(subpasta_encontrada) == 0) {
          nova_pasta <- googledrive::drive_mkdir(nome_subpasta, path = googledrive::as_id(pasta_destino))
          pasta_destino <- nova_pasta$id
        } else {
          pasta_destino <- subpasta_encontrada$id[1]
        }
      }
    }

    # ==========================================
    # VERIFICA SE O ARQUIVO JÁ EXISTE NO DRIVE
    # ==========================================
    fazer_upload <- TRUE

    if (skip_existing) {
      # Lista os arquivos existentes na pasta de destino atual
      arquivos_no_drive <- googledrive::drive_ls(path = googledrive::as_id(pasta_destino), type = "file")

      # Se o nome do arquivo já estiver lá, não faz o upload
      if (nome_do_arquivo %in% arquivos_no_drive$name) {
        fazer_upload <- FALSE
      }
    }

    # Executa o upload apenas se o arquivo não existir (ou se skip_existing for FALSE)
    if (fazer_upload) {
      tryCatch({
        googledrive::drive_upload(
          media = arquivo,
          path = googledrive::as_id(pasta_destino),
          name = nome_do_arquivo,
          overwrite = overwrite
        )
      }, error = function(e) {
        warning("\nError uploading '", caminho_relativo, "': ", e$message)
      })
    }

    # Atualiza a barra de progresso
    if (verbose) {
      pb$tick()
    }
  }

  if (verbose) message("\nUpload completed successfully!")
  return(invisible(TRUE))
}
