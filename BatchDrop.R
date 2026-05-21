#' Upload de Diretório Local para o Google Drive Mantendo a Estrutura
#'
#' Esta função espelha a hierarquia de pastas de um diretório local em uma pasta
#' de destino no Google Drive, criando subpastas recursivamente conforme necessário
#' e enviando os arquivos de forma iterativa com barra de progresso.
#'
#' @param pasta_local Caractere. Caminho absoluto ou relativo para o diretório local.
#' @param id_pasta_raiz_drive Caractere. ID da pasta raiz no Google Drive.
#' @param overwrite Lógico. Se \code{TRUE} (padrão), substitui arquivos no Drive com o mesmo nome.
#' @param verbose Lógico. Se \code{TRUE} (padrão), exibe a barra de progresso e mensagens.
#'
#' @return Retorna \code{TRUE} de forma invisível em caso de sucesso na varredura.
#'
#' @importFrom googledrive as_id drive_ls drive_mkdir drive_upload
#' @importFrom progress progress_bar
#'
#' @export
sync_dir_to_drive <- function(pasta_local, id_pasta_raiz_drive, overwrite = TRUE, verbose = TRUE) {
  
  if (!dir.exists(pasta_local)) {
    stop("O diretório local fornecido não existe ou está inacessível: ", pasta_local)
  }
  
  if (missing(id_pasta_raiz_drive) || !is.character(id_pasta_raiz_drive)) {
    stop("O parâmetro 'id_pasta_raiz_drive' deve ser fornecido como uma string de texto válida.")
  }
  
  pasta_local <- normalizePath(pasta_local, winslash = "/")
  
  # Lista todos os caminhos
  todos_caminhos <- list.files(path = pasta_local, full.names = TRUE, recursive = TRUE)
  
  # Filtra previamente para manter apenas arquivos (remove pastas da contagem)
  arquivos_validos <- todos_caminhos[!file.info(todos_caminhos)$isdir]
  
  total_arquivos <- length(arquivos_validos)
  
  if (total_arquivos == 0) {
    if (verbose) message("Nenhum arquivo encontrado no diretório especificado.")
    return(invisible(FALSE))
  }
  
  # Configura a barra de progresso
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "  Enviando [:bar] :percent | ETA: :eta | Arquivo :current de :total",
      total = total_arquivos,
      clear = FALSE,
      width = 80
    )
    # Suprime as mensagens padrão do googledrive para não quebrar a barra no console
    options(googledrive_quiet = TRUE)
    on.exit(options(googledrive_quiet = FALSE)) # Restaura ao finalizar ou dar erro
  }
  
  # Processo de organização
  for (arquivo in arquivos_validos) {
    
    arq_norm <- normalizePath(arquivo, winslash = "/")
    caminho_relativo <- sub(paste0(pasta_local, "/"), "", arq_norm)
    partes <- strsplit(caminho_relativo, "/")[[1]]
    
    pasta_destino <- id_pasta_raiz_drive
    
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
    
    tryCatch({
      googledrive::drive_upload(
        media = arquivo,
        path = googledrive::as_id(pasta_destino),
        name = partes[length(partes)],
        overwrite = overwrite
      )
    }, error = function(e) {
      warning("\nErro ao enviar '", caminho_relativo, "': ", e$message)
    })
    
    # Atualiza a barra de progresso
    if (verbose) {
      pb$tick()
    }
  }
  
  if (verbose) message("\nUpload concluído com sucesso!")
  return(invisible(TRUE))
}