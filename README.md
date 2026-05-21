
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BatchDrop

<!-- badges: start -->

<!-- badges: end -->

O `BatchDrop` fornece ferramentas eficientes para sincronização e
manipulação em lote de arquivos. Seu objetivo principal é simplificar
fluxos de trabalho que exigem o espelhamento de estruturas complexas de
diretórios locais diretamente para serviços de nuvem, como o Google
Drive, mantendo a hierarquia original intacta.

# Instalação

Como o pacote ainda não está no CRAN, você pode instalar a versão de
desenvolvimento do BatchDrop diretamente do GitHub usando:

``` r
# install.packages("pak")
pak::pak("Natanaelsl/BatchDrop")
```

# Dependências

O `BatchDrop` utiliza a API do Google Drive por baixo dos panos. Ao usar
as funções pela primeira vez, o pacote `googledrive()` solicitará
autenticação no seu navegador.

# Exemplo de Uso

Este é um exemplo básico de como espelhar uma pasta local inteira para o
Google Drive. A função varre recursivamente o diretório, recria as
subpastas no Drive (se não existirem) e faz o upload iterativo dos
arquivos com uma barra de progresso.

# Licença

Este projeto está licenciado sob os termos da licença MIT.
