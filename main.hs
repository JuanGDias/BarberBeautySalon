import Data.Char
import System.Directory
import System.IO
import Mensagens
import Util
import Cliente
import Empresa
import Membro
import Servico
import Agendamento

main :: IO ()
main = do
    putStr("\n      === Bem-vindo ao BarberBeautySalon ===\n")
    Mensagens.menuPrincipal
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    escolheOpcao opcao

escolheOpcao :: String -> IO()
escolheOpcao opcao
    | opcao == "1" = loginEmpresa main
    | opcao == "2" = loginCliente main
    | opcao == "3" = cadastrar main
    | opcao == "4" = putStrLn "----------"
    | otherwise = do {Mensagens.opcaoInvalida; main}

loginEmpresa :: (IO()) -> IO()
loginEmpresa menu = do
    putStr "\nInforme o email para login: "
    email <- Util.lerEntradaString
    putStr "\nInforme a senha para login: "
    senha <- Util.lerEntradaString

    arq <- openFile "files/empresas.txt" ReadMode
    empresasConteudo <- hGetContents arq
    let empresas = lines empresasConteudo
    let temEmpresa = Util.encontraEmpresaEmailSenha [read x :: Empresa | x <- empresas] email senha

    if temEmpresa
        then do {menuDeEmpresa menu}
    else do
        {Mensagens.usuarioInvalido; menu}

loginCliente :: (IO()) -> IO()
loginCliente menu = do
    putStr "\nInforme o email para login: "
    email <- Util.lerEntradaString
    putStr "\nInforme a senha para login: "
    senha <- Util.lerEntradaString
    
    arq <- openFile "files/clientes.txt" ReadMode
    clientesConteudo <- hGetContents arq
    let clientes = lines clientesConteudo
    let temCliente = Util.encontraClienteEmailSenha [read x :: Cliente | x <- clientes] email senha

    if temCliente
        then do {menuDeCliente menu}
    else do
        {Mensagens.usuarioInvalido; menu}

menuDeEmpresa :: (IO()) -> IO()
menuDeEmpresa menu = do
    Mensagens.menuEmpresa
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do menuDeMembros menu
    else if opcao == "2"
        then do menuDeServicos menu
    else if opcao == "3"
        then do menuDeAgendamentos menu
    else if opcao == "4"
        then do menu
    else do
        {Mensagens.opcaoInvalida; menu}

menuDeCliente :: (IO()) -> IO()
menuDeCliente menu = do
    Mensagens.menuCliente
    putStr("Opção: ")
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do {listarAgendamentos menu; menuDeCliente menu}
    else if opcao == "2"
        then do {agendarServico menu; menuDeCliente menu}
    else if opcao == "3"
        then do menu
    else do {Mensagens.opcaoInvalida; menuDeCliente menu}

agendarServico :: (IO()) -> IO()
agendarServico menu = do
    putStr "\n Digite o número de identificação do agendamento: "
    numIdent <- Util.lerEntradaString
    putStr "\n Digite o número do cpf do cliente: "
    cpf <- Util.lerEntradaString

    agendamentosConteudo <- readFile "files/agendamentos.txt"
    let agendamentos = lines agendamentosConteudo
    let dadosDoAgendamento = Util.encontraAgendamentoNumIdent [read x :: Agendamento | x <- agendamentos] numIdent
    
    let empresa = getAgendamentoEmpresa dadosDoAgendamento
    let servico = getAgendamentoServico dadosDoAgendamento
    let dia = getAgendamentoDia dadosDoAgendamento
    let hora = getAgendamentoHora dadosDoAgendamento
    let status = getAgendamentoStatus dadosDoAgendamento

    arq <- openFile "files/agendamentos.txt" ReadMode
    agendamentosConteudo2 <- hGetContents arq
    let agendamentos2 = lines agendamentosConteudo2

    removeFile "files/agendamentos.txt"
    let novaListaDeAgendamentos = [read x :: Agendamento | x <- agendamentos2, Util.getAgendamentoNumIdent (read x :: Agendamento) /= numIdent]
    atualizarArqAgendamentos novaListaDeAgendamentos
    
    let newAgendamento = Agendamento numIdent empresa cpf servico dia hora status
    file <- appendFile "files/agendamentos.txt" ("\n" ++ show newAgendamento)
    putStrLn "\nOperação realizada com sucesso!"

menuDeMembros :: (IO()) -> IO()
menuDeMembros menu = do
    Mensagens.menuMembros
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do {cadastrarMembro menu; menuDeMembros menu}
    else if opcao == "2"
        then do {listarMembros menu; menuDeMembros menu}
    else if opcao == "3"
        then do {atualizarMembro menu; menuDeMembros menu}  
    else if opcao == "4"
        then do {removerMembro menu; menuDeMembros menu} 
    else if opcao == "5"
        then do menuDeEmpresa menu  
    else do
        {Mensagens.opcaoInvalida; menuDeMembros menu}

cadastrarMembro :: (IO()) -> IO()
cadastrarMembro menu = do
    putStr "\n Digite o nome do membro: "
    nome <- Util.lerEntradaString
    putStr "\n Digite o cpf do membro: "
    cpf <- Util.lerEntradaString    
    putStr "\n Digite o nome da empresa em que o membro faz parte: "
    empresa <- Util.lerEntradaString

    arq <- openFile "files/membrosEmpresas.txt" ReadMode
    membrosConteudo <- hGetContents arq
    let membros = lines membrosConteudo
    let temMembro = Util.encontraMembroCPF [read x :: Membro | x <- membros] cpf

    if temMembro
        then do {Mensagens.membroCadastrado; menuDeMembros menu}
    else do
        let newMembro = Membro nome cpf empresa
        file <- appendFile "files/membrosEmpresas.txt" ("\n" ++ show newMembro)
        putStrLn "\nOperação realizada com sucesso!"

listarMembros :: (IO()) -> IO()
listarMembros menu = do
    arq <- openFile "files/membrosEmpresas.txt" ReadMode
    membrosConteudo <- hGetContents arq
    let membros = "\n" ++ membrosConteudo ++ "\n"
    putStr membros

atualizarMembro :: (IO()) -> IO()
atualizarMembro menu = do
    putStr "\nInsira o cpf do membro: "
    cpf <- Util.lerEntradaString

    arq <- openFile "files/membrosEmpresas.txt" ReadMode
    membrosConteudo <- hGetContents arq
    let membros = lines membrosConteudo
    let temMembro = Util.encontraMembroCPF [read x :: Membro | x <- membros] cpf

    if temMembro
        then do
            removeFile "files/membrosEmpresas.txt"
            let novaListaDeMembros = [read x :: Membro | x <- membros, Util.getMembroCPF (read x :: Membro) /= cpf]
            atualizarArqMembros novaListaDeMembros
            putStrLn "Informe os novos dados do membro:"
            cadastrarMembro menu
    else do
        putStrLn ("\nMembro não existente!")

removerMembro :: (IO()) -> IO()
removerMembro menu = do
    putStr "\nInsira o cpf do membro: "
    cpf <- Util.lerEntradaString

    arq <- openFile "files/membrosEmpresas.txt" ReadMode
    membrosConteudo <- hGetContents arq
    let membros = lines membrosConteudo
    let temMembro = Util.encontraMembroCPF [read x :: Membro | x <- membros] cpf

    if temMembro
        then do
            removeFile "files/membrosEmpresas.txt"
            let novaListaDeMembros = [read x :: Membro | x <- membros, Util.getMembroCPF (read x :: Membro) /= cpf]
            atualizarArqMembros novaListaDeMembros
            putStrLn "\nOperação realizada com sucesso!"
    else do
        putStrLn ("\nMembro não existente!")    

atualizarArqMembros :: [Membro] -> IO ()
atualizarArqMembros [] = putStrLn "Lista vazia"
atualizarArqMembros (x : xs) = do
  membrosCadastrados <- doesFileExist "files/membrosEmpresas.txt"
  if not membrosCadastrados
    then do
      file <- openFile "files/membrosEmpresas.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
  else appendFile "files/membrosEmpresas.txt" ("\n" ++ show x)
  atualizarArqMembros xs

menuDeServicos :: (IO()) -> IO()
menuDeServicos menu = do
    Mensagens.menuServicos
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do {cadastrarServico menu; menuDeServicos menu}
    else if opcao == "2"
        then do {listarServicos menu; menuDeServicos menu}
    else if opcao == "3"
        then do {atualizarServico menu; menuDeServicos menu}
    else if opcao == "4"
        then do {removerServico menu; menuDeServicos menu}
    else if opcao == "5"
        then do menuDeEmpresa menu  
    else do
        {Mensagens.opcaoInvalida; menuDeServicos menu}

cadastrarServico :: (IO()) -> IO()
cadastrarServico menu = do
    putStr "\n Digite o nome do serviço: "
    nome <- Util.lerEntradaString
    putStr "\n Digite uma descrição para o serviço: "
    descricao <- Util.lerEntradaString 
    putStr "\n Digite o nome da empresa que presta o serviço: "
    empresa <- Util.lerEntradaString

    arq <- openFile "files/servicos.txt" ReadMode
    servicosConteudo <- hGetContents arq
    let servicos = lines servicosConteudo
    let temServico = Util.encontraServicoNomeEmpresa [read x :: Servico | x <- servicos] nome empresa

    if temServico
        then do {Mensagens.servicoCadastrado; menuDeServicos menu}
    else do
        let newServico = Servico nome descricao empresa
        file <- appendFile "files/servicos.txt" ("\n" ++ show newServico)
        putStrLn "\nOperação realizada com sucesso!"
        menuDeServicos menu

listarServicos :: (IO()) -> IO()
listarServicos menu = do
    arq <- openFile "files/servicos.txt" ReadMode
    servicosConteudo <- hGetContents arq
    let servicos = "\n" ++ servicosConteudo ++ "\n"
    putStr servicos

atualizarServico :: (IO()) -> IO()
atualizarServico menu = do
    putStr "\n Digite o nome do serviço: "
    nome <- Util.lerEntradaString  
    putStr "\n Digite o nome da empresa que presta o serviço: "
    empresa <- Util.lerEntradaString

    arq <- openFile "files/servicos.txt" ReadMode
    servicosConteudo <- hGetContents arq
    let servicos = lines servicosConteudo
    let temServico = Util.encontraServicoNomeEmpresa [read x :: Servico | x <- servicos] nome empresa

    if temServico
        then do
            removeFile "files/servicos.txt"
            let novaListaDeServicos = [read x :: Servico | x <- servicos, Util.getServicoNome (read x :: Servico) /= nome, Util.getServicoEmpresa (read x :: Servico) /= empresa]
            atualizarArqServicos novaListaDeServicos
            putStrLn "Informe os novos dados do serviço:"
            cadastrarServico menu
    else do
        putStrLn ("\nServiço não existente!")

removerServico :: (IO()) -> IO()
removerServico menu = do
    putStr "\n Digite o nome do serviço: "
    nome <- Util.lerEntradaString  
    putStr "\n Digite o nome da empresa que presta o serviço: "
    empresa <- Util.lerEntradaString

    arq <- openFile "files/servicos.txt" ReadMode
    servicosConteudo <- hGetContents arq
    let servicos = lines servicosConteudo
    let temServico = Util.encontraServicoNomeEmpresa [read x :: Servico | x <- servicos] nome empresa

    if temServico
        then do
            removeFile "files/servicos.txt"
            let novaListaDeServicos = [read x :: Servico | x <- servicos, Util.getServicoNome (read x :: Servico) /= nome, Util.getServicoEmpresa (read x :: Servico) /= empresa]
            atualizarArqServicos novaListaDeServicos
            putStrLn "\nOperação realizada com sucesso!"
    else do
        putStrLn ("\nServiço não existente!")    

atualizarArqServicos :: [Servico] -> IO ()
atualizarArqServicos [] = putStrLn "Lista vazia"
atualizarArqServicos (x : xs) = do
  servicosCadastrados <- doesFileExist "files/servicos.txt"
  if not servicosCadastrados
    then do
      file <- openFile "files/servicos.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
  else appendFile "files/servicos.txt" ("\n" ++ show x)
  atualizarArqServicos xs

menuDeAgendamentos :: (IO()) -> IO()
menuDeAgendamentos menu = do
    Mensagens.menuAgendamentos
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do {cadastrarAgendamento menu; menuDeAgendamentos menu}
    else if opcao == "2"
        then do {listarAgendamentos menu; menuDeAgendamentos menu}
    else if opcao == "3"
        then do {menuDeModificarStatus menu; menuDeAgendamentos menu} 
    else if opcao == "4"
        then do menuDeEmpresa menu 
    else do {Mensagens.opcaoInvalida; menuDeEmpresa menu}

cadastrarAgendamento :: (IO()) -> IO()
cadastrarAgendamento menu = do
    putStr "\n Digite o número de identificação do agendamento: "
    numIdent <- Util.lerEntradaString
    putStr "\n Digite o nome da empresa: "
    empresa <- Util.lerEntradaString
    putStr "\n Digite o nome do serviço: "
    servico <- Util.lerEntradaString
    putStr "\n Digite o dia do serviço: "
    dia <- Util.lerEntradaString
    putStr "\n Digite a hora do serviço: "
    hora <- Util.lerEntradaString

    arq <- openFile "files/agendamentos.txt" ReadMode
    agendamentosConteudo <- hGetContents arq
    let agendamentos = lines agendamentosConteudo
    let temAgendamento = Util.verificaAgendamentoNumIdent [read x :: Agendamento | x <- agendamentos] numIdent

    if temAgendamento
        then do {Mensagens.agendamentoCadastrado; menuDeServicos menu}
    else do
        let newAgendamento = Agendamento numIdent empresa "" servico dia hora "Nao iniciado"
        file <- appendFile "files/agendamentos.txt" ("\n" ++ show newAgendamento)
        putStrLn "\nOperação realizada com sucesso!"
        menuDeAgendamentos menu

listarAgendamentos :: (IO()) -> IO()
listarAgendamentos menu = do
    arq <- openFile "files/agendamentos.txt" ReadMode
    agendamentosConteudo <- hGetContents arq
    let agendamentos = "\n" ++ agendamentosConteudo ++ "\n"
    putStr agendamentos

menuDeModificarStatus :: (IO()) -> IO()
menuDeModificarStatus menu = do
    Mensagens.menuStatusAgendamento
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    if opcao == "1"
        then do {modificaStatusEmAndamento menu; menuDeModificarStatus menu}
    else if opcao == "2"
        then do {modificaStatusFinalizado menu; menuDeModificarStatus menu}
    else if opcao == "3"
        then do menuDeAgendamentos menu
    else do {Mensagens.opcaoInvalida; menuDeModificarStatus menu}

modificaStatusEmAndamento :: (IO()) -> IO()
modificaStatusEmAndamento menu = do 
    putStr "\nInforme qual o número de identificação do agendamento: "
    numIdent <- Util.lerEntradaString
    modificaStatus menu numIdent "Em andamento"

modificaStatusFinalizado :: (IO()) -> IO()
modificaStatusFinalizado menu = do
    putStr "\nInforme qual o número de identificação do agendamento: "
    numIdent <- Util.lerEntradaString
    modificaStatus menu numIdent "Finalizado"

modificaStatus :: (IO()) -> String -> String -> IO()
modificaStatus menu numIdent status = do
    agendamentosConteudo <- readFile "files/agendamentos.txt"
    let agendamentos = lines agendamentosConteudo
    let dadosDoAgendamento = Util.encontraAgendamentoNumIdent [read x :: Agendamento | x <- agendamentos] numIdent
    
    let empresa = getAgendamentoEmpresa dadosDoAgendamento
    let cliente = getAgendamentoCliente dadosDoAgendamento
    let servico = getAgendamentoServico dadosDoAgendamento
    let dia = getAgendamentoDia dadosDoAgendamento
    let hora = getAgendamentoHora dadosDoAgendamento

    arq <- openFile "files/agendamentos.txt" ReadMode
    agendamentosConteudo2 <- hGetContents arq
    let agendamentos2 = lines agendamentosConteudo2

    removeFile "files/agendamentos.txt"
    let novaListaDeAgendamentos = [read x :: Agendamento | x <- agendamentos2, Util.getAgendamentoNumIdent (read x :: Agendamento) /= numIdent]
    atualizarArqAgendamentos novaListaDeAgendamentos

    let newAgendamento = Agendamento numIdent empresa cliente servico dia hora status
    file <- appendFile "files/agendamentos.txt" ("\n" ++ show newAgendamento)
    putStrLn "\nOperação realizada com sucesso!"

atualizarArqAgendamentos :: [Agendamento] -> IO ()
atualizarArqAgendamentos [] = putStrLn "Lista vazia"
atualizarArqAgendamentos (x : xs) = do
  agendamentosCadastrados <- doesFileExist "files/agendamentos.txt"
  if not agendamentosCadastrados
    then do
      file <- openFile "files/agendamentos.txt" WriteMode
      hPutStr file (show x)
      hFlush file
      hClose file
  else appendFile "files/agendamentos.txt" ("\n" ++ show x)
  atualizarArqAgendamentos xs

cadastrar :: (IO()) -> IO()
cadastrar menu = do
    Mensagens.menuCadastro
    putStr("Opção: ")    
    opcao <- Util.lerEntradaString
    escolheCadastro menu opcao

escolheCadastro :: (IO()) -> String -> IO()
escolheCadastro menu opcao 
    | opcao == "1" = do {cadastrarEmpresa menu; menu}
    | opcao == "2" = do {cadastrarCliente menu; menu}
    | opcao == "3" = menu
    | otherwise = do {Mensagens.opcaoInvalida; cadastrar menu}

cadastrarEmpresa :: (IO()) -> IO()
cadastrarEmpresa menu = do
    putStr "\nInforme o nome da empresa: "
    nomeEmpresa <- Util.lerEntradaString
    putStr "\nInforme o email da empresa: "
    emailEmpresa <- Util.lerEntradaString
    putStr "\nInforme o telefone da empresa: "
    telefoneEmpresa <- Util.lerEntradaString
    putStr "\nInforme a senha da empresa: "
    senhaEmpresa <- Util.lerEntradaString

    arq <- openFile "files/empresas.txt" ReadMode
    empresasConteudo <- hGetContents arq
    let empresas = lines empresasConteudo
    let temEmpresa = Util.encontraEmpresaEmail [read x :: Empresa | x <- empresas] emailEmpresa

    if temEmpresa
        then do Mensagens.empresaCadastrada
    else do
        let newEmpresa = Empresa nomeEmpresa emailEmpresa telefoneEmpresa senhaEmpresa
        file <- appendFile "files/empresas.txt" ("\n" ++ show newEmpresa)
        putStrLn "\nEmpresa cadastrada com sucesso!"

cadastrarCliente :: (IO()) -> IO()
cadastrarCliente menu = do
    putStr "\nInforme o nome do cliente: "
    nome <- Util.lerEntradaString
    putStr "\nInforme o email do cliente: "
    email <- Util.lerEntradaString
    putStr "\nInforme o cpf do cliente: "
    cpf <- Util.lerEntradaString
    putStr "\nInforme o telefone do cliente: "
    telefone <- Util.lerEntradaString
    putStr "\nInforme a senha do cliente: "
    senha <- Util.lerEntradaString

    arq <- openFile "files/clientes.txt" ReadMode
    clientesConteudo <- hGetContents arq
    let clientes = lines clientesConteudo
    let temCliente = Util.encontraClienteCPF [read x :: Cliente | x <- clientes] cpf

    if temCliente
        then do Mensagens.clienteCadastrado
    else do
        let newCliente = Cliente nome email cpf telefone senha
        file <- appendFile "files/clientes.txt" ("\n" ++ show newCliente)
        putStrLn "\nCliente cadastrado com sucesso!"