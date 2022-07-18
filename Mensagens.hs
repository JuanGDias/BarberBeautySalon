module Mensagens where
import Data.List
import System.IO

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn("\n           -----MENU PRINCIPAL-----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Login Empresa")
    putStrLn("[2] Login Cliente")
    putStrLn("[3] Cadastro")
    putStrLn("[4] Sair\n")

menuCadastro :: IO ()
menuCadastro = do
    putStrLn("\n           -----MENU CADASTRO-----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Cadastro de empresa")
    putStrLn("[2] Cadastro de cliente")
    putStrLn("[3] Sair\n")

menuEmpresa :: IO()
menuEmpresa = do
    putStrLn("\n    ---- EMPRESA ----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Membros")
    putStrLn("[2] Serviços")
    putStrLn("[3] Agendamentos")
    putStrLn("[4] Voltar ao menu principal\n")

menuMembros :: IO()
menuMembros = do
    putStrLn("\n    ---- MEMBROS ----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Cadastrar membro na empresa")
    putStrLn("[2] Listar membros da empresa")
    putStrLn("[3] Atualizar membro da empresa")
    putStrLn("[4] Remover membro da empresa")
    putStrLn("[5] Voltar ao menu principal\n")

menuServicos :: IO()
menuServicos = do
    putStrLn("\n    ---- SERVIÇOS ----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Cadastrar serviço da empresa")
    putStrLn("[2] Listar serviços da empresa")
    putStrLn("[3] Atualizar serviço da empresa")
    putStrLn("[4] Remover serviço da empresa")
    putStrLn("[5] Voltar ao menu principal\n")

menuAgendamentos :: IO()
menuAgendamentos = do
    putStrLn("\n    ---- AGENDAMENTOS ----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Cadastrar agendamento")
    putStrLn("[2] Visualizar agendamentos")
    putStrLn("[3] Modificar status do agendamento")
    putStrLn("[4] Voltar ao menu principal\n")

menuStatusAgendamento :: IO()
menuStatusAgendamento = do
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Modificar status do agendamento para em andamento")
    putStrLn("[2] Modificar status do agendamento para finalizado")
    putStrLn("[4] Voltar ao menu principal\n")

menuCliente :: IO()
menuCliente = do
    putStrLn("\n    ---- CLIENTE ----")
    putStrLn("\nComo deseja prosseguir?")
    putStrLn("[1] Listar agendamentos")
    putStrLn("[2] Agendar serviço")
    putStrLn("[3] Voltar ao menu principal\n")

empresaCadastrada :: IO()
empresaCadastrada = do
    putStrLn("\n Empresa já cadastrada!")

membroCadastrado :: IO()
membroCadastrado = do
    putStrLn("\n Membro já cadastrado!")

servicoCadastrado :: IO()
servicoCadastrado = do
    putStrLn("\n Serviço já cadastrado!")

clienteCadastrado :: IO()
clienteCadastrado = do
    putStrLn("\n Cliente já cadastrado!")

agendamentoCadastrado :: IO()
agendamentoCadastrado = do
    putStrLn("\n Agendamento já cadastrado!")

opcaoInvalida :: IO()
opcaoInvalida = do
     putStrLn("\nError: OPÇÃO INVÁLIDA\n")

usuarioInvalido :: IO()
usuarioInvalido = do
    putStrLn("\nUsuário Inválido!")