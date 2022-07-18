module Util where
import System.IO
import Data.List 
import Cliente
import Empresa
import Membro
import Servico
import Agendamento

lerEntradaString :: IO String
lerEntradaString = do
    x <- getLine
    return x

encontraEmpresaEmail :: [Empresa] -> String -> Bool
encontraEmpresaEmail [] email = False
encontraEmpresaEmail (x : xs) email
  | verificarEmpresa x "emailEmpresa" == email = True
  | verificarEmpresa x "emailEmpresa" /= email = encontraEmpresaEmail xs email

encontraEmpresaEmailSenha :: [Empresa] -> String -> String -> Bool
encontraEmpresaEmailSenha [] email senha = False
encontraEmpresaEmailSenha (x : xs) email senha
  | verificarEmpresa x "emailEmpresa" == email && verificarEmpresa x "senhaEmpresa" == senha = True
  | verificarEmpresa x "emailEmpresa" /= email || verificarEmpresa x "senhaEmpresa" /= senha = encontraEmpresaEmailSenha xs email senha

verificarEmpresa :: Empresa -> String -> String
verificarEmpresa Empresa {Empresa.nomeEmpresa = nome, Empresa.emailEmpresa = email, Empresa.telefoneEmpresa = telefone, Empresa.senhaEmpresa = senha} p
  | p == "emailEmpresa" = email
  | p == "senhaEmpresa" = senha

encontraClienteCPF :: [Cliente] -> String -> Bool
encontraClienteCPF [] cpf = False
encontraClienteCPF (x : xs) cpf
  | verificarCliente x "cpfCliente" == cpf = True
  | verificarCliente x "cpfCliente" /= cpf = encontraClienteCPF xs cpf

encontraClienteEmailSenha :: [Cliente] -> String -> String -> Bool
encontraClienteEmailSenha [] email senha = False
encontraClienteEmailSenha (x : xs) email senha
  | verificarCliente x "emailCliente" == email && verificarCliente x "senhaCliente" == senha = True
  | verificarCliente x "emailCliente" /= email || verificarCliente x "senhaCliente" /= senha = encontraClienteEmailSenha xs email senha

verificarCliente :: Cliente -> String -> String
verificarCliente Cliente {Cliente.nomeCliente = nome, Cliente.emailCliente = email, Cliente.cpfCliente = cpf, Cliente.telefoneCliente = telefone, Cliente.senhaCliente = senha} p
  | p == "emailCliente" = email
  | p == "cpfCliente" = cpf
  | p == "senhaCliente" = senha

encontraMembroCPF :: [Membro] -> String -> Bool
encontraMembroCPF [] cpf = False
encontraMembroCPF (x : xs) cpf
  | verificarMembro x "cpfMembro" == cpf = True
  | verificarMembro x "cpfMembro" /= cpf = encontraMembroCPF xs cpf

verificarMembro :: Membro -> String -> String
verificarMembro Membro {Membro.nomeMembro = nome, Membro.cpfMembro = cpf, Membro.nomeEmpresa = empresa} p
  | p == "cpfMembro" = cpf

encontraServicoNomeEmpresa :: [Servico] -> String -> String -> Bool
encontraServicoNomeEmpresa [] nome empresa = False
encontraServicoNomeEmpresa (x : xs) nome empresa
  | verificarServico x "nomeServico" == nome && verificarServico x "nomeEmpresa" == empresa = True
  | verificarServico x "nomeServico" /= nome || verificarServico x "nomeEmpresa" /= empresa = encontraServicoNomeEmpresa xs nome empresa

verificarServico :: Servico -> String -> String
verificarServico Servico {Servico.nomeServico = nome, Servico.descricaoServico = descricao, Servico.nomeEmpresa = empresa} p
  | p == "nomeServico" = nome
  | p == "nomeEmpresa" = empresa

encontraAgendamentoNumIdent :: [Agendamento] -> String -> Agendamento
encontraAgendamentoNumIdent (x : xs) num
  | verificarAgendamento x "numIdent" == num = x
  | verificarAgendamento x "numIdent" /= num = encontraAgendamentoNumIdent xs num

verificaAgendamentoNumIdent :: [Agendamento] -> String -> Bool
verificaAgendamentoNumIdent [] num = False
verificaAgendamentoNumIdent (x : xs) num
  | verificarAgendamento x "numIdent" == num = True
  | verificarAgendamento x "numIdent" /= num = verificaAgendamentoNumIdent xs num

verificarAgendamento :: Agendamento -> String -> String
verificarAgendamento Agendamento {Agendamento.numIdent = ident, Agendamento.nomeEmpresa = nome, Agendamento.cpfCliente = cpf, Agendamento.nomeServico = servico, Agendamento.diaServico = dia, Agendamento.horaServico = hora, Agendamento.statusServico = status} prop
  | prop == "numIdent" = ident

getServicoNome :: Servico -> String
getServicoNome (Servico nomeServico _ _) = nomeServico 

getServicoEmpresa :: Servico -> String
getServicoEmpresa (Servico _ _ nomeEmpresa) = nomeEmpresa 

getMembroCPF :: Membro -> String
getMembroCPF (Membro _ cpfMembro _) = cpfMembro

obterServicoNome :: Servico -> String
obterServicoNome Servico {Servico.nomeServico = c, Servico.descricaoServico = e, Servico.nomeEmpresa = s} = c

obterServicoEmpresa :: Servico -> String
obterServicoEmpresa Servico {Servico.nomeServico = c, Servico.descricaoServico = e, Servico.nomeEmpresa = s} = e

getAgendamentoNumIdent :: Agendamento -> String
getAgendamentoNumIdent (Agendamento numIdent _ _ _ _ _ _) = numIdent 

getAgendamentoEmpresa :: Agendamento -> String
getAgendamentoEmpresa (Agendamento _ nomeEmpresa _ _ _ _ _) = nomeEmpresa 

getAgendamentoCliente :: Agendamento -> String
getAgendamentoCliente (Agendamento _ _ cpfCliente _ _ _ _) = cpfCliente

getAgendamentoServico :: Agendamento -> String
getAgendamentoServico (Agendamento _ _ _ nomeServico _ _ _) = nomeServico 

getAgendamentoDia :: Agendamento -> String
getAgendamentoDia (Agendamento _ _ _ _ diaServico _ _) = diaServico 

getAgendamentoHora :: Agendamento -> String
getAgendamentoHora (Agendamento _ _ _ _ _ horaServico _) = horaServico 

getAgendamentoStatus :: Agendamento -> String
getAgendamentoStatus (Agendamento _ _ _ _ _ _ statusServico) = statusServico 