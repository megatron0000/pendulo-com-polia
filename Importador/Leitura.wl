(* ::Package:: *)

BeginPackage["Leitura`"];


FixarDiretorio::usage = "FixarDiretorio[diretorio] far\[AAcute] Leitura` ler todos os arquivos pedidos a partir do
diret\[OAcute]rio especificado";


LerEscalar::usage = "LerEscalar[arquivo] retorna o \[UAcute]nico n\[UAcute]mero contido em 'arquivo'";


LerLista::usage = "LerLista[arquivo] retorna a \[UAcute]nica lista contida em 'arquivo'";


(* ::Chapter:: *)
(*Interno*)


Begin["`Private`"];


<< TrackerImport`;


dir = None;


(* ::Chapter:: *)
(*Publica\[CCedilla]\[ATilde]o*)


FixarDiretorio[diretorio_] := dir=diretorio;


(* Fun\[CCedilla]\[ATilde]o de importa\[CCedilla]\[ATilde]o usada para o raio, a massa e o comprimento do fio *)
LerEscalar[arquivo_] := Flatten[Import[FileNameJoin@{dir,arquivo}, "Table"]][[1]];


LerLista[arquivo_] := TrackerImport[FileNameJoin@{dir,arquivo}];


End[];
EndPackage[];
