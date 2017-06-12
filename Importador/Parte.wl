(* ::Package:: *)

BeginPackage["Parte`"];


Parte::usage="Parte[lista, parte1, ...]. Exemplo: Parte[listaCinematica, 'x', 'r']";
Formato::usage = "Formato[] retorna as associa\[CCedilla]\[OTilde]es chave->\[IAcute]ndice definidas internamente";


(* ::Chapter:: *)
(*Interno*)


Begin["`Private`"];


(* ::Chapter:: *)
(*Publica\[CCedilla]\[ATilde]o*)


Formato[] = {"t"->1, "x"->2,"y"->3,"r"->4,"\[Phi]"->5,"\[Theta]"->6};


(* Fun\[CCedilla]\[ATilde]o para acesso por chave, em vez de por \[IAcute]ndice *)
Parte[lista_, parte_]:=lista[[parte/.Formato[]]];


End[];
EndPackage[];
