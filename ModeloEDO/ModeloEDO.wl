(* ::Package:: *)

BeginPackage["ModeloEDO`"];


<< PenduloComPolia`Importador`;
<< PenduloComPolia`ModeloGeometrico`;


SolucaoNumerica::usage = "SolucaoNumerica[tempoInicial,tempoFinal,funcaoAngulo] retorna uma express\[ATilde]o para o \[AHat]ngulo, dependente de s\[IAcute]mbolo 'funcaoAngulo'";


Begin["Private`"];


(* ::Text:: *)
(*Estado interno*)


r = Pegar["raio da polia"];
L0 = Pegar["comprimento do fio"];
m = Pegar["massa oscilante"];
g = 10;


x[t_]=X[r,L0,\[Theta][t]];
y[t_]=Y[r,L0,\[Theta][t]];


(* ::Text:: *)
(*Publica\[CCedilla]\[ATilde]o*)


SolucaoNumerica[tempoInicial_, tempoFinal_,tempo_] := 
Flatten[Values[(NDSolve[{
	m*y''[t]== T[t]*Cos[\[Theta][t]]-m*g,
	m*x''[t]== -T[t]*Sin[\[Theta][t]],
	\[Theta][0]== -\[Pi]/10, (* Necess\[AAcute]rio converter para \[Theta] como definido no modelo matem\[AAcute]tico (em oposi\[CCedilla]\[ATilde]o \[AGrave] defini\[CCedilla]\[ATilde]o \[Phi] do Tracker) *)
	\[Theta]'[0]==0
	},
	{\[Theta][t],T[t]},
	{t,tempoInicial,tempoFinal}
	]//Flatten)[[1]]]]/.t->tempo;


End[];


EndPackage[];
