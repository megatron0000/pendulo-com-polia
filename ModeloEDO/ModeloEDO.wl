(* ::Package:: *)

BeginPackage["ModeloEDO`"];


SolucaoNumerica::usage = "SolucaoNumerica[tomada,{angulo[tempo1]== anguloInicial, angulo'[tempo1]==velocidadeInicial},{tempo,tempoInicial, tempoFinal}] 
retorna uma express\[ATilde]o para o \[AHat]ngulo em fun\[CCedilla]\[ATilde]o do s\[IAcute]mbolo 'tempo' fornecido";
g::usage="Acelera\[CCedilla]\[ATilde]o gravitacional (n\[UAcute]mero)";


(* ::Chapter:: *)
(*Interno*)


Begin["Private`"];


<< PenduloComPolia`Importador`;
<< PenduloComPolia`ModeloGeometrico`;


r = Pegar[1, "raio da polia"]
For[i=1, i<= QuantasTomadas[], i=i+1, L0[i] = Pegar[i,"comprimento do fio"]]
m = Pegar[1, "massa oscilante"]
g = 9.784


x[t_, tomada_]=X[r,L0[tomada],\[Theta][t]]
y[t_,tomada_]=Y[r,L0[tomada],\[Theta][t]]


(* ::Text:: *)
(*Publica\[CCedilla]\[ATilde]o*)


SolucaoNumerica[
tomada_,
{angulo_[tempo1_]== anguloInicial_, angulo_'[tempo1_]==velocidadeInicial_},
{tempo_,tempoInicial_, tempoFinal_}] := 
Flatten[Values[(NDSolve[{
	(* \[Theta]''[t]== (r*\[Theta]'[t]^2-g*Sin[\[Theta][t]])/(L0[tomada]-r*\[Theta][t]), *)
	m*D[y[t,tomada],{t,2}]== T[t]*Cos[\[Theta][t]]-m*g,
	m*D[x[t,tomada],{t,2}]== -T[t]*Sin[\[Theta][t]],
	\[Theta][tempo1]== anguloInicial,
	\[Theta]'[tempo1]== velocidadeInicial
	},
	{\[Theta][t], T[t]},
	{t,tempoInicial,tempoFinal}
	]//Flatten)[[1]]]]/.t->tempo


End[];


EndPackage[];
