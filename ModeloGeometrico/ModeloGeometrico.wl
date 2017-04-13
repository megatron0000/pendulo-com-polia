(* ::Package:: *)

BeginPackage["ModeloGeometrico`"];


X::usage = "X[raioPolia,comprimentoFio,funcaoAngulo] retorna uma fun\[CCedilla]\[ATilde]o que associa a cada tempo 't' uma posi\[CCedilla]\[ATilde]o 'x' da massa oscilante (origem no centro da polia)";


Y::usage = "Y[raioPolia,comprimentoFio,funcaoAngulo] retorna uma fun\[CCedilla]\[ATilde]o que associa a cada tempo 't' uma posi\[CCedilla]\[ATilde]o 'y' da massa oscilante (origem no centro da polia)";


Begin["Private`"];


(* ::Text:: *)
(*Estado interno*)


y[t_,r_,L0_]:=-(r*(Sin[\[Theta][t]]-\[Theta][t]*Cos[\[Theta][t]])+L0*Cos[\[Theta][t]]);
x[t_,r_,L0_]:= -r*(Cos[\[Theta][t]]+\[Theta][t]*Sin[\[Theta][t]])+L0*Sin[\[Theta][t]];


(* ::Text:: *)
(*Expor API*)


X[raioPolia_, comprimentoFio_, funcaoAngulo_]:= x[t,raioPolia,comprimentoFio]/.{\[Theta][t]:> funcaoAngulo};


Y[raioPolia_, comprimentoFio_, funcaoAngulo_]:= y[t,raioPolia,comprimentoFio]/.{\[Theta][t]:> funcaoAngulo};


End[];


EndPackage[];
