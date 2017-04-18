(* ::Package:: *)

BeginPackage["CalcularPeriodo`"];


<< PenduloComPolia`Importador`;


CalcularPeriodoExp::usage="CalcularPeriodoExp[tomada] retorna o per\[IAcute]odo m\[EAcute]dio da tomada pedida (1, 2 ou 3)";
CalcularPeriodoTeo::usage="CalcularPeriodoTeo[\[Theta][t]] retorna o per\[IAcute]odo m\[EAcute]dio da fun\[CCedilla]\[ATilde]o fornecida";


Begin["Private`"];


CalcularPeriodoExp[tomada_]:=
	2*Mean[Select[Differences[Map[#[[1]]&,Select[Listar[tomada,"t","\[Theta]"],Abs[#[[2]]]<0.02&]]],#>0.05& ]];

CalcularPeriodoTeo[theta_[t_]]:=
	2*Mean[Select[Differences[Map[#[[1]]&,Select[Table[{t,theta[t]},{t,0,8,0.0001}],Abs[#[[2]]]<0.001&]]],#>0.05& ]];


End[];
EndPackage[];
