(* ::Package:: *)

BeginPackage["Importador`"];


<< VarCleanup`;


<< TrackerImport`;


<< PenduloComPolia`ModeloGeometrico`;


(* ::Section:: *)
(*Interface*)


ChavesExistentes::usage = "Retorna lista de strings. Cada uma \[EAcute] tipo de dado existente";


Listar::usage = "Listar['r', '\[Theta]'] (exemplo) pega lista de pares ordenados (r,\[Theta]). A lista pode ser de trincas, quartetos etc.";


Pegar::usage = "Argumentos poss\[IAcute]veis s\[ATilde]o 'comprimento do fio', 'raio da polia' e 'massa oscilante'";


QuantasTomadas::usage = "Retorna quantas tomadas foram feitas";


(* ::Section:: *)
(*Processamento interno*)


Begin["Private`"];


dir = If[$InputFileName=="", NotebookDirectory[]<>"..\\tabelas\\", DirectoryName[$InputFileName]<>"..\\tabelas\\"];


formato = {"t"->1, "x"->2,"y"->3,"r"->4,"\[Phi]"->5,"\!\(\*SubscriptBox[\(L\), \(fio\)]\)"->6, "\[Theta]"->7};


Parte[lista_, parte_]:=lista[[parte/.formato]];


r = (Import[dir<>"raio-polia", "Table"]//Flatten)[[1]];
massa = (Import[dir<>"massa", "Table"]//Flatten)[[1]];
L0[1]=(Import[dir<>"comprimento-fio-1", "Table"]//Flatten)[[1]];
L0[2]=(Import[dir<>"comprimento-fio-2", "Table"]//Flatten)[[1]];
L0[3]=(Import[dir<>"comprimento-fio-3", "Table"]//Flatten)[[1]];


tamanhoFio[posicao_]:=Sqrt[posicao^2-r^2];
concatenarFio[lista_]:=Join[#, {tamanhoFio[ Parte[#,"r"] ]}]&/@lista;


x[t_, tomada_]:=X[r, L0[tomada], \[Theta][t]]
y[t_, tomada_]:=Y[r, L0[tomada], \[Theta][t]]


For[i=1, i<=3, i=i+1, phiParaTheta[phi_, i]=Module[{x1,y1,phiComTheta,modelo},
	x1= Replace[x[t, i],\[Theta][t_]:> \[Theta], All];
	y1 = Replace[y[t, i],\[Theta][t_]:> \[Theta], All];
	phiComTheta = Table[{ArcTan[x1,y1]/.\[Theta]->theta,theta},{theta,-45\[Degree],+45\[Degree],0.01\[Degree]}];
	modelo = LinearModelFit[phiComTheta, \[Phi],\[Phi] ];
	modelo
][phi]]
concatenarTheta[lista_, tomada_]:=Join[#, {phiParaTheta[Parte[#, "\[Phi]"]*\[Pi]/180, tomada]}]&/@lista;


dados[1] = TrackerImport[dir<>"tabela-cinematica-1"]//concatenarFio//(concatenarTheta[#,1]&);
dados[2] = TrackerImport[dir<>"tabela-cinematica-2"]//concatenarFio//(concatenarTheta[#,2]&);
dados[3] = TrackerImport[dir<>"tabela-cinematica-3"]//concatenarFio//(concatenarTheta[#,3]&);


(* ::Section:: *)
(*Publica\[CCedilla]\[ATilde]o*)


ChavesExistentes[]={"t", "x", "y", "r", "\[Theta]", "\!\(\*SubscriptBox[\(L\), \(fio\)]\)", "\[Phi]"};


QuantasTomadas[] = 3;


Listar[tomada_, x__]:=Module[{listaFiltrada, pedidos, interpolador},
	pedidos = {x};
	interpolador[enupla_] := Parte[enupla,#]&/@pedidos;
	listaFiltrada = interpolador/@(dados[tomada]);
	If[Length[Part[listaFiltrada,1]]>1, listaFiltrada, Flatten[listaFiltrada]]
];


Pegar[tomada_ ,parametro_]:=
	If[parametro=="comprimento do fio", L0[tomada],
	If[parametro=="raio da polia", r,
	If[parametro=="massa oscilante", massa,
	"desconhecido"
	]]];


End[];


EndPackage[];
