(* ::Package:: *)

BeginPackage["Importador`"];


(* ::Chapter:: *)
(*Interface*)


ChavesExistentes::usage = "ChavesExistentes[] retorna lista de strings. Cada uma \[EAcute] tipo de dado existente";


Listar::usage = "Listar[tomada, 'r', '\[Theta]'] (exemplo) pega lista de pares ordenados (r,\[Theta]). A lista pode ser 
de trincas, quartetos etc.";


Pegar::usage = "Pegar[tomada, algo]. Argumentos poss\[IAcute]veis s\[ATilde]o 'comprimento do fio', 'raio da polia' e 
'massa oscilante'";


QuantasTomadas::usage = "QuantasTomadas[] Retorna quantas tomadas foram feitas";


(* ::Chapter:: *)
(*Interno*)


Begin["`Private`"];


<< PenduloComPolia`Importador`Parte`;


<< PenduloComPolia`Importador`Leitura`;


<< TrackerImport`;


<< PenduloComPolia`ModeloGeometrico`;


dir = If[
	$InputFileName=="", 
	FileNameJoin@{NotebookDirectory[],"..","tabelas"},
	FileNameJoin@{DirectoryName[$InputFileName],"..","tabelas"}
];
FixarDiretorio[dir];


(* Verificar tipos de arquivo existentes *)
{raioUnico,massaUnica,comprimentoFioUnico} = {
	FileExistsQ@FileNameJoin@{dir,"raio-polia"},
	FileExistsQ@FileNameJoin@{dir,"massa"},
	FileExistsQ@FileNameJoin@{dir,"comprimento-fio"}
};

(* 
Receber os dados de raio, massa, comprimento, cinem\[AAcute]tica. 
Inferir quantidade de tomadas 
*)
For[i=1, FileExistsQ@FileNameJoin@{dir,"tabela-cinematica-"<>ToString@i}, i=i+1,
	r[i] = LerEscalar[
		If[raioUnico, "raio-polia", "raio-polia-"<>ToString@i]
	];
	massa[i] = LerEscalar[
		If[massaUnica, "massa", "massa-"<>ToString@i]
	];
	L0[i] = LerEscalar[
		If[comprimentoFioUnico, "comprimento-fio", "comprimento-fio-"<>ToString@i]
	];
	dados[i] = LerLista[
		"tabela-cinematica-"<>ToString@i
	];
	(* Selecionar somente tempos entre os limites impostos *)
	limites = LerLista["limites-tabela-cinematica-"<>ToString@i];
	dados[i] = Select[dados[i], #[[1]] <= If[Length@limites == 2, Last@limites, \[Infinity]]&];
	dados[i] = Select[dados[i], #[[1]] >= First@limites&];
	dados[i] = Map[Join[{First@# - First@limites}, Rest@#]&, dados[i]];
];
quantTomadas = i-1;


(* Parametriza x[t] e y[t] usando \[AHat]ngulo-s\[IAcute]mbolo \[Theta][t] *)
x[t_, tomada_]:=X[r[tomada], L0[tomada], \[Theta][t]]
y[t_, tomada_]:=Y[r[tomada], L0[tomada], \[Theta][t]]


(* Construir interpola\[CCedilla]\[ATilde]o linear de \[Phi] para \[Theta] *)
For[i=1,i<=quantTomadas,i=i+1,
	phiParaTheta[phi_, i] = Module[
		{x1,y1,phiComTheta,modelo},
		
		(* Converter em express\[ATilde]o, retirando s\[IAcute]mbolo 't' *)
		x1= ReplaceAll[x[t, i], \[Theta][t]->\[Theta]];
		y1 = ReplaceAll[y[t, i], \[Theta][t]->\[Theta]];
		
		(* Lista {...,{phi,theta},...} *)
		phiComTheta = Table[
			{ReplaceAll[ArcTan[x1,y1],\[Theta]->theta],theta},
			{theta,-45\[Degree],+45\[Degree],0.01\[Degree]}
		];
		
		(* Retornar interpola\[CCedilla]\[ATilde]o *)
		LinearModelFit[phiComTheta, \[Phi], \[Phi]]
	][phi]
]

(* Usa interpola\[CCedilla]\[ATilde]o linear constru\[IAcute]da para acoplar medidas de \[Theta] \[AGrave] lista cinem\[AAcute]tica *)
concatenarTheta[lista_, tomada_]:=Map[Join[#, {phiParaTheta[Parte[#, "\[Phi]"]*\[Pi]/180, tomada]}]&,lista];


For[i = 1, i <= quantTomadas, i = i+1,
	dados[i] = concatenarTheta[dados[i],i]
]


(* ::Chapter:: *)
(*Publica\[CCedilla]\[ATilde]o*)


ChavesExistentes[] = Keys@Formato[];


QuantasTomadas[] = quantTomadas;


Listar[tomada_, x__] := Module[
	{listaFiltrada, pedidos, interpolador},
	
	pedidos = {x};
	interpolador[enupla_] := Map[Parte[enupla,#]&,pedidos];
	listaFiltrada = Map[interpolador,dados[tomada]];
	If[
		Length@Part[listaFiltrada,1] == 1,
		Flatten@listaFiltrada,
		listaFiltrada
	]
	
];


Pegar[tomada_ ,parametro_]:=
	If[parametro=="comprimento do fio", L0[tomada],
		If[parametro=="raio da polia", r[tomada],
			If[parametro=="massa oscilante", massa[tomada],
				"desconhecido"
			]
		]
	];


End[];


EndPackage[];
