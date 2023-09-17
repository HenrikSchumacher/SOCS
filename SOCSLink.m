(* ::Package:: *)

(* Mathematica Package *)

(* :Title:   SOCS - Simples, Old-school Cholesky Solver    *)
(* :Context: SOCS`   *)
(* :Author:  Henrik Schumacher     *)
(* :Date:    2023-09-9   *)

(* :Mathematica Version: 13.2 *)
(* :Copyright: (c) 2023 Henrik Schumacher *)
(* :License: MIT license, see LICENSE.txt *)
(* :Keywords: Cholesky Decomposition *)



BeginPackage["SOCSLink`", {"CCompilerDriver`"}];


SOCS::usage="";


(* Privately load LTemplate. Note the leading ` character!! *)
Get["`LTemplate`LTemplatePrivate`"]

ConfigureLTemplate[
	"MessageSymbol" -> SOCSLink, 
	(* If lazy loading is enabled, functions are loaded only on first use.
	   This improves package loading performance, but it is not convenient
	   during development and debugging. *)
	"LazyLoading" -> True
];

LTemplate`Private`symName["SOCS"]="SOCSLink`SOCS";

LClassContext[] ="SOCSLink`";


Begin["`Private`"];

$packageDirectory  = DirectoryName[$InputFileName];
$libraryDirectory  = FileNameJoin[{$packageDirectory, "LibraryResources", $SystemID}];
$sourceDirectory   = FileNameJoin[{$packageDirectory, "LibraryResources", "Source"}];

Print["$packageDirectory"->$packageDirectory];
Print["$libraryDirectory"->$libraryDirectory];
Print["$sourceDirectory"->$sourceDirectory];

Print["This is the package SOCSLink` installed at:"];
Print["\t",$packageDirectory];

$buildSettings     = Get[FileNameJoin[{$packageDirectory, "BuildSettings.m"}]];

Print["$buildSettings"->$buildSettings];

(* Add $libraryDirectory to $LibraryPath in case the package 
   is not installed in $UserBaseDirectory/Applications. 
 *)
If[Not@MemberQ[$LibraryPath, $libraryDirectory],
  AppendTo[$LibraryPath, $libraryDirectory]
];

template = LTemplate["SOCSLib",
	{
		LClass["SOCS",
		{
			LFun["Init",{{Integer,1,"Constant"},{Integer,2,"Constant"},{Real,1,"Constant"},{Integer,1,"Constant"},Integer},"Void"]
			,LFun["Refactorize",{{Real,1,"Constant"}},"Void"]
			,LFun["SolveVector",{{Real,1,"Constant"}},{Real,1}]
			,LFun["SolveMatrix",{{Real,2,"Constant"}},{Real,2}]
			,LFun["Dimensions",{},{Integer,1}]
			(*,LFun["NonzeroValues",{},{Real,1}]*)
		}]
	}
];

loadMyApp[] := (
	compileMyApp[];
	LoadTemplate[template];
	);
	
compileMyApp[] := 
	Module[{res,linkeroptions,lib,src},
		lib = FileNameJoin[{$libraryDirectory,"SOCSLib"<>CCompilerDriver`CCompilerDriverBase`$PlatformDLLExtension}];
		src = FileNameJoin[{$sourceDirectory,"SOCS.h"}];
		If[!FileExistsQ[lib]||(FileDate[src]>FileDate[lib]),
			Internal`WithLocalSettings[
				SetDirectory[$sourceDirectory];
				,
				res = CompileTemplate[template, 
					Sequence@@$buildSettings,
					"TargetDirectory" -> $libraryDirectory
				];
				,
				ResetDirectory[];
			];
			Print["Compilation done."];
		];
		res
	];
	 

loadMyApp[] (* load template *)
Sparsify[A_SparseArray]:=A;

Sparsify[A_?(MatrixQ[#]&&Head[#]=!=SparseArray&)]:=
Module[{m,n},
	{m,n}=Dimensions[A];
	SparseArray@@{Automatic,{m,n},0.,{1,{
		Range[0,n m,n],
		Partition[Flatten[ConstantArray[Range[n],m]],1]
	},
	Flatten[A]}}
];

(*PardisoUpdate::nnz = "Second argument of PardisoUpdate has `1` nonzero values. `2` nonzero values required.";

PardisoUpdate::n = "Second argument of PardisoUpdate has dimension `1`. Dimension `2` required.";

Pardiso::vecdim = "Second Argument of Pardiso is vector with `1` entries; `2` entries expected.";

Pardiso::matdim = "Second Argument of Pardiso is matrix with `1` rows; `2` rows expected.";

Pardiso::mtype = "`1` is not a valid matrix type. The only accepted types are 1, 2, -2, 3, 4, -4, 6, 11, and 13.";

Pardiso::unreal = "Error: Currently, only real matrices are support.";

Pardiso::unsorted = "Warning: Input matrix is not ordered according to the CRS standard. Sorting now. For maximal performance, please consider to assemble the matrix conforming to the standard.";
Pardiso[A0_SparseArray, OptionsPattern[{
	"MatrixType" -> 11, 
	"IntegerParameters" -> Automatic,
	"Permutation" -> Automatic,
	"CheckMatrix" -> True
}]] := Module[{n, P, iparm, mtype, A, perm, a},
	mtype = OptionValue["MatrixType"]; 
	n = Length[A0]; 
	P = Make["Pardiso"]; 
	If[!MemberQ[{11,1,2,-2}, mtype],mtype=11;];
	A=Switch[mtype,
		(*Generic unsymmetric matrix.*)
		11, A0,
		(*structurally symmetric matrix. Diagonal elements must be nonzero for pivoting reasons.*)
		1, A0,
		(*symmetric, positive-definite matrix. Diagonal elements must be nonzero for pivoting reasons.*)
		2, UpperTriangularize[A0 + $MachineEpsilon IdentityMatrix[n,SparseArray,WorkingPrecision->MachinePrecision]],
		(*symmetric, indefinite matrix. Diagonal elements must be nonzero for pivoting reasons.*)
		-2, UpperTriangularize[A0 + $MachineEpsilon IdentityMatrix[n,SparseArray,WorkingPrecision->MachinePrecision]]
	];
	P["SetCheckMatrixQ"[Boole[!TrueQ[!OptionValue["CheckMatrix"]]]]];
	If[(P["CheckMatrixQ"[]]=!=0)&&(!SparseArray`SparseArraySortedQ[A]),
		Message[Pardiso::unsorted];
		A= SparseArray`SparseArraySort[A];
	];
	a = Developer`ToPackedArray[N[A["NonzeroValues"]]];
	If[ Developer`PackedArrayQ[a,Real],
		P["Init"[A["RowPointers"] + 1, Flatten[A["ColumnIndices"]],a,mtype]];
		,
		Message[Pardiso::unreal];
	]; 
	iparm = OptionValue["IntegerParameters"]; 
	If[VectorQ[iparm], P["SetIntegerParameters"[iparm]]; ];
	perm = OptionValue["Permutation"]; 
	If[(VectorQ[iparm])&&(Length[perm]==n)&&1<=Min[perm]&&Max[perm]<=1,
		iparm = P["IntegerParameters"[]];
		iparm[[5]]=1;
		P["SetPermutation"[perm]]; 
	];
	P["FactorizeSymbolically"[]]; 
	If[P["Error"[]] =!= 0, Print["Pardiso error in FactorizeSymbolically. Error code = ", P["Error"[]]]]; 
	P["FactorizeNumerically"[]]; 
	If[P["Error"[]] =!= 0, Print["Pardiso error in FactorizeNumerically. Error code = ", P["Error"[]]]]; 
	P["SetTimeStamp"[AbsoluteTime[]]]; 
	P
];

Pardiso[(A0_)?MatrixQ] := Pardiso[Sparsify[A0]];

Pardiso /: (P_Pardiso)[b0_?VectorQ, modestr_String:"N"] := Module[{mode,postproc,b,x},
	b=Developer`ToPackedArray[N[b0]];
	If[!Developer`PackedArrayQ[b,Real],
		Message[Pardiso::unreal];
		x = $Failed;
	,
		If[Length[b] != P["Length"[]],
			Message[Pardiso::vecdim, Length[b], P["Length"[]]]; 
			x = $Failed;
		, 
			mode=modestr/.{"N"->0,"T"->2,"C"->1,"J"->0};
			postproc=If[modestr==="J",Conjugate,Identity];
			x = postproc[P["LinearSolve"[b, mode]]];
			If[P["Error"[]] =!= 0, 
				Print["Pardiso error in P_Pardiso[b_?VectorQ,",modestr,"]. Error code = ", P["Error"[]]]
			];
		];
	];
	x
];

Pardiso /: (P_Pardiso)[B0_?MatrixQ, modestr_String:"N",side:(Left|Right):Left] :=Module[{mode,postproc,B,X,nrhs,n},
	B=Developer`ToPackedArray[Normal[B0]];
	If[!Developer`PackedArrayQ[B,Real],
		Message[Pardiso::unreal];
		X = $Failed
	,
		X = Switch[side
		, Right
			,
			{nrhs,n}=Dimensions[B];
			If[n != P["Length"[]], 
				Message[Pardiso::matdim, n, P["Length"[]]]; 
				$Failed
			, 
				mode=modestr/.{"N"->2,"T"->0,"C"->0,"J"->1};
				postproc=If[modestr==="C",Conjugate,Identity];
				postproc[P["LinearSolveMatrix"[B, mode]]]
			]
		, Left
			,
			{n,nrhs}=Dimensions[B];
			If[n != P["Length"[]], 
				Message[Pardiso::matdim, n, P["Length"[]]]; 
				$Failed
			, 
				mode=modestr/.{"N"->0,"T"->2,"C"->1,"J"->0};
				postproc=If[modestr==="J",ConjugateTranspose,Transpose];
				postproc[P["LinearSolveMatrix"[Transpose[B], mode]]]
			]
		];
		If[P["Error"[]] =!= 0,
			Print["Pardiso error in P_Pardiso[B_?MatrixQ,",modestr,",",side,"]. Error code = ", P["Error"[]]];
		];
	];
	X
];

Pardiso /: P_Pardiso["Update"[A0_SparseArray]] := Module[{n, A}, 
	n = P["Length"[]];
	If[Length[A0] =!= n, 
		Message[PardisoUpdate::n, Length[A0], n]; 
	, 
		A=Switch[P["MatrixType"[]],
			(*Generic unsymmetric matrix.*)
			11, A0,
			(*structurally symmetric matrix. Diagonal elements must be nonzero for pivoting reasons.*)
			1, A0,
			(*symmetric, positive-definite matrix. Diagonal elements must be nonzero for pivoting reasons.*)
			2, UpperTriangularize[A0 + $MachineEpsilon IdentityMatrix[n,SparseArray,WorkingPrecision->MachinePrecision]],
			(*symmetric, indefinite matrix. Diagonal elements must be nonzero for pivoting reasons.*)
			-2, UpperTriangularize[A0 + $MachineEpsilon IdentityMatrix[n,SparseArray,WorkingPrecision->MachinePrecision]]
		];
		If[P["CheckMatrixQ"[]]=!=0&&(!SparseArray`SparseArraySortedQ[A]),
			Message[Pardiso::unsorted];
			A= SparseArray`SparseArraySort[A];
		];
		P["Update"[A["NonzeroValues"]]];
	]; 
];

Pardiso /: P_Pardiso["Update"[a0_?VectorQ]] := Module[{nnz,a}, 
	nnz = P["NumberOfNonzeros"[]]; 
	If[Length[a0] =!= nnz, 
		Message[PardisoUpdate::nnz, Length[a], nnz];
	, 
		a= Developer`ToPackedArray[N[a0]];
		If[!Developer`PackedArrayQ[a,Real],
			Message[Pardiso::unreal];
		,
			P["SetNonzeroValues"[a]]; 
			P["FactorizeNumerically"[]]; 
			If[P["Error"[]] =!= 0, Print["Pardiso error in FactorizeSymbolically. Error code =", P["Error"[]]]];
			P["SetTimeStamp"[AbsoluteTime[]]]
		]; 
	]; 
];

Pardiso /: P_Pardiso["Update"[(A0_)?MatrixQ]] := P["Update"[Sparsify[A0]]];

Pardiso /: P_Pardiso["Dimensions"[]] := {1,1} P["Length"[]];

Pardiso /: Dimensions[P_Pardiso] := {1,1} P["Length"[]];

Pardiso /: P_Pardiso["Properties"[]]= template[[2,1,2,All,1]];

Pardiso /: P_Pardiso["Properties"]= template[[2,1,2,All,1]];

(*To make it conforming with the LinearSolve syntax.*)
Pardiso /: Pardiso[L_?MatrixQ,x_,opts___]:=Pardiso[L][x];

Pardiso /: MakeBoxes[P_Pardiso, StandardForm] := BoxForm`ArrangeSummaryBox[
	Pardiso, "", StringJoin["ID:  ", ToString[P[[1]]]], 
	{
		{
			BoxForm`MakeSummaryItem[{"Specified Elements: ", P["NumberOfNonzeros"[]]}, StandardForm], 
			BoxForm`MakeSummaryItem[{"Matrix Type: ", $PardisoMatrixTypes[P["MatrixType"[]]][[2;;]]}, StandardForm]
		}, 
		{
			BoxForm`MakeSummaryItem[{"Matrix Dimensions: ", {1,1} P["Length"[]]}, StandardForm], 
			BoxForm`MakeSummaryItem[{"Type: ", $PardisoMatrixTypes[P["MatrixType"[]]][[1]]}, StandardForm]
		}
	}, 
	{
	}, 
	StandardForm, "Interpretable" -> False
] /; Function[Q, Head[Q[[1]]] === Integer][P]*)


(*SetAttributes[AssemblyFunction,HoldAll];

Assembly::expected="Values list has `2` elements. Expected are `1` elements. Returning prototype.";

Assembler[pat_?MatrixQ,{m_Integer,n_Integer},background_: 0.]:=Module[{pa,c,ci,rp,pos},
	pa=SparseArray`SparseArraySort@SparseArray[pat->_,{m,n}];
	rp=pa["RowPointers"];
	ci=pa["ColumnIndices"];
	c=Length[ci];
	pos=cLookupAssemblyPositions[Range[c],rp,Flatten[ci],pat];
	Module[{a},
		a=<|"Dimensions"->{m,n},"Positions"->pos,"RowPointers"->rp,"ColumnIndices"->ci,"Background"->background,"Length"->c|>;
		AssemblyFunction@@{a}
	]
];

AssemblyFunction/:a_AssemblyFunction[vals0_]:=Module[{len,expected,dims,u,vals},
	If[VectorQ[vals0],vals=vals0,vals=Flatten[vals0]];
	len=Length[vals];
	expected=Length[a[[1]][["Positions"]]];
	dims=a[[1]][["Dimensions"]];
	If[
		len===expected
	,
		If[
			Length[dims]==1
		,
			u=ConstantArray[0.,dims[[1]]];
			u[[a[[1]][["ColumnIndices"]]]]=cAssembleDenseVector[a[[1]][["Positions"]],vals,{a[[1]][["Length"]]}];
			u
		,
			SparseArray@@{Automatic,dims,a[[1]][["Background"]],
				{
					1,
					{a[[1]][["RowPointers"]],a[[1]][["ColumnIndices"]]},
					cAssembleDenseVector[a[[1]][["Positions"]], vals,{a[[1]][["Length"]]}]
				}
			}
		]
	,
		Message[Assembly::expected,expected,len];
		Abort[]
	]
];

ClearAll[cLookupAssemblyPositions];
cLookupAssemblyPositions:=cLookupAssemblyPositions=(
Print["Compiling cLookupAssemblyPositions."];
Compile[{{vals,_Integer,1},{rp,_Integer,1},{ci,_Integer,1},{pat,_Integer,1}},
	Block[{k,c,i,j},i=Compile`GetElement[pat,1];
		j=Compile`GetElement[pat,2];
		k=Compile`GetElement[rp,i]+1;
		c=Compile`GetElement[rp,i+1];
		While[k<c+1&&Compile`GetElement[ci,k]!=j,++k];
		Compile`GetElement[vals,k]
	],
	RuntimeAttributes->{Listable},
	Parallelization->True,
	CompilationTarget->"C",
	RuntimeOptions->"Speed"
]
);

cAssembleDenseVector:=cAssembleDenseVector=(
Print["Compiling cAssembleDenseVector."];
Compile[{{ilist,_Integer,1},{values,_Real,1},{dims,_Integer,1}},
	Block[{v},
		v=Table[0.,{Compile`GetElement[dims,1]}];
		Do[
			v[[Compile`GetElement[ilist,i]]]+=Compile`GetElement[values,i]
		,{i,1,Length[values]}];
		v
	],
	CompilationTarget->"C",
	RuntimeOptions->"Speed"
]
);*)


(*Quiet[
Pardiso[id$_Integer][f$_String[___]]/;(Message[LTemplate::nofun,StringTemplate["``::``"][Pardiso,f$]];False)=.;
Pardiso[id$_Integer][f$_String[___]]/;(Message[LTemplate::nofun,StringTemplate["``::``"][Pardiso,f$]];False):=$Failed;
];*)


End[];

EndPackage[];
