(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["CheckProperties`"];

CheckProperties::usage="CheckProperties[symbol] returns the {available, unavailable} properties for symbol.";
PropertyAudit::usage="PropertyAudit[symbol] returns a TableForm of the available properties for symbol.";

Begin["`Private`"];


(* ::Input::Initialization:: *)
CheckProperties[thing_]:=Module[{not},
not=Pick[thing["Properties"],Quiet@Check[thing[#],Missing[#]]&/@thing["Properties"],_Missing|With[{thang=thing},HoldPattern[thang[_]]]];

{Complement[thing["Properties"],not],not}]


(* ::Input::Initialization:: *)
PropertyAudit[thing_]:=Module[{t,u},
TableForm[
{#->If[AtomQ[t=thing[#]],t,
If[Length[u=Union[t]]==1,{Head[t],Length[t],u},{Head[t],Dimensions[t],Short[t]}]]}&/@First[CheckProperties[thing]]]]


(* ::Input::Initialization:: *)
SetAttributes[{PropertyAudit,CheckProperties}, HoldFirst] 

End[];

EndPackage[];
