(* ::Package:: *)

(* ::Section:: *)
(*(*OEIS Package: Title and comments*)*)


(* :Title: Package OEIS *)

(* :Context: Utilities`OEIS`         *)

(* :Author:  Enrique P\[EAcute]rez Herrero   *)

(* :Summary: 
	This package provides various utilities for 
    searching and working with the information in 
    The On-Line Encyclopedia of Integer Sequences.
    http://oeis.org/
*)

(* :Package Version: 2.01 *)

(* :Mathematica Version: 7.0.0 *)

(* :Links:
The OEIS Foundation Inc:            http://oeisf.org/
OEIS Wiki:                          http://oeis.org/wiki/
Psychedelic Geometry Blogspot:      http://psychedelic-geometry.blogspot.com/
OEIS Page:                          http://oeis.org/wiki/User:Enrique_P\[EAcute]rez _Herrero/
*)

(* :History:
	V. 1.0 26 August 2010, by E.P\[EAcute]rez Herrero.
    V. 2.0 21 November 2010, by EPH.
*)

(* :Keywords:
	packages, sequence, OEIS
*)

(* :Licence:
Creative Commons Attribution-Noncommercial-Share Alike 3.0 License.
*)

(* :Limitations:  *)

(* :Discussion:  *)

(* :Changes:  
29/Aug/2010 - Added OEISbFile function and exporting to Html
28/Nov/2010 - Code rebuilt due to changes in OEIS, old links are no longer valid.
04/Jun/2011 - Fixed Unicode LF, instead of Window's CRLF for bFiles.
*)


(* ::Section:: *)
(*(*Begin Package and Help*)*)


BeginPackage["OEIS`"];


(*Help on Package functions *)


(*
Old links are no more valid
OEISServerURL::usage="http://oeis.org/classic/";
OEISWikiURL::usage="http://oeis.org/wiki/";
*)

OEISServerURL::usage="http://oeis.org/";


OEISTotalNumberOfSequences::usage="OEISTotalNumberOfSequences[]: Reads the actual number of Sequences that OEIS contais from http://oeis.org/
or returns a default value if it cannot connect.";

OEISValidateIDQ::usage="OEISValidateIDQ[ID]: Checks that the Sequence ID is valid.";
OEISImport::usage="OEISImport[ID,element]: Retrieves Data, Description, Image, Offset or bFile from OEIS Server"; 
OEISURL::usage="OEISURL[ID]: Gives the OEIS sequence URL when the ID is entered";
OEISFunction::usage="OEISFunction[ID]:  Creates a function named with the OEIS ID and preloads data from the sequence and its bFile, if found";

OEISExport::usage="OEISExport[ID,filename]: Export required data to file";


(*Help on other Functions *)

OEISbFile::usage="OEISGeneratebFile[ID,Vmax,filena,a]: Save the bFile up to Vmax values.";



(*Help on Options *)

bFile::usage="bFile";
URL::usage="URL";
URLType::usage="URLType";
AddHelp::usage="AddHelp";
Output::usage="Output";


(* Error Messages *)

OEIS::conopen="Cannot connect to OEIS Server: `1`"; 
OEIS::bFile ="Cannot open bFile from OEIS Server: `1`";
OEIS::ID="`1` is not a valid OEIS ID";


Unprotect["`*"];


Begin["`Private`"];


(* ::Section:: *)
(*(* URLs *)*)


(*
Old OEIS links are no longer valid
OEISServerURL="http://oeis.org/classic/";
OEISWikiURL="http://oeis.org/wiki/"
*)

OEISServerURL="http://oeis.org/";


(* ::Section:: *)
(*(* Importing Data  *) *)


(*OEISTotalNumberOfSequences[]:   *)

Options[OEISTotalNumberOfSequences]={URL->True};

OEISTotalNumberOfSequences[OptionsPattern[OEISTotalNumberOfSequences]]:=OEISTotalNumberOfSequences[OptionsPattern[OEISTotalNumberOfSequences]]=
	Module[{dataloaded,first,last, defaultvalue=178477,urlQ},
		dataloaded=Quiet[Import[OEISServerURL,"Plaintext"]];
		urlQ=OptionValue[URL];
		If[Head[Element[urlQ,Booleans]]===Symbol,
			If[!urlQ,Return[ defaultvalue]];
			If[dataloaded===$Failed,
			(*No internet connection to OEIS Server *)
				Message[OEIS::conopen,OEISServerURL];
                Return[defaultvalue];,
                (*Read OEIS Data from: http://oeis.org/*)
	             first=2+Last[Flatten[StringPosition[dataloaded,"Contains"],1]];
                 last=-2+First[Flatten[StringPosition[dataloaded,"sequences."],1]];
	             Return[ToExpression[StringTake[dataloaded,{first,last}]]];
			     ],
                 (* URL option is not True or False*)
                 Message[General::opttf,"URL",1];
		         Return[$Failed];
                 ];
        ]

(*Execute Function and connect to OEIS Server *)
OEISTotalNumberOfSequences[];


(* OEISValidateIDQ *)

OEISValidateIDQ[ID_]:=Module[{result,IDNumber},
	Which[
		(*ID is a String*)
		StringQ[ID],
		IDNumber=ToExpression[StringDrop[ID,1]];
	    result=((StringLength[ID]==7)&&
		(StringTake[ID,1]=="A")&&
		NumberQ[IDNumber](*&&
		(IDNumber<=OEISTotalNumberOfSequences[])*)),
		(*ID is a List*)
		ListQ[ID],
		result=And@@OEISValidateIDQ/@ID,
		(*Error *)
		True,
		Message[General::string,"ID",1];
		result=$Failed;
		];
		If[result===False ,Message[OEIS::ID,ID]];
	Return[result];
];


(* OEISImport *)

OEISImport[ID_?OEISValidateIDQ,element_:"Data"]:=Module[{result,myurl,dataloaded},
Which[
	(* Data  *)
	element=="Data",
	myurl=OEISServerURL<>ID<>"/list";
    dataloaded=ToExpression[Drop[Import[myurl,"Data"][[2]][[2]][[1]],1]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded];,
	
	(* Description  *)
	element=="Description",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Module[{first,last},
		first=2+Flatten[StringPosition[dataloaded,"%N"],1][[2]];
		last=-3+Select[StringPosition[dataloaded,"%"][[All,1]],#>first&][[1]];
		Return[StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""]]
			]
		];,

	(* Author *)
	element=="Author",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Module[{first,last,authorfound,resultstring,anotherentryfound},
		authorfound=StringPosition[dataloaded,"%A"];
        If[authorfound!={},
			first=2+Flatten[authorfound,1][[2]];, 
			(*No Author Found*)
            Return[{}];
		];
		last=-2+Select[StringPosition[dataloaded,"Lookup"][[All,1]],#>first&][[1]];
		anotherentryfound=Select[StringPosition[dataloaded,"%"][[All,1]],#>first&];
		If[anotherentryfound!={},last=Min[last,-2+anotherentryfound[[1]]] ];
		resultstring=StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""];
		(* Remove emails *)
		resultstring=StringReplace[resultstring,"(AT)"->""];
		resultstring=StringReplace[resultstring,"("~~Shortest[___]~~")"->""];
		(* Remove ending dot*)
		resultstring=StringReplace[resultstring," ."->""];
		(*Remove Date*)
		resultstring=StringReplace[resultstring,DatePattern[{" , ","MonthName"," ","Day"," ","Year"}]->""];
		(* Remove Spaces and newlines *)
		resultstring=StringTrim[resultstring];
		Return[{resultstring}]
			]
		];,
	
    (* Date    *)
	element=="Date",
	myurl=OEISServerURL<>ID<>"/internal";
	dataloaded=Quiet[Import[myurl,"Plaintext"]];
	If[dataloaded===$Failed, result=$Failed,
		result=Module[{first,last,authorfound,resultstring,anotherentryfound},
		authorfound=StringPosition[dataloaded,"%A"];
        If[authorfound!={},
			first=10+Flatten[authorfound,1][[2]];,
			(*No Author Entry Found*)
            Return[{""}];
		];
		last=-2+Select[StringPosition[dataloaded,"page"][[All,1]],#>first&][[1]];
		anotherentryfound=Select[StringPosition[dataloaded,"%"][[All,1]],#>first&];
		If[anotherentryfound!={},last=Min[last,-2+anotherentryfound[[1]]] ];
		resultstring=StringReplace[StringTake[dataloaded,{first,last}],"  "~~_->""];
		resultstring=StringCases[resultstring,DatePattern[{"MonthName"," ","Day"," ","Year"}]];
		If[Length[resultstring]==0,Return[{""}],resultstring=Flatten[StringSplit[resultstring],1]];
		Return[resultstring]
			]
		];,

	(* Image  *)
	element=="Image",
	myurl=OEISServerURL<>ID<>"/graph?png=1";
	dataloaded=Quiet[Import[myurl,"Image"]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded];,

	(* bFile  *)
	element=="bFile",
	myurl=OEISServerURL<>ID<>"/b"<>StringDrop[ID,1]<>".txt";
	result=Module[{dataloaded2=Quiet[Import[myurl,"Data"]]},
	If[dataloaded2===$Failed,
		(* No bFile Found *)
		Message[OEIS::bFile,myurl];
		Return[$Failed];,
		Select[dataloaded2,#!={}&]]];,

	(* Offset or MinData*)
	(element=="Offset")||(element=="MinData"),
	myurl=OEISServerURL<>ID<>"/list";
    dataloaded=ToExpression[Drop[Import[myurl,"Data"][[2]][[2]][[1]],1]];
	If[dataloaded===$Failed, result=$Failed,result=dataloaded[[1]][[1]]];,	

	(* MaxData *)
	element=="MaxData",
	Module[{mymaxdata},
		mymaxdata=Quiet[OEISImport[ID,"bFile"]];
		If[mymaxdata===$Failed,
		mymaxdata=First[Last[OEISImport[ID,"Data"]]],
		mymaxdata=First[Last[mymaxdata]];
		];
		Return[mymaxdata]
		],	

	(* Erroneous Input *)
		True,
		Message[General::optx,element,2];
		Return[$Failed];
	];
If[result===$Failed&&(element!="bFile"),Message[OEIS::conopen,myurl]];
Return[result];
]
(* Attributes *)
SetAttributes[OEISImport,{Listable}];


(* OEISURL: *)

Options[OEISURL]={URL->True,bFile->False};

OEISURL[ID_?OEISValidateIDQ,OptionsPattern[OEISURL]]:=
 Module[{urlQ,urltype,bfileQ,url="",result="",myid=""},
	urlQ=OptionValue[URL];
	bfileQ=OptionValue[bFile];
    If[!(Head[Element[bfileQ,Booleans]]===Symbol),
			(* bFile option is not True or False*)
			Message[General::opttf,"bFile",bfileQ];
		    Return[$Failed];
	];
	If[!(Head[Element[urlQ,Booleans]]===Symbol),
			(* URL option is not True or False*)
			Message[General::opttf,"URL",urlQ];
		    Return[$Failed];
	];
	If[bfileQ,myid=("b"<>StringDrop[ID,1]<>".txt"),myid=ID];
	If[urlQ,url=OEISServerURL];
result=url<>myid;
Return[result];
]


(* OEISFunction *)

Options[OEISFunction]={AddHelp->True,bFile->True,Output->True};

OEISFunction[ID_?OEISValidateIDQ,OptionsPattern[OEISFunction]]:=Module[{descriptionloaded,dataloaded,bfileloaded,bfileQ,addhelpQ,outputQ},
	addhelpQ=OptionValue[AddHelp];
	bfileQ=OptionValue[bFile];
	outputQ=OptionValue[Output];
	(* Invalid Values for Valid Options *)
	If[!(Head[Element[addhelpQ,Booleans]]===Symbol),
			(* bFile option is not True or False*)
			Message[General::opttf,"AddHelp",addhelpQ];
		    Return[$Failed];
	];
	If[!(Head[Element[bfileQ,Booleans]]===Symbol),
			(* bFile option is not True or False*)
			Message[General::opttf,"bFile",bfileQ];
		    Return[$Failed];
	];
	If[!(Head[Element[outputQ,Booleans]]===Symbol),
			(* bFile option is not True or False*)
			Message[General::opttf,"Output",outputQ];
		    Return[$Failed];
	];
	descriptionloaded=Quiet[OEISImport[ID,"Description"]];
	If[descriptionloaded===$Failed,
		Return[$Failed];
		Message[OEIS::conopen,OEISURL[ID]],
		(*Adding Help *)
		If[addhelpQ,
		ToExpression[StringJoin[ID,"::usage=\"",ID,"[n]: ",descriptionloaded,"\""]]];
		(*Loading data from bFile if exists*)
		If[bfileQ,bfileloaded=Quiet[OEISImport[ID,"bFile"]],bfileloaded={}];
		If[bfileloaded===$Failed||!bfileQ,dataloaded=OEISImport[ID,"Data"],dataloaded=bfileloaded];
		(ToExpression[StringJoin["("<>ID<>"[#[[1]]]=#[[2]])&"]]/@dataloaded);
		If[outputQ,
			(* Prints information about function created *)
			Print[ID,"(n): ",descriptionloaded];
			Print["Link-1: ",Hyperlink[OEISURL[ID]]];
			(* Old code, wiki link removed
			Print["Link-2: ",Hyperlink[OEISURL[ID,URLType->"Wiki"]]];*)
			If[bfileQ&&!bfileloaded===$Failed,
				Print["Link-2: ",Hyperlink[OEISURL[ID,bFile->True]]]];
			Print["Help Added: ",addhelpQ||descriptionloaded===$Failed];
			Print["Loaded data from bFile: ",bfileQ&&!bfileloaded===$Failed];
			Print["Loaded data from Sequence: ",!bfileQ||bfileloaded===$Failed];
			Print["Data available: Table of n, ",ID,"(n) for n=",First[First[dataloaded]],"..",First[Last[dataloaded]]];
		]
	]
]



(* ::Section:: *)
(*(* Exporting Data *)*)


(*  OEISExport  *)

OEISExport[ID_?OEISValidateIDQ,filename_]:=Module[{myfileextension=ToLowerCase[FileExtension[filename]]},
Which[
(*AddHelp *)
MemberQ[{"gp","m"},myfileextension],
Module[{myhelptype,myhelpstring,myID},
If[ListQ[ID],myID=ID,myID={ID}];
Which[
	(*Help for PARI/GP *)
	myfileextension==="gp",myhelpstring=StringJoin["addhelp(",#,",\"",#,": ",OEISImport[#,"Description"],"\");"]&/@myID,
          (*Help for Mathematica *)
	myfileextension==="m",myhelpstring=StringJoin[#,"::usage=\"",#,"[n]: ",OEISImport[#,"Description"],"\";"]&/@myID,
          (*Wrong HelpType option value *)
         True,
	 Message[General::optx,myhelptype,2];
         Return[$Failed];
   ];
Return[Export[filename,myhelpstring,"Text"]];
],

(*Data *)
MemberQ[{"txt","xls","csv","tsv","dat"},myfileextension],
Module[{myfilename,myID,myresult},
If[ListQ[ID],myID=ID,myID={ID}];
If[Length[myID]>1,myfilename=Function[#<>"_"<>filename],myfilename=Function[filename]];
Module[{dataloaded},
dataloaded=Quiet[OEISImport[#,"bFile"]];
	If[dataloaded===$Failed,dataloaded=OEISImport[#,"Data"]];
Export[myfilename[#],dataloaded]]&/@myID
],

(*Image *)
(MemberQ[{"jpg","jpeg","jp2", "j2k","bmp","pgn","gif","tiff", "tif"},myfileextension]),
If[ListQ[ID],
Return[Export[#<>"_"<>filename,OEISImport[#,"Image"],"Image"]&/@ID];,
Export[filename,OEISImport[ID,"Image"],"Image"];
],

(*BibTeX *)
(MemberQ[{"bib"},myfileextension]),
	Module[{mybibtex,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		mybibtex=(Module[{mydate,mymonth={},myyear={},myauthor,mydescription,mycitation={}},
	(*Reading Online Data*)
	mydate=OEISImport[#,"Date"];
	If[Length[mydate]!=1,
	mymonth=mydate[[1]];
	myyear=mydate[[3]];
	];
	myauthor=Flatten[OEISImport[#,"Author"]];
	mydescription=OEISImport[#,"Description"];
	(*Function Output*)
	mycitation="@MISC{oeis"<>#<>","<>"\n"<>
		"AUTHOR={"<>myauthor<>"},"<>"\n"<>
		"TITLE={The {O}n-{L}ine {E}ncyclopedia of {I}nteger {S}equences},"<>"\n"<>
		"HOWPUBLISHED={\\href{"<>OEISServerURL<>#<>"}{"<>#<>"}},"<>"\n"<>
		"MONTH={"<>mymonth<>"},"<>"\n"<>
		"YEAR={"<>myyear<>"},"<>"\n"<>
		"NOTE={"<>mydescription<>"}"<>"\n"<>
		"}"<>"\n";mycitation]&/@myID);
Return[Export[filename,mybibtex,"Text"]];
],

(*HTML *)
(MemberQ[{"htm","html"},myfileextension]),
	Module[{myhtmlcode,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		myID=({#,Part[myID,#]}&/@Range[Length[myID]]);
		myhtmlcode=(Module[{separator,myauthor,mydescription,mycitation={}},
	(*Reading Online Data*)
	
	myauthor=Flatten[OEISImport[#[[2]],"Author"]];
    If[Length[myauthor]==1,separator=", ",separator=""];
	mydescription=OEISImport[#[[2]],"Description"];
	(*Function Output*)
	mycitation="<a name=\"oeis"<>ToString[#[[2]]]<>"\">["<>ToString[#[[1]]]<>"]-</a> "<>
		myauthor<>separator<>
		"The On-Line Encyclopedia of Integer Sequences. "<>
		"<a href=\""<>OEISServerURL<>ToString[#[[2]]]<>"\">"<>ToString[#[[2]]]<>"</a>: "<>
		mydescription<>"<br/>";
		mycitation])&/@myID;
Return[Export[filename,myhtmlcode,"Text"]];
],

(*Wiki *)
(MemberQ[{"wiki"},myfileextension]),
	Module[{mywikicode,myID},
		If[ListQ[ID],myID=ID,myID={ID}];
		myID=({#,Part[myID,#]}&/@Range[Length[myID]]);
		mywikicode=(Module[{mydescription,mycitation={}},
	(*Reading Online Data*)
	mydescription=OEISImport[#[[2]],"Description"];
	(*Function Output*)
	mycitation="* {{oeis|"<>ToString[#[[2]]]<>"}}: "<>mydescription;
		mycitation])&/@myID;
Return[Export[filename,mywikicode,"Text"]];
],

(*Wrong element*)
True,
 Message[General::optx,filename,2];
	   Return[$Failed];
	]
]


(* ::Section:: *)
(*(* Exporting b-Files *)*)


OEISbFile[ID_?OEISValidateIDQ,VMax_Integer,filename___]:=Module[{mybfiledata,mymindata,mymaxdata,mybfilename,mybfile},
mymindata=OEISImport[ID,"MinData"];
(*mymaxdata=OEISImport[ID,"MaxData"];*)
(*If no input filename use a default filename*) 
If[filename=="Null",mybfilename=OEISURL[ID,URL->False,bFile->True],mybfilename=filename];
(* If the function is not defined load all available data from OEIS*)
If[NameQ[ID]==False,OEISFunction[ID,Output->False]];
mybfiledata=({#,(ToExpression[ID<>"["<>ToString[#]<>"]"])}&/@Range[mymindata,VMax]);
(*Delete non numerical data*)
mybfiledata=Select[mybfiledata,NumberQ[#[[2]]]&];
(* Unicode LF for newlines *)
mybfile=OpenWrite[mybfilename, BinaryFormat->True, CharacterEncoding->"Unicode"]; 
WriteString[mybfile, ToString[#[[1]]]<>" "<>ToString[#[[2]]]<>"\n"]&/@mybfiledata;
Return[Close[mybfile]]
]


End[];


Protect["`*"];


EndPackage[];
