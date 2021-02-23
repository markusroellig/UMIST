BeginPackage["UMIST`"]
countElements::usage = 
  "countElements[mol_String] counts the atoms in a molecule and \
prints out the inventory list, i.e. the number of particular atoms, \
in the form: {H,He,C,N,O,F,Na,Mg,Si,P,S,Cl,Fe}.";

countHCNOS::usage = "countHCNOS[mol_String] counts the atoms in a molecule and prints out the inventory list, i.e. the number of particular atoms, in the form: {H,He,C,N,O,Si,P,S,Cl}.";
findPartnerA::usage = "Identifies the product of A in the reaction A + B -> D + E by looking for the least changes in atomic composition. A result of 1 means A transforms to D, 2 means A transforms to E. 0 Means the routine can't decide because both options require the same number of transfered atoms.";

findPartnerB::usage = "Identifies the product of B in the reaction A + B -> D + E by looking for the least changes in atomic composition. A result of 1 means B transforms to D, 2 means B transforms to E. 0 Means the routine can't decide because both options require the same number of transfered atoms.";
\[CapitalDelta]AX::usage = "\[CapitalDelta]AX[{a_String,b_String,d_String,e_String}] determines the transport vector for species A in the reaction A + B -> D + E by looking for the least changes in atomic composition. It gives the number of particular atoms changing between A \[LongLeftRightArrow] B, in the form: {\[CapitalDelta]H,\[CapitalDelta]He,\[CapitalDelta]C,\[CapitalDelta]N,\[CapitalDelta]O,\[CapitalDelta]Si,\[CapitalDelta]P,\[CapitalDelta]S,\[CapitalDelta]Cl}. Negative numbers denote particle reception, positive numbers particle donation."; 
\[CapitalDelta]BX::usage = "\[CapitalDelta]BX[{a_String,b_String,d_String,e_String}] determines the transport vector for species B in the reaction A + B -> D + E by looking for the least changes in atomic composition. It gives the number of particular atoms changing between A \[LongLeftRightArrow] B, in the form: {\[CapitalDelta]H,\[CapitalDelta]He,\[CapitalDelta]C,\[CapitalDelta]N,\[CapitalDelta]O,\[CapitalDelta]Si,\[CapitalDelta]P,\[CapitalDelta]S,\[CapitalDelta]Cl}. Negative numbers denote particle reception, positive numbers particle donation."; 
sticky13C::usage = "sticky13C[{a_String, b_String, ...}] looks for the molecules H2CCC or H2CCCC in a list of molecules and gives TRUE if they are identical isotopomers AND it gives TRUE if it does NOT find two matching molecules!";
sticky18O::usage = "sticky18O[{a_String, b_String, ...}] looks for the molecules string COOC  in a list of molecules and gives TRUE if they are identical isotopomers AND it gives TRUE if it does NOT find two matching molecules!";
fortranOut::usage = "fortranOut[{n,t,a,b,c,d,e,f,g,h,i,j,k__}] transformiert die CSV formattierten Zeilen der UMIST Datenbank in das fixe FORTRAN77 format, das die routine cmread.f einliesst. Das Format wurde leicht veraendert um besonders Lange Molekuelbezeichnungen weiterhin einlesen zu koennen. Dazu wurde die Breite der ersten 4 Reaktanden von 10 auf 15 Zeichen und die der letzten beiden Produkte von 5 auf 10 Zeichen hochgesetzt.";

Eformat::usage = "Eformat[x,{total,ndigits}] returns Fortran 
E Format representation of the number x. Numbers whose exponents are 
  greater than 99 are treated as errors. Note that zero is detected   
by comparison with $MachineEpsilon and printed as 0.0000.. without   
the exponent.";

printF2::usage = "printF2[x,m,n] gives a string equivalent to number x
printed in FORMAT(Fm.n)\nx is real, complex or integer.\nm, n are integers"

countCatoms::usage="countCatoms[molecule] counts the Carbon atoms in a molecule."
countOatoms::usage="countOatoms[molecule] counts the Oxygen atoms in a molecule."
countHatoms::usage="countHatoms[molecule] counts the Hydrogen atoms in a molecule."
countHeatoms::usage="countHeatoms[molecule] counts the Helium atoms in a molecule."
countNatoms::usage="countNatoms[molecule] counts the Nitrogen atoms in a molecule."
countSatoms::usage="countSatoms[molecule] counts the Sulfur atoms in a molecule."
countSiatoms::usage="countSiatoms[molecule] counts the Silicium atoms in a molecule."
countClatoms::usage="countClatoms[molecule] counts the Chlorium atoms in a molecule."
countPatoms::usage="countPatoms[molecule] counts the Phosporus atoms in a molecule."
countFatoms::usage="countFatoms[molecule] counts the Fluor atoms in a molecule."
countFeatoms::usage="countFeatoms[molecule] counts the Iron atoms in a molecule."
countMgatoms::usage="countMgatoms[molecule] counts the Magnesium atoms in a molecule."
countNaatoms::usage="countNaatoms[molecule] counts the Sodium (Na) atoms in a molecule."


stripCharge::usage="stripCharge[molecule] removes the charge sign from the String."

isIon::usage="isIon[molecule] tests whether a molecule is ionized or not."
neutral::usage="neutral[molecule] removes any charge specifier from the chemical formula."
charge::usage="charge[molecule] returns the charge specifier from the chemical formula."

isAtom::usage="isAtom[molecule] tests whether a species is atomic or not. Ions, such as C+ count as atoms in that case."

isCNbound::usage="isCNbound[molecule] returns True if the molecule contains a C-N bound, i.e. neighbouring C and N atoms." 
isCHbound::usage="isCHbound[molecule] returns True if the molecule contains a C-H bound, i.e. neighbouring C and H atoms." 
isCHnbound::usage="isCHnbound[molecule] returns True if the molecule contains a CHn group. This finds all types of CHn groups only at the beginning or ending of the structure formula."
isCPbound::usage="isCPbound[molecule] returns True if the molecule contains a C-P bound, i.e. neighbouring C and P atoms." 
isCSbound::usage="isCSbound[molecule] returns True if the molecule contains a C-S bound, i.e. neighbouring C and S atoms." 
isCObound::usage="isCObound[molecule] returns True if the molecule contains a C-O bound, i.e. neighbouring C and O atoms." 
isC18Obound::usage="isC18Obound[molecule] returns True if the molecule contains a C-18O bound, i.e. neighbouring C and 18O atoms." 
isleft18Obound::usage="isleft18Obound[molecule] returns True if the molecule contains a 18O-O bound. Tries to find distinguish 18O-O sorting from O-18O in the following molecules:
HCOOH, HCOOH+,HCOOH2+,HCOOCH3,COOCH4+."
isright18Obound::usage="isright18Obound[molecule] returns True if the molecule contains a O-18O bound. tries to find distinguish 18O-O sorting from O-18O in the following molecules:
HCOOH, HCOOH+,HCOOH2+,HCOOCH3,COOCH4+." 
isleft13CNbound::usage="isleft13CNbound[molecule] tries to  distinguish N-13C sorting from 13C-N in the following molecules:
NCCN,NCCNCH3+,NCCNH+."
isCOCHnbound::usage="isCOCHnbound[molecule] returns True if the molecule contains a CO-CHn bound."

isCHisotop::usage="isCHisotop[molecule] returns True if the molecule contains a 13C-H bound, i.e. neighbouring 13C and N atoms." 
isNotCHisotop::usage="isNotCHisotop[molecule] returns False if the molecule contains a 13C-H bound, i.e. neighbouring 13C and N atoms." 
isCHnisotop::usage="isCHnisotop[molecule] returns True if the molecule contains a 13CHn bound, i.e. an isotopic CHn group." 
isNotCHnisotop::usage="isNotCHnisotop[molecule] returns False if the molecule contains a 13CHn bound, i.e. an isotopic CHn group." 


isCNisotop::usage="isCNisotop[molecule] returns True if the molecule contains a 13C-N bound, i.e. neighbouring 13C and N atoms." 
isNotCNisotop::usage="isNotCNisotop[molecule] returns False if the molecule contains a 13C-N bound, i.e. neighbouring 13C and N atoms." 
isCPisotop::usage="isCPisotop[molecule] returns True if the molecule contains a 13C-P bound, i.e. neighbouring 13C and P atoms." 
isNotCPisotop::usage="isNotCPisotop[molecule] returns False if the molecule contains a 13C-P bound, i.e. neighbouring 13C and P atoms." 
isCSisotop::usage="isCSisotop[molecule] returns True if the molecule contains a 13C-S bound, i.e. neighbouring 13C and S atoms." 
isNotCSisotop::usage="isNotCSisotop[molecule] returns False if the molecule contains a 13C-S bound, i.e. neighbouring 13C and S atoms." 
isCOisotop::usage="isCOisotop[molecule] returns True if the molecule contains a 13C-O bound, i.e. neighbouring 13C and O atoms." 
isNotCOisotop::usage="isNotCOisotop[molecule] returns False if the molecule contains a 13C-O bound, i.e. neighbouring 13C and O atoms." 

is13Cisotop::usage="is13Cisotop[molecule] returns True if the molecule contains a 13C."
is18Oisotop::usage="is18Oisotop[molecule] returns True if the molecule contains a 18O."

countCOgroups::usage="countCOgroups[molecule] tries to find CO bound molecules that are not obvious from the chemical formula, e.g. that CO2 bounds both oxygen atoms to the same carbon atom."
countCHngroups::usage="countCHngroups[molecule] tries to find CHn groups in the molecule."

in13C18O::usage="in13C18O[list] takes a list of chemical reactions and returns a list of all possible 13C and 18O isotopologue variants."
carbonizePlusSingle::usage="carbonizePlusSingle[molecule] returns all possible 13C isotopologues of molecule, preserving its charge."
oxygenizePlusSingle::usage="oxygenizePlusSingle[molecule] returns all possible 18O isotopologues of molecule, preserving its charge."


chemicalPalindrome::usage="chemicalPalindrome[molecule] tests whether a molecule is invariant against reversal, 
i.e. if the chemical formula is the same when reading it from the right to the left." 

InsertIsotopes::usage="InsertIsotopes[list] takes a list (CSV import) of chemical reactions and inserts one 13C and one 18O atom into reaction. The unreasonable permutations are cleaned afterwards."
rescaleRates::usage="rescaleRates[list] takes a list of isotopized reactions and rescales the reaction rates for cases where the isotopologues introduced new product branches. A uniform branching ratio among all branches is assumed."
testChecks::usage="testChecks[molecule] performs a series of regular expression checks on the molecule and all its isotopologues."

fromJ::usage="fromJ[molecule] returns the molecule name without the prepend \"J\" for surface bound species." 
toJ::usage="toJ[molecule] returns the molecule name with a prepend \"J\" for surface bound species." 
fracOut;
fortranOut2;padString;
Begin["`Private`"]

regelCheckIN[num_Integer] :=
    Module[ {alt,neu},
        alt = showR@num;
        neu = cleanUpIN@showR@num;
        Map[If[ MemberQ[neu,#],
                Style[#,Bold],
                Style[#,FontVariations->{"StrikeThrough"->True}]
            ]&,alt]
    ]

regelCheck[num_Integer] :=
    Module[ {alt, neu},
        alt = showR@num;
        If[ alt != {},(*to catch missing reactions*)
            Which[
             alt[[1, 2]] == "NN", neu = cleanUpNN@alt,
             alt[[1, 2]] == "IN", neu = cleanUpIN@alt,
             alt[[1, 2]] == "RA", neu = cleanUpRA@alt,
             alt[[1, 2]] == "AD", neu = cleanUpAD@alt,
             alt[[1, 2]] == "DR", neu = cleanUpDRV2@alt,
             alt[[1, 2]] == "CE", neu = cleanUpCE@alt,
             alt[[1, 2]] == "CL", neu = cleanUpCL@alt,
             alt[[1, 2]] == "MN", neu = cleanUpMN@alt,
             alt[[1, 2]] == "PH" || alt[[1, 2]] == "CR" || alt[[1, 2]] == "CP",
              neu = cleanUpPhoto@alt,
             True, Print["Unknown reaction " <> ToString@num];
                   neu = alt;],
            Print["Missing reaction " <> ToString@num];
        ];
        Map[If[ MemberQ[neu, #],
                Style[#, Bold],
                Style[#, FontVariations -> {"StrikeThrough" -> True}]
            ] &, alt]
    ]

onlyPositive[vek_List] :=
    If[ And@@NonNegative@vek,
        True,
        False
    ];
onlyNegative[vek_List] :=
    If[ And@@NonPositive@vek,
        True,
        False
    ];
mixedSigns[vek_List] :=
    If[ Or@@Positive@vek&&Or@@Negative@vek,
        True,
        False
    ];

stripCharge[s_String] := 
 If[StringLength[s] > 
    1 && (StringTake[s, -1] == "-" || StringTake[s, -1] == "+"), 
  StringTake[s, 1 ;; -2], s]
  
SetAttributes[stripCharge, Listable];

countElements[instr_String] := Module[{ins},
   ins = StringReplace[
     instr, {"Si" -> "\[DoubleStruckCapitalS]", 
      "Cl" -> "\[DoubleStruckCapitalC]", 
      "He" -> "\[DoubleStruckCapitalH]", 
      "Mg" -> "\[DoubleStruckCapitalM]", 
      "Na" -> "\[DoubleStruckCapitalN]", 
      "Fe" -> "\[DoubleStruckCapitalF]", "CRPHOT" -> "crphot", 
      "CRP" -> "crp", "CRPHOT" -> "crphot", "PHOTON" -> "photon"}];
   {countHatoms@instr, countHeatoms@instr, countCatoms@instr, 
    countNatoms@instr, countOatoms@instr, countFatoms@instr, 
    countNaatoms@instr, countMgatoms@instr, countSiatoms@instr, 
    countPatoms@instr, countSatoms@instr, countClatoms@instr, 
    countFeatoms@instr}];
SetAttributes[countElements, Listable];

countHCNOS[instr_String] :=
    Module[ {ins},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        {countHatoms@instr,countHeatoms@instr,countCatoms@instr,countNatoms@instr,countOatoms@instr,countSiatoms@instr,countPatoms@instr,countSatoms@instr,countClatoms@instr}
    ];

SetAttributes[countHCNOS,Listable];

isAtom[a_String] := If[Total@countHCNOS[a] == 1, True, False]

SetAttributes[isAtom,Listable];


(* atomic masses of H,C,N,O,S *)
\[ScriptCapitalM] = {1,12,14,16,32};

findPartnerA[{a_String,b_String,d_String,e_String}] :=
    Module[ {ad,ae},
        ae = countHCNOS@a-countHCNOS@e;
        ad = countHCNOS@a-countHCNOS@d;
        ad = countHCNOS@a-countHCNOS@d;
        Which[!mixedSigns@ae&&!mixedSigns@ad&&Total@Abs@ae<Total@Abs@ad,2,
        !mixedSigns@ae&&!mixedSigns@ad&&Total@Abs@ae>Total@Abs@ad,1,
        !mixedSigns@ae&&mixedSigns@ad,2,!mixedSigns@ad&&mixedSigns@ae,1,
        True,0]
    ];

findPartnerB[{a_String,b_String,d_String,e_String}] :=
    Module[ {bd,be},
        be = countHCNOS@b-countHCNOS@e;
        bd = countHCNOS@b-countHCNOS@d;
        Which[!mixedSigns@be&&!mixedSigns@bd&&Total@Abs@be<Total@Abs@bd,2,
        !mixedSigns@be&&!mixedSigns@bd&&Total@Abs@be>Total@Abs@bd,1,
        !mixedSigns@be&&mixedSigns@bd,2,!mixedSigns@bd&&mixedSigns@be,1,
        True,0]
    ];


\[CapitalDelta]AD[{a_String,b_String,d_String,e_String}] :=
    countHCNOS@a-countHCNOS@d;
\[CapitalDelta]AE[{a_String,b_String,d_String,e_String}] :=
    countHCNOS@a-countHCNOS@e;
\[CapitalDelta]BD[{a_String,b_String,d_String,e_String}] :=
    countHCNOS@b-countHCNOS@d;
\[CapitalDelta]BE[{a_String,b_String,d_String,e_String}] :=
    countHCNOS@b-countHCNOS@e;

\[CapitalDelta]AX[{a_String,b_String,d_String,e_String}] :=
    Module[ {ad,ae},
        ae = countHCNOS@a-countHCNOS@e;
        ad = countHCNOS@a-countHCNOS@d;
        Which[!mixedSigns@ae&&!mixedSigns@ad&&Total@Abs@ae<Total@Abs@ad,ae,
        !mixedSigns@ae&&!mixedSigns@ad&&Total@Abs@ae>Total@Abs@ad,ad,
        !mixedSigns@ae&&mixedSigns@ad,ae,!mixedSigns@ad&&mixedSigns@ae,ad,
        True,{}]
    ];


\[CapitalDelta]BX[{a_String,b_String,d_String,e_String}] :=
    Module[ {bd,be},
        be = countHCNOS@b-countHCNOS@e;
        bd = countHCNOS@b-countHCNOS@d;
        Which[!mixedSigns@be&&!mixedSigns@bd&&Total@Abs@be<Total@Abs@bd,be,
        !mixedSigns@be&&!mixedSigns@bd&&Total@Abs@be>Total@Abs@bd,bd,
        !mixedSigns@be&&mixedSigns@bd,be,!mixedSigns@bd&&mixedSigns@be,bd,
        True,{}]
    ];


sticky13C[a__List] :=
    Module[ {pos},
        pos = Cases[Flatten@MapIndexed[If[ StringMatchQ[#1,RegularExpression["^H2(C|13C)(C|13C)(C|13C)$|^H2(C|13C)(C|13C)(C|13C)(C|13C)$"]],
                                           #2,
                                           0
                                       ]&,a],Except[0]];
        If[ Length[pos]==2,
            If[ a[[pos[[1]]]]===a[[pos[[2]]]],
                True,
                False
            ],
            True
        ]
    ];


sticky18O[a__List] :=
    Module[ {pos},
        pos = Cases[Flatten@MapIndexed[If[ StringMatchQ[#1,RegularExpression["^H?(C|13C)(O|18O)(O|18O)(C|13C)H[34]{1}[+]?$"]],
                                           #2,
                                           0
                                       ]&,a],Except[0]];
        If[ Length[pos]==2,
            If[ StringCases[a[[pos[[1]]]],RegularExpression["(C|13C)(O|18O)(O|18O)(C|13C)"]]===StringCases[a[[pos[[2]]]],RegularExpression["(C|13C)(O|18O)(O|18O)(C|13C)"]],
                True,
                False
            ],
            True
        ]
    ];


testChecks[instr_String] :=
    Module[ {tab,repl,list,head},
        repl = {True->Item["T",Background->Green],False->Item["F",Background->Red]};
        head = Prepend[Graphics[Rotate[Text[Style[#]],-Pi/2.5],ImageSize->{30,80}]&/@{"CHn-bound","CHn-isotope","not CHn-isotope","CH-bound","CH-isotope","not CH-isotope","CO-bound","CO-isotope","not CO-isotope","CN-bound","CN-isotope","not CN-isotope","C18O-bound","# of CO bonds","# of Subscript[CH, n] groups"},""];
        list = Flatten@Prepend[{carbonizePlusSingle@#,oxygenizePlusSingle/@carbonizePlusSingle@#},#]&@instr;
        tab = Transpose@{list,isCHnbound@list,isCHnisotop@list,isNotCHnisotop@list,isCHbound@list,isCHisotop@list,isNotCHisotop@list,isCObound@list,isCOisotop@list,isNotCOisotop@list,isCNbound@list,isCNisotop@list,isNotCNisotop@list,isC18Obound@list,countCOgroups@list,countCHngroups@list};
        Grid[Prepend[tab/.repl,head],Dividers->{{False,True,{False,False,True}},{False,{True}}},Spacings->{{0,1,{0}},1}]
    ];

neutral[in_String] :=
    If[ isIon[in],
        StringDrop[in,-1],
        in
    ];

SetAttributes[countCatoms,Listable];
countCatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="C",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            count+=9*StringCount[in,"10"];
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"C"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"C"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"C"]}],
            count = 0;
            count
        ]
    ];

countNatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="N",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"N"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"N"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"N"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countNatoms,Listable];

countSatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="S",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"S"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"S"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"S"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countSatoms,Listable];

countHeatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"He"->"\[DoubleStruckCapitalH]","Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="\[DoubleStruckCapitalH]",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalH]"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalH]"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"\[DoubleStruckCapitalH]"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countHeatoms,Listable];

countSiatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"\[DoubleStruckCapitalS]","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="\[DoubleStruckCapitalS]",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalS]"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalS]"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"\[DoubleStruckCapitalS]"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countSiatoms,Listable];

countClatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"\[CapitalSigma]","Cl"->"\[DoubleStruckCapitalC]","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="\[DoubleStruckCapitalC]",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalC]"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"\[DoubleStruckCapitalC]"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"\[DoubleStruckCapitalC]"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countClatoms,Listable];

countPatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="P",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"P"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"P"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"P"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countPatoms,Listable];

countFatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="F",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"F"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"F"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"F"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countFatoms,Listable];

countMgatoms[instr_String] := Module[{in, ins,  count},
   ins = StringReplace[
     instr, {"Si" -> "\[DoubleStruckCapitalS]", 
      "Cl" -> "\[DoubleStruckCapitalC]", 
      "He" -> "\[DoubleStruckCapitalH]", 
      "Mg" -> "\[DoubleStruckCapitalM]", 
      "Na" -> "\[DoubleStruckCapitalN]", 
      "Fe" -> "\[DoubleStruckCapitalF]", "CRPHOT" -> "crphot", 
      "CRP" -> "crp", "CRPHOT" -> "crphot", "PHOTON" -> "photon"}];
   If[StringLength[ins] > 0,
    If[StringTake[ins, -1] == "\[DoubleStruckCapitalM]", (count = 1; 
      in = StringTake[ins, StringLength[ins] - 1]), (count = 0; 
      in = ins;)];
    (*count+=9*StringCount[in,"10"];*)
    count += Total@Table[If[
        StringMatchQ[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalM]"][[i]]], 
         DigitCharacter], 
        ToExpression[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalM]"][[i]]]], 
        1], {i, 1, StringCount[in, "\[DoubleStruckCapitalM]"]}], 
    count = 0; count]];
SetAttributes[countMgatoms, Listable];



countFeatoms[instr_String] := Module[{in, ins,  count},
   ins = StringReplace[
     instr, {"Si" -> "\[DoubleStruckCapitalS]", 
      "Cl" -> "\[DoubleStruckCapitalC]", 
      "He" -> "\[DoubleStruckCapitalH]", 
      "Mg" -> "\[DoubleStruckCapitalM]", 
      "Na" -> "\[DoubleStruckCapitalN]", 
      "Fe" -> "\[DoubleStruckCapitalF]", "CRPHOT" -> "crphot", 
      "CRP" -> "crp", "CRPHOT" -> "crphot", "PHOTON" -> "photon"}];
   If[StringLength[ins] > 0,
    If[StringTake[ins, -1] == "\[DoubleStruckCapitalF]", (count = 1; 
      in = StringTake[ins, StringLength[ins] - 1]), (count = 0; 
      in = ins;)];
    (*count+=9*StringCount[in,"10"];*)
    count += Total@Table[If[
        StringMatchQ[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalF]"][[i]]], 
         DigitCharacter], 
        ToExpression[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalF]"][[i]]]], 
        1], {i, 1, StringCount[in, "\[DoubleStruckCapitalF]"]}], 
    count = 0; count]];
SetAttributes[countFeatoms, Listable];

countNaatoms[instr_String] := Module[{in, ins, count},
   ins = StringReplace[
     instr, {"Si" -> "\[DoubleStruckCapitalS]", 
      "Cl" -> "\[DoubleStruckCapitalC]", 
      "He" -> "\[DoubleStruckCapitalH]", 
      "Mg" -> "\[DoubleStruckCapitalM]", 
      "Na" -> "\[DoubleStruckCapitalN]", 
      "Fe" -> "\[DoubleStruckCapitalF]", "CRPHOT" -> "crphot", 
      "CRP" -> "crp", "CRPHOT" -> "crphot", "PHOTON" -> "photon"}];
   If[StringLength[ins] > 0,
    If[StringTake[ins, -1] == "\[DoubleStruckCapitalN]", (count = 1; 
      in = StringTake[ins, StringLength[ins] - 1]), (count = 0; 
      in = ins;)];
    (*count+=9*StringCount[in,"10"];*)
    count += Total@Table[If[
        StringMatchQ[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalN]"][[i]]], 
         DigitCharacter], 
        ToExpression[
         StringTake[ins, 
          1 + StringPosition[in, "\[DoubleStruckCapitalN]"][[i]]]], 
        1], {i, 1, StringCount[in, "\[DoubleStruckCapitalN]"]}], 
    count = 0; count]];
 SetAttributes[countNaatoms, Listable];



isCNbound[instr_String] :=
    Module[ {ins,regEx},
(*regEx="C[2-9]*N|N[2-9]*(C|13C)|CH[2-9]*N";(* this finds all types of C-N bonds *)*)
(*regEx="NCCN|N(C13C|13CC)N|(?<!C[2-9]{1,1})(?<!13)CN(?!2)|^N(C|13C)|H2N(C|13C)|^(CNC|CN13C|13CNC)|\\wN(C|13C)";
regEx="NCCN|N(C13C|13CC)N|\\b(C|13C)N|(C|13C)N[+\-]?\\b";*)
        regEx = "\\b(C|13C)N|(?<=(H\\d))(C|13C)N[+\-]?\\b|H[23]?(C|13C)N|H[23]?N(C|13C)|NCCN|N(C13C|13CC)N|S(i)?N(C|13C)|O(C|13C)N";
        (* this finds only identifiable C-N bonds *)
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCNbound,Listable];

isCNisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="13C[2-9]*N|N[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*N|13C(D|D[2-9])*(H|H[2-9])*N";*)
        regEx = "\\b13CN|(?<=(H\\d))13CN[+\-]?\\b|H[23]?13CN|H[23]?N13C|N(C13C|13CC)N|S(i)?N13C|O13CN";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCNisotop,Listable];

isNotCNisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="(?<!13)CN|NC";*)(*negative lookbehind construction (?<!a)b : find b not preceded by a *)
(*regEx="13C[2-9]*N|N[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*N|13C(D|D[2-9])*(H|H[2-9])*N";*)
        regEx = "\\b13CN|(?<=(H\\d))13CN[+\-]?\\b|H[23]?13CN|H[23]?N13C|N(C13C|13CC)N|S(i)?N13C|O13CN";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Cl"->"cl","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCNisotop,Listable];

isCPbound[instr_String] :=
    Module[ {ins,regEx},
(*regEx="C[2-9]*N|N[2-9]*(C|13C)|CH[2-9]*N";(* this finds all types of C-N bonds *)*)
(*regEx="NCCN|N(C13C|13CC)N|(?<!C[2-9]{1,1})(?<!13)CN(?!2)|^N(C|13C)|H2N(C|13C)|^(CNC|CN13C|13CNC)|\\wN(C|13C)";
regEx="NCCN|N(C13C|13CC)N|\\b(C|13C)N|(C|13C)N[+\-]?\\b";*)
        regEx = "\\b(C|13C)N|(?<=(H\\d))(C|13C)N[+\-]?\\b|H[23]?(C|13C)N|H[23]?N(C|13C)|NCCN|N(C13C|13CC)N|S(i)?N(C|13C)|O(C|13C)N";
        regEx = "^(C|13C)(C|13C)P[+]?$|^(C|13C)P[+]?$|^H(C|13C)P[+]?$|^(C|13C)H2PH$|^P(C|13C)H[234]{1}[+]?$";
        (* this finds only identifiable C-N bonds *)
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCPbound,Listable];

isCPisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="13C[2-9]*N|N[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*N|13C(D|D[2-9])*(H|H[2-9])*N";*)
        regEx = "\\b13CN|(?<=(H\\d))13CN[+\-]?\\b|H[23]?13CN|H[23]?N13C|N(C13C|13CC)N|S(i)?N13C|O13CN";
        regEx = "^(C|13C)13CP[+]?$|^13CP[+]?$|^H13CP[+]?$|^13CH2PH$|^P13CH[234]{1}[+]?$";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCPisotop,Listable];

isNotCPisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="(?<!13)CN|NC";*)(*negative lookbehind construction (?<!a)b : find b not preceded by a *)
(*regEx="13C[2-9]*N|N[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*N|13C(D|D[2-9])*(H|H[2-9])*N";*)
        regEx = "\\b13CN|(?<=(H\\d))13CN[+\-]?\\b|H[23]?13CN|H[23]?N13C|N(C13C|13CC)N|S(i)?N13C|O13CN";
        regEx = "^(C|13C)13CP[+]?$|^13CP[+]?$|^H13CP[+]?$|^13CH2PH$|^P13CH[234]{1}[+]?$";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCPisotop,Listable];

countSatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="S",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"S"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"S"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"S"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countNatoms,Listable];

isCSbound[instr_String] :=
    Module[ {ins,regEx},
(*regEx="C[2-9]*S|S[2-9]*(C|13C)|CH[2-9]*S";(* this finds all types of C-N bonds *)*)
        regEx = "CS|S[2-9]*(C|13C)\\D*|CH[2-9]*S";(* this finds only identifiable C-N bonds *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCSbound,Listable];

isCSisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "13C[2-9]*S|S[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*S|13C(D|D[2-9])*(H|H[2-9])*S";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCSisotop,Listable];

isNotCSisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "(?<!13)CS|SC";(*negative lookbehind construction (?<!a)b : find b not preceded by a *)(* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCSisotop,Listable];

countOatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="O",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"O"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"O"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"O"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countOatoms,Listable];

isCObound[instr_String] :=
    Module[ {ins,regEx},
(*regEx="C[2-9]*O|O[2-9]*(C|13C)|CH[2-9]*O";(* this finds all types of C-N bonds *)*)
        regEx = "C(O|18O)|(O|18O)[2-9]*(C|13C)\\D*|CH[2-9]*(O|18O)";
        regEx = "(C|13C)H[2-3]*(O|18O)|(C|13C)(O|18O)|(O|18O)(C|13C)"; (*ergibt einige Falschmeldungen*)
        regEx = "^(C|13C)H[2-3]{1}(C|13C)[H]{0,1}(O|18O)|^(C|13C)H[23]{1}(O|18O)H|^(C|13C)(O|18O)|^H[23]?(C|13C)(O|18O)|^H[23]{1}(C|13C)(C|13C)(O|18O)|^HN(C|13C)(O|18O)[+]?$|^H(O|18O)(C|13C)|^(O|18O)(C|13C)|^(C|13C)H3(O|18O)(C|13C)H[34]{1}";
        (* this finds only identifiable C-O bonds *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCObound,Listable];

isC18Obound[instr_String] :=
    Module[ {ins,regEx},
        regEx = "C18O|CH18O|^[H]?(C|13C)O18O[+]?$|CO18O(C|13C)|^[H]?18O(C|13C)[SNH]?|CH318O";
        regEx = "^(C|13C)H[2-3]{1}(C|13C)[H]{0,1}18O|^(C|13C)H[23]{1}18OH|^(C|13C)18O|^H[23]?(C|13C)18O|^H[23]{1}(C|13C)(C|13C)18O|^HN(C|13C)18O[+]?$|^H18O(C|13C)|^18O(C|13C)|^H[23]?(C|13C)(O18O|18OO|18O18O)(C|13C)|^(C|13C)(O18O|18OO|18O18O)(C|13C)|^H(C|13C)O18OH|^H(C|13C)O18O|^(C|13C)H318O(C|13C)H[34]{1}|^(C|13C)O18O[+]?$";
        (* this finds only identifiable C-18O bonds *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];

isNotC18Obound[instr_String] :=
    Module[ {ins,regEx},
        regEx = "C18O|CH18O|^[H]?(C|13C)O18O[+]?$|CO18O(C|13C)|^[H]?18O(C|13C)[SNH]?|CH318O";
        regEx = "^(C|13C)H[2-3]{1}(C|13C)[H]{0,1}18O|^(C|13C)H[23]{1}18OH|^(C|13C)18O|^H[23]?(C|13C)18O|^H[23]{1}(C|13C)(C|13C)18O|^HN(C|13C)18O[+]?$|^H18O(C|13C)|^18O(C|13C)|^H[23]?(C|13C)(O18O|18OO|18O18O)(C|13C)|^(C|13C)(O18O|18OO|18O18O)(C|13C)|^H(C|13C)O18OH|^H(C|13C)O18O|^(C|13C)H318O(C|13C)H[34]{1}^(C|13C)O18O[+]?$";
        (* this finds only identifiable C-18O bonds *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isC18Obound,Listable];
SetAttributes[isNotC18Obound,Listable];

isCOisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="13C[2-9]*(O|18O)|(?<!C)O13C|(?<!C)18O13C|(?<!C)18O[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*(O|18O)|13C(D|D[2-9])*(H|H[2-9])*(O|18O)";(* so far only checking for 13C *)*)
        regEx = "13CH[2-9]*(O|18O)|13COC|13C18OC|^(O|18O)13C|^H(O|18O)13C|(O|18O)(O|18O)13C|13CO(?!C)|13C18O(?!C)|^CH3(O|18O)13C";
        regEx = "^(C|13C)H[2-3]{1}13C[H]{0,1}(O|18O)|^13CH[23]{1}(O|18O)H|^13C(O|18O)|^H[23]?13C(O|18O)|^H[23]{1}(C|13C)13C(O|18O)|^HN13C(O|18O)[+]?$|^H(O|18O)13C|^(O|18O)13C|^H[23]?C(O|18O)(O|18O)13C|^C(O|18O)(O|18O)13C|^13CH3(O|18O)(C|13C)H[34]{1}|^(C|13C)H3(O|18O)13CH[34]{1}";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCOisotop,Listable];

isNotCOisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "13CH[2-9]*(O|18O)|13COC|13C18OC|^(O|18O)13C|^H(O|18O)13C|(O|18O)(O|18O)13C|13CO(?!C)|13C18O(?!C)|^CH3(O|18O)13C";(*negative lookbehind construction (?<!a)b : find b not preceded by a *)(* so far only checking for 13C *)
        regEx = "^(C|13C)H[2-3]{1}13C[H]{0,1}(O|18O)|^13CH[23]{1}(O|18O)H|^13C(O|18O)|^H[23]?13C(O|18O)|^H[23]{1}(C|13C)13C(O|18O)|^HN13C(O|18O)[+]?$|^H(O|18O)13C|^(O|18O)13C|^H[23]?C(O|18O)(O|18O)13C|^C(O|18O)(O|18O)13C";
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCOisotop,Listable];

countCOgroups[instr_String] :=
    Module[ {ins,count,regEx,negregEx},
(*regEx="C[2-9]*N|N[2-9]*(C|13C)|CH[2-9]*N";(* this finds all types of C-N bonds *)*)
        regEx = "(C|13C)H[2-3]*(O|18O)|(C|13C)(O|18O)|(O|18O)(C|13C)";
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        count = StringCount[ins,RegularExpression[regEx],Overlaps->False];
        (* spezialfaelle, die durch die obige regEx nicht erkannt werden, bzw. die
        einige Chemie-Kenntnisse erfordern, beispielsweise, dass bei CO2, die O-atome beide
        an das gleiche C-atom gebunden sind *)
        If[ !StringFreeQ[ins,RegularExpression["H3(18O|O)(13C|C)H[34]"]],
            count+=1
        ];
        If[ !StringFreeQ[ins,RegularExpression["^[H]?(13C|C)O18O[+-]?$|^[H]?(13C|C)O2[+-]?$"]],
            count+=1
        ];
        If[ !StringFreeQ[ins,RegularExpression["^H(13C|C)O18OH[2]?[+-]?$|^H(13C|C)18OOH[2]?[+-]?$|^H(13C|C)OOH[2]?[+-]?$$"]],
            count+=1
        ];
        If[ !StringFreeQ[ins,RegularExpression["^H(13C|C)(O18O|18OO|OO)(C|13C)H3$"]],
            count+=1
        ];
        (* explicitily look for the exclusions and if found, set count->0 *)
        (* molecules to look for: C2H5OH,C2H5OH2,C3H2O,C3O,C2O,H3C3O,H5C2O2,HC2O,HC3O, as well as their ions and isotopomeres *)
        negregEx = "^C13C|^C[2-9]?13C|^H[35]{1}C[2-9]{1}|^H[35]{1}C13C|^HC2(O|18O)|^HC13C(O|18O)|^HC3(O|18O)|^HC213C(O|18O)";
        If[ Not@StringFreeQ[ins,RegularExpression[negregEx]],
            count = 0
        ];
        count
    ];

SetAttributes[countCOgroups,Listable];

is13Cisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="13C[2-9]*(O|18O)|(?<!C)O13C|(?<!C)18O13C|(?<!C)18O[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*(O|18O)|13C(D|D[2-9])*(H|H[2-9])*(O|18O)";(* so far only checking for 13C *)*)
        regEx = "13C";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[is13Cisotop,Listable];

is18Oisotop[instr_String] :=
    Module[ {ins,regEx},
(*regEx="13C[2-9]*(O|18O)|(?<!C)O13C|(?<!C)18O13C|(?<!C)18O[2-9]*13C|13C(H|H[2-9])*(D|D[2-9])*(O|18O)|13C(D|D[2-9])*(H|H[2-9])*(O|18O)";(* so far only checking for 13C *)*)
        regEx = "18O";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[is18Oisotop,Listable];

isleft18Obound[instr_String] :=
    Module[ {ins,regEx},
(* tries to find distinguish 18O-O sorting from O-18O in the following molecules:
HCOOH, HCOOH+,HCOOH2+,HCOOCH3,COOCH4+ *)
        regEx = "^H(C|13C)18OO|^(C|13C)18OO";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isleft18Obound,Listable];

isright18Obound[instr_String] :=
    Module[ {ins,regEx},
(* tries to find distinguish 18O-O sorting from O-18O in the following molecules:
HCOOH, HCOOH+,HCOOH2+,HCOOCH3,COOCH4+ *)
        regEx = "^H(C|13C)O18O|^(C|13C)O18O";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isright18Obound,Listable];

isleft13CNbound[instr_String] :=
    Module[ {ins,regEx},
(* tries to find distinguish N-13C sorting from 13C-N in the following molecules:
NCCN,NCCNCH3+,NCCNH+ *)
        regEx = "^N13CCN";
        (* so far only checking for 13C *)
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isleft13CNbound,Listable];

countHatoms[instr_String] :=
    Module[ {in,ins,count},
        ins = StringReplace[instr,{"He"->"\[ScriptCapitalH]","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon"}];
        If[ StringLength[ins]>0,
            If[ StringTake[ins,-1]=="H",
                (count = 1;
                 in = StringTake[ins,StringLength[ins]-1]),
                (count = 0;
                 in = ins;)
            ];
            (*count+=9*StringCount[in,"10"];*)
            count+=Total@Table[If[ StringMatchQ[StringTake[ins,1+StringPosition[in,"H"][[i]]],DigitCharacter],
                                   ToExpression[StringTake[ins,1+StringPosition[in,"H"][[i]]]],
                                   1
                               ],{i,1,StringCount[in,"H"]}],
            count = 0;
            count
        ]
    ];
SetAttributes[countHatoms,Listable];

isCHbound[instr_String] := Module[{ins, regEx},
  (*regEx="C[2-9]*N|N[2-9]*(C|13C)|CH[2-9]*N"
  ;(*this finds all types of C-H bonds*)*)(*regEx=
  "C[2-9]*H[2-5]*|(?<!C[2-9]{1})H[2-5]*(C|13C)(?![2-9]{1})";*)
  (* update to omit NH2C...*)
  regEx = 
   "C[2-9]*H[2-5]*|\\b(?!=N)(?<!C[2-9]{1})H[2-5]*(C|13C)(?![2-9]{1})";
  (*this finds only identifiable C-N bonds*)
  ins = StringReplace[
    instr, {"He" -> "he", "Cl" -> "cl", "CRPHOT" -> "crphot", 
     "CRP" -> "crp", "CRPHOT" -> "crphot", "PHOTON" -> "photon", 
     "Na" -> "na"}];
  Not@StringFreeQ[ins, RegularExpression[regEx]]]
SetAttributes[isCHbound,Listable];

isCHisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "13CH[2-5]*|^H[2-5]*13C(?![2-9]{1})";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCHisotop,Listable];

isNotCHisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "(C|13C)H[2-5]*|^H[2-5]*(C|13C)(?![2-9]{1})";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCHisotop,Listable];

isCHnbound[instr_String] :=
    Module[ {ins,regEx},
(* this finds all types of CHn groups only at the beginning or ending of the structure formula *)
        regEx = "^(C|13C)H[2-4]{1}|(?<!C)CH[2-4]{1}$&(?<!13)CH[2-4]{1}$|^(13C|C)H5|[(Si)(CH3)PNO](13C|C)H[34][+]*$";
        regEx = "^(C|13C)H[2-5]{1,1}|[abd-z|ABD-Z](13C|C)H[2-4]{1,1}[+]?$|^(13C|C)H3(13C|C)H3|^H[2-3]{1}C(?!\\d{1,1})|^H2(C13C|13CC)|^H[2-3]{1}13C[abd-zABD-Z]|^H[2-3]{1}(C18O|13C18O)";(* this finds only identifiable C-Hn bonds does not allow for carbon chains with CHn at the end *)
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCHnbound,Listable];

isCHnisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "^13CH[2-5]{1}|(?<!C)[2-9]*13CH[2-5]{1}|CH313CH3";
        (*regEx="^13CH[2-4]{1}|(?<!(C7|C6|C5|C4|C3|C2))13CH[2-4]{1}&(?<!C)13CH[2-4]{1}|(C|13C)(C|13)(C|13C)13CH[2-4]{1}|^13CH5";*)(* so far only checking for 13C *)
        regEx = "^13CH[2-5]{1,1}|[abd-z|ABD-Z]13CH[2-4]{1,1}[+]?$|^13CH3CH3|^CH313CH3|^H[2-3]{1}13C[abd-zABD-Z]|^H[2-3]{1}13C18O|^H[2-3]{1}13CCl|^H[2-3]{1}13C(C|13C)(O|18O)[+]?|^H[2-3]{1}13C(C|13C)(C|13C)(C|13C)?[+]?";
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        Not@StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isCHnisotop,Listable];

isNotCHnisotop[instr_String] :=
    Module[ {ins,regEx},
        regEx = "^13CH[2-5]{1}|(?<!C)![2-9]*13CH[2-5]{1}|CH313CH3";
        regEx = "^13CH[2-4]{1}|(?<!C)13CH[2-4]{1}|(C|13C)(C|13)C13CH[2-4]{1}|^13CH5";
        regEx = "^13CH[2-5]{1,1}|[abd-z|ABD-Z]13CH[2-4]{1,1}[+]?$|^13CH3CH3|^CH313CH3|^H[2-3]{1}13C[abd-zABD-Z]|^H[2-3]{1}13C18O|^H[2-3]{1}13CCl|^H[2-3]{1}13C(C|13C)(O|18O)[+]?|^H[2-3]{1}13C(C|13C)(C|13C)(C|13C)?[+]?";(* so far only checking for 13C *)
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        StringFreeQ[ins,RegularExpression[regEx]]
    ];
SetAttributes[isNotCHnisotop,Listable];

countCHngroups[instr_String] :=
    Module[ {ins,count,regEx},
(*regEx="C[2-9]*N|N[2-9]*(C|13C)|CH[2-9]*N";(* this finds all types of C-N bonds *)*)
        regEx = "CH[2-5]{1}";
        regEx = "^(C|13C)H[2-5]{1,1}|[abd-z|ABD-Z](13C|C)H[2-4]{1,1}[+]?$|^(13C|C)H3(13C|C)H3|^H[2-3]{1}C(?!\\d{1,1})|^H2(C13C|13CC)|^H[2-3]{1}13C[abd-zABD-Z]|^H[2-3]{1}(C18O|13C18O)";
        ins = StringReplace[instr,{"He"->"he","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        count = StringCount[ins,RegularExpression[regEx]];
        If[ !StringFreeQ[ins,RegularExpression["^(C|13C)H3(C|13C)H3$"]],
            count+=1
        ];
        count
    ];
SetAttributes[countCHngroups,Listable];

isCOCHnbound[instr_String] :=
    Module[ {ins},
        ins = StringReplace[instr,{"Si"->"si","Cl"->"cl","CRPHOT"->"crphot","CRP"->"crp","CRPHOT"->"crphot","PHOTON"->"photon","Na"->"na"}];
        If[ isCHnbound[instr],
            countCOgroups@instr+countCHngroups@instr>countCatoms@instr,
            False
        ]
    ];
SetAttributes[isCOCHnbound,Listable];

Palindrome[word_] :=
    If[ word==StringReverse[word],
        "True",
        "False"
    ];
chemicalPalindrome[word_String] :=
    Module[ {rest,length},
        If[ isIon[word],
            rest = StringDrop[word,-1],
            rest = word
        ];
        length = StringLength[rest];
        Which[
        length>=6,If[ StringTake[rest,3]==StringTake[rest,-3]||StringTake[rest,3]==StringReverse[StringTake[rest,-3]],
                      True,
                      False
                  ],
        length>=4,If[ StringTake[rest,2]==StringTake[rest,-2]||StringTake[rest,2]==StringReverse[StringTake[rest,-2]],
                      True,
                      False
                  ],
        length==3,If[ StringTake[rest,1]==StringTake[rest,-1],
                      True,
                      False
                  ],
        True,False]
    ];
SetAttributes[chemicalPalindrome,Listable];

palindromeRules = {"CH313CH3"->"13CH3CH3",
"CH313CH3+"->"13CH3CH3+",
"CH3CO13CH3"->"13CH3COCH3",
"CH3CO13CH3+"->"13CH3COCH3+",
"CH3C18O13CH3"->"13CH3C18OCH3",
"CH3C18O13CH3+"->"13CH3C18OCH3+",
"CH3O13CH3"->"13CH3OCH3",
"CH3O13CH3+"->"13CH3OCH3+",
"CH318O13CH3"->"13CH318OCH3",
"CH318O13CH3+"->"13CH318OCH3+",
"CN13C+"->"13CNC+",
"NC13CN"->"N13CCN"};

removeDoublesRule :=
    {a___List,x_List,y_List,b___List}:>{a,x,b}/;x[[3;;7]]===y[[3;;7]]
(*Complement[dataNoCL,dataNoCL//.removeDoublesRule];*)
rate[\[Alpha]_,\[Beta]_,\[Gamma]_,T_] :=
    \[Alpha]*(T/300)^\[Beta]*Exp[-\[Gamma]/T]
(*elements={"H","He", "C","O","S","Si","N","F","P","Na","Mg","Fe","Cl","e"};

photons={"CRPHOT","CRP","PHOTON"};
carbons={"C","C2","C3","C4","C5","C6","C7","C8","C9","C10"};
(*hydrogens={"H","H2","H3","H4","H5","H6","H7","D","D2","D3"};*)
hydrogens={"H","H2","H3","H4","H5","H6","H7"};
oxygens={"O","O2"};
nitrogens={"N","N2"};
sulphurs={"S","S2"};

chemicalContent={RotateLeft[hydrogens],"He", RotateLeft[carbons],RotateLeft[oxygens],RotateLeft[sulphurs],"Si",RotateLeft[nitrogens],"F","P","Na","Mg","Fe","Cl","e"};

ruleH={"H2"->"H1D1","H3"->"H2D1","H4"->"H3D1","H5"->"H4D1","H6"->"H5D1","H7"->"H6D1","H1"->"D1"};
ruleHmulti={"H2"->"H1D1","H3"->"H2D1","H4"->"H3D1","H5"->"H4D1","H6"->"H5D1","H7"->"H6D1","H1"->"D1","H1D1"->"D2","H2D1"->"H1D2","H3D1"->"H2D2","H4D1"->"H3D2","H5D1"->"H4D2","H6D1"->"H5D2","H1D2"->"D3","H2D2"->"H1D3","H3D2"->"H2D3","H4D2"->"H3D3","H5D2"->"H4D3"};
ruleC={"C2"->"C1X1","C3"->"C2X1","C4"->"C3X1","C5"->"C4X1","C6"->"C5X1","C7"->"C6X1","C8"->"C7X1","C9"->"C8X1","C10"->"C9X1","C1"->"X1"}; (*X=^13C*)
ruleO={"O2"->"O1Y1","O1"->"Y1"}; (*Y=^18O*)

enumerateRule={x_String/;!StringMatchQ[x,__~~NumberString~~EndOfString]:>x~~"1",x_String/;StringMatchQ[x,__~~NumberString~~EndOfString]:>x}
{x_String/;!StringMatchQ[x,__~~NumberString~~EndOfString]:>x~~"1",x_String/;StringMatchQ[x,__~~NumberString~~EndOfString]:>x}

denumerateRule={x_String/;StringMatchQ[x,__~~"1"~~EndOfString]:>StringDrop[x,-1]}
{x_String/;StringMatchQ[x,__~~"1"~~EndOfString]:>StringDrop[x,-1]}

insertOne[x_String]:=StringJoin@@(Evaluate[StringCases[x,RegularExpression["(He|H|C|O|S|Si|N|P|Na|Mg|Fe|Cl|X|Y|e)(\d)?"]]]/.enumerateRule)
deuterate[x_String]:=StringReplace[StringReplaceList[insertOne[x],ruleH],"1"->""]

carbonizeSingle[x_String]:=StringReplace[StringReplace[StringReplaceList[insertOne[x],ruleC],"1"->""],"X"->"13C"]
oxygenizeSingle[x_String]:=StringReplace[StringReplace[StringReplaceList[insertOne[x],ruleO],"1"->""],"Y"->"18O"]
carbonizeChain[x_String]:=StringReplace[StringReplaceList[insertOne[x],ruleC],"1"->""]

isIon[x_String]:=StringMatchQ[x,__~~RegularExpression["[+-]"]~~EndOfString]
charge[x_String]:=If[isIon[x],StringTake[x,-1],""]
chargeList[x_List]:=Thread[ion[x]]*)

norm2doubleStruck = {"C11" -> "\[DoubleStruckCapitalW]", 
   "C10" -> "\[DoubleStruckCapitalX]", 
   "Cl" -> "\[DoubleStruckCapitalC]", 
   "Si" -> "\[DoubleStruckCapitalS]", 
   "He" -> "\[DoubleStruckCapitalH]", 
   "Na" -> "\[DoubleStruckCapitalN]", 
   "Mg" -> "\[DoubleStruckCapitalM]", 
   "Fe" -> "\[DoubleStruckCapitalF]"};

doubleStruck2norm = {"\[DoubleStruckCapitalW]" -> "C11", 
   "\[DoubleStruckCapitalX]" -> "C10", 
   "\[DoubleStruckCapitalC]" -> "Cl", 
   "\[DoubleStruckCapitalS]" -> "Si", 
   "\[DoubleStruckCapitalH]" -> "He", 
   "\[DoubleStruckCapitalN]" -> "Na", 
   "\[DoubleStruckCapitalM]" -> "Mg", 
   "\[DoubleStruckCapitalF]" -> "Fe"};
elements = {"H","\[DoubleStruckCapitalH]", "C","O","S","\[DoubleStruckCapitalS]","N","F","P","\[DoubleStruckCapitalN]","\[DoubleStruckCapitalM]","\[DoubleStruckCapitalF]","\[DoubleStruckCapitalC]","e"};

photons = {"CRPHOT","CRP","PHOTON"};
carbons = {"C", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", 
  "\[DoubleStruckCapitalX]", "\[DoubleStruckCapitalW]"};
(*hydrogens={"H","H2","H3","H4","H5","H6","H7","D","D2","D3"};*)
hydrogens = {"H","H2","H3","H4","H5","H6","H7"};
oxygens = {"O","O2"};
nitrogens = {"N","N2"};
sulphurs = {"S","S2"};

chemicalContent = {RotateLeft[hydrogens],"\[DoubleStruckCapitalH]", RotateLeft[carbons],RotateLeft[oxygens],RotateLeft[sulphurs],"\[DoubleStruckCapitalS]",RotateLeft[nitrogens],"F","P","\[DoubleStruckCapitalN]","\[DoubleStruckCapitalM]","\[DoubleStruckCapitalF]","\[DoubleStruckCapitalC]","e"};

ruleH = {"H2"->"H1D1","H3"->"H2D1","H4"->"H3D1","H5"->"H4D1","H6"->"H5D1","H7"->"H6D1","H1"->"D1"};
ruleHmulti = {"H2"->"H1D1","H3"->"H2D1","H4"->"H3D1","H5"->"H4D1","H6"->"H5D1","H7"->"H6D1","H1"->"D1","H1D1"->"D2","H2D1"->"H1D2","H3D1"->"H2D2","H4D1"->"H3D2","H5D1"->"H4D2","H6D1"->"H5D2","H1D2"->"D3","H2D2"->"H1D3","H3D2"->"H2D3","H4D2"->"H3D3","H5D2"->"H4D3"};
ruleC = {"C2"->"C1X1","C3"->"C2X1","C4"->"C3X1","C5"->"C4X1","C6"->"C5X1","C7"->"C6X1","C8"->"C7X1","C9"->"C8X1","\[DoubleStruckCapitalX]1"->"C9X1","\[DoubleStruckCapitalW]1" -> "\[DoubleStruckCapitalX]X1","C1"->"X1"}; (*X=^13C*)
ruleO = {"O2"->"O1Y1","O1"->"Y1"}; (*Y=^18O*)

enumerateRule = {x_String/;!StringMatchQ[x,__~~NumberString~~EndOfString]:>x~~"1",x_String/;StringMatchQ[x,__~~NumberString~~EndOfString]:>x};(*
{x_String/;!StringMatchQ[x,__~~NumberString~~EndOfString]:>x~~"1",x_String/;StringMatchQ[x,__~~NumberString~~EndOfString]:>x};*)
denumerateRule = {x_String/;StringMatchQ[x,__~~"1"~~EndOfString]:>StringDrop[x,-1]};
{x_String/;StringMatchQ[x,__~~"1"~~EndOfString]:>StringDrop[x,-1]};

insertOne[x_String] :=
    StringJoin@@(Evaluate[StringCases[x,RegularExpression["(\[DoubleStruckCapitalH]|H|C|\[DoubleStruckCapitalX]|\[DoubleStruckCapitalW]|O|F|S|\[DoubleStruckCapitalS]|N|P|\[DoubleStruckCapitalN]|\[DoubleStruckCapitalM]|\[DoubleStruckCapitalF]|\[DoubleStruckCapitalC]|X|Y|e)(\\d)?"]]]/.enumerateRule);
deuterate[x_String] :=
    StringReplace[StringReplaceList[insertOne[x],ruleH],"1"->""];
carbonizeSingle[x_String] :=
    StringReplace[StringReplace[StringReplaceList[insertOne[x],ruleC],"1"->""],"X"->"13C"];
oxygenizeSingle[x_String] :=
    StringReplace[StringReplace[StringReplaceList[insertOne[x],ruleO],"1"->""],"Y"->"18O"];
carbonizeChain[x_String] :=
    StringReplace[StringReplaceList[insertOne[x],ruleC],"1"->""];

cleanUpRule[x_String] :=
    StringReplace[x,{"X"->"13C","Y"->"18O"}]
cleanUpRule[x_?NumberQ] :=
    x
SetAttributes[cleanUpRule,Listable];

isIon[x_String] :=
    StringMatchQ[x,__~~RegularExpression["[+-]"]~~EndOfString]
charge[x_String] :=
    If[ isIon[x],
        StringTake[x,-1],
        ""
    ]
fromJ[s_String] /; StringTake[s, 1] == "J" := StringDrop[s, 1]
fromJ[s_String] := s
toJ[""] := ""
toJ[a_String] /; StringTake[a, 1] != "J" := "J" <> a
toJ[a_String] := a
onGrain[s_String] := StringTake[s, 1] == "J"
    

chargeList[x_List] :=
    Thread[charge[x]]

insertOne2[x_String] :=
    StringJoin@@(Evaluate[StringCases[StringReplace[x,{"13C"->"X","18O"->"Y"}],RegularExpression["(He|D|H|C|O|S|Si|N|P|Na|Mg|Fe|Cl|X|Y|e)(\\d)?"]]]/.enumerateRule)
ruleC2 = {"C1X1"->"X2","C2X1"->"C1X2","C3X1"->"C2X2","C4X1"->"C3X2","C5X1"->"C4X2","C6X1"->"C5X2","C7X"->"C6X2","C8X1"->"C7X2","C2"->"C1X1","C3"->"C2X1","C4"->"C3X1","C5"->"C4X1","C6"->"C5X1","C7"->"C6X1","C8"->"C7X1","C9"->"C8X1","C1"->"X1"}; (*X=^13C*)

carbonize2[x_String] :=
   If[onGrain[x], toJ[#], #] & /@ StringReplace[StringReplaceList[insertOne2[x],ruleC2],"1"->""]
SetAttributes[carbonize2,Listable]
deuteratePlus[y_] :=
   If[onGrain[y], toJ[#], #] & /@ StringReplace[deuterate[y],x_~~EndOfString:>x<>charge[y]]
oxygenizePlusSingle[y_] :=
   If[onGrain[y], toJ[#], #] & /@ StringReplace[oxygenizeSingle[y],x_~~EndOfString:>x<>charge[y]]
carbonizePlusSingle[y_] :=
   If[onGrain[y], toJ[#], #] & /@ StringReplace[carbonizeSingle[y],x_~~EndOfString:>x<>charge[y]]

carbonizePlusChain[y_] :=
   If[onGrain[y], toJ[#], #] & /@ StringReplace[carbonizeChain[y],x_~~EndOfString:>x<>charge[y]]

insert13CChain[x_List] :=
    Flatten[Table[Reverse[Union[Flatten[Distribute[#,List]&/@(Flatten[Union[Table[{#/.rule[carbonizePlusChain][i]}&[x[[j]]],{i,0,12}]],1]),1]]],{j,Length[x]}],1];
insert13CSingle[x_List] :=
    Flatten[Table[Reverse[Union[Flatten[Distribute[#,List]&/@(Flatten[Union[Table[{#/.rule[carbonizePlusSingle][i]}&[x[[j]]],{i,0,12}]],1]),1]]],{j,Length[x]}],1];
insert18O[x_List] :=
    Flatten[Table[Reverse[Union[Flatten[Distribute[#,List]&/@(Flatten[Union[Table[{#/.rule[oxygenizePlusSingle][i]}&[x[[j]]],{i,0,12}]],1]),1]]],{j,Length[x]}],1];
in13C18O[x_List] :=
    insert13CChain[x]//insert18O//cleanUpRule;
in13C[x_List] :=
    insert13CSingle[x]//cleanUpRule;
in18O[x_List] :=
    insert18O[x]//cleanUpRule;

Clear[rule];
rule[fkt_][0] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}
rule[fkt_][1] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,fkt[a],b,c,fkt[d],e,f,g,h,i,j,k}/;(fkt[a]!={}&&fkt[d]!={}&&!MemberQ[photons,a]&&!MemberQ[photons,d]);
rule[fkt_][2] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,fkt[a],b,c,d,fkt[e],f,g,h,i,j,k}/;fkt[a]!={}&&fkt[e]!={}&&!MemberQ[photons,a]&&!MemberQ[photons,e]&&d=!=e;
rule[fkt_][3] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,fkt[a],b,c,d,e,fkt[f],g,h,i,j,k}/;fkt[a]!={}&&fkt[f]!={}&&!MemberQ[photons,a]&&!MemberQ[photons,f]&&e=!=f;
rule[fkt_][4] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,fkt[a],b,c,d,e,f,fkt[g],h,i,j,k}/;fkt[a]!={}&&fkt[g]!={}&&!MemberQ[photons,a]&&!MemberQ[photons,g];
rule[fkt_][5] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,fkt[b],c,fkt[d],e,f,g,h,i,j,k}/;fkt[b]!={}&&fkt[d]!={}&&!MemberQ[photons,b]&&!MemberQ[photons,d]&&a=!=b;
rule[fkt_][6] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,fkt[b],c,d,fkt[e],f,g,h,i,j,k}/;fkt[b]!={}&&fkt[e]!={}&&!MemberQ[photons,b]&&!MemberQ[photons,e]&&d=!=e&&a=!=b;
rule[fkt_][7] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,fkt[b],c,d,e,fkt[f],g,h,i,j,k}/;fkt[b]!={}&&fkt[f]!={}&&!MemberQ[photons,b]&&!MemberQ[photons,f]&&e=!=f&&a=!=b;
rule[fkt_][8] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,fkt[b],c,d,e,f,fkt[g],h,i,j,k}/;fkt[b]!={}&&fkt[g]!={}&&!MemberQ[photons,b]&&!MemberQ[photons,g]&&a=!=b;
rule[fkt_][9] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,fkt[c],fkt[d],e,f,g,h,i,j,k}/;fkt[c]!={}&&fkt[d]!={}&&!MemberQ[photons,c]&&!MemberQ[photons,d];
rule[fkt_][10] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,fkt[c],d,fkt[e],f,g,h,i,j,k}/;fkt[c]!={}&&fkt[e]!={}&&!MemberQ[photons,c]&&!MemberQ[photons,e]&&c=!=b;
rule[fkt_][11] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,fkt[c],d,e,fkt[f],g,h,i,j,k}/;fkt[c]!={}&&fkt[f]!={}&&!MemberQ[photons,c]&&!MemberQ[photons,f]&&c=!=b;
rule[fkt_][12] :=
    {n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,fkt[c],d,e,f,fkt[g],h,i,j,k}/;fkt[c]!={}&&fkt[g]!={}&&!MemberQ[photons,c]&&!MemberQ[photons,g]&&c=!=b;


cleanAll :=
    Module[ {data},
        log = {};
        data = DeleteCases[Table[If[ showR@i!={},(* to catch missing reactions *)
                                     Which[
                                     showR[i][[1,2]]=="NN",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpNN@showR@i),
                                     showR[i][[1,2]]=="MN",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpMN@showR@i),                      
                                     showR[i][[1,2]]=="IN",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpIN@showR@i),
                                     showR[i][[1,2]]=="RA",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpRA@showR@i),
                                     showR[i][[1,2]]=="AD",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpAD@showR@i),
                                     showR[i][[1,2]]=="DR",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpDRV2@showR@i),
                                     showR[i][[1,2]]=="CE",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpCE@showR@i),
                                     showR[i][[1,2]]=="CL",log = {ToString@showR[i][[1,1;;2]],log};
                                                           Sequence@@(cleanUpCL@showR@i),
                                     showR[i][[1,2]]=="PH"||showR[i][[1,2]]=="CR"||showR[i][[1,2]]=="CP",log = {ToString@showR[i][[1,1;;2]],log};
                                                                                                         Sequence@@(cleanUpPhoto@showR@i),
                                     True,Print["Unknown reaction "<>ToString@showR[i][[1,1;;2]]];
                                          log = {"Unknown reaction "<>ToString@showR[i][[1,1;;2]],log};
                                          Sequence@@(showR@i)
                                     ],
                                     Print["Missing reaction "<>ToString@i];
                                     log = {"Missing reaction "<>ToString@i,log};
                                 ],{i,1,4599}],Null];
        log = Reverse[Flatten[log]];
        data
    ];

padString[instr_String, i_Integer] := Module[{len},
   len = StringLength@instr;
   If[len <= i,
    StringJoin@PadRight[Characters@instr, i, " "], 
    Print["Warning! Input String is too large."]]];

fortranOut[{n_,t_,a_String,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}] :=
    Module[ {},(* Mitglieder der liste doppelteR auskommentarisieren, das sind die Reaktionen, die mehrere Reaktioneraten fuer verschiedene Temperaturen haben. *)
        If[ MemberQ[doppelteR[[All,1]],n],
            "*",
            " "
        ]<>ToString@PaddedForm[n,5]<>" "<>padString[a,15]<>" "<>padString[b,15]<>" "<>padString[d,15]<>" "<>padString[e,15]<>" "<>padString[f,10]<>" "<>padString[g,9](* nur padding 9 weil die nachfolgende konvertierung einen String an den Anfang setzt *)<>ToString@Rest@Eformat[h,{9,2}]<>" "<>printF2[i,5,2]<>" "<>printF2[j,8,1]
    ]

fortranOut2[{n_,t_,a_String,b_,c_,d_,e_,f_,g_,h_,i_,j_,k_,l_,m_,o__}] :=
    Module[ {},(* Mitglieder der liste doppelteR auskommentarisieren, das sind die Reaktionen, die mehrere Reaktioneraten fur verschiedene Temperaturen haben. *)
        If[ MemberQ[doppelteR[[All,1]],n],
            "*",
            " "
        ]<>ToString@PaddedForm[n,5]<>" "<>padString[a,15]<>" "<>padString[b,15]<>" "<>padString[d,15]<>" "<>padString[e,15]<>" "<>padString[f,10]<>" "<>padString[g,9](* nur padding 9 weil die nachfolgende konvertierung einen String an den Anfang setzt *)<>ToString@Rest@Eformat[h,{9,2}]<>" "<>printF2[i,5,2]<>" "<>printF2[j,8,1]<>"   "<>ToString@l<>" "<>ToString@m
    ]

printF2[x_,m_Integer,n_Integer] :=
    (c = 10^n;
     l = m-2;
     x1 = N[x];
     Which[Head[x1]===Real||Head[x1]===Integer,r = N[Round[c x1]/c];
                                               ToString[NumberForm[r,{l,n},NumberPadding->{" ","0"},ExponentFunction->(Null&)]],Head[x1]===Complex,re = N[Round[c x1//Re]/c];
                                                                                                                                                   im = N[Round[c x1//Im]/c];
                                                                                                                                                   ToString[NumberForm[re,{l,n},NumberPadding->{" ","0"},ExponentFunction->(Null&)]]<>ToString[NumberForm[im,{l,n},NumberPadding->{" ","0"},ExponentFunction->(Null&)]]])


Eformat::notallowed = "This function will not accept `1` 
as input, it only works on Integer or Real numbers";
Eformat::toomany = "Too many digits to right of decimal Point 
requested.";

Eformat[0,{tot_Integer,ndigits_Integer}] :=
    Eformat[0.,{tot,ndigits}];
Eformat[x_Rational,{tot_Integer,ndigits_Integer}] :=
    Eformat[N[x],{tot,ndigits}];
Eformat[x_Integer,{tot_Integer,ndigits_Integer}] :=
    Eformat[N[x],{tot,ndigits}];
Eformat[x_Real,{tot_Integer,ndigits_Integer}] :=
    Module[ {exp,man,junk},
        (If[ (tot-ndigits)>=7,
             (If[ Abs[x]<(*$MachineEpsilon*)0,
                  (SequenceForm[" ",PaddedForm[0.,{tot,ndigits+4},NumberPadding->{"","0"}]]),
                  (junk = MantissaExponent[x,10];
                   {man,exp} = {PaddedForm[If[ junk[[1]]==1.,
                                               junk[[1]],
                                               junk[[1]]*10.
                                           ],{tot,ndigits},NumberPadding->{"","0"}],If[ junk[[1]]==1.,
                                                                                        junk[[2]],
                                                                                        junk[[2]]-1
                                                                                    ]};
                   If[ Abs[exp]<=99,
                       (If[ Abs[exp]<10,
                            (SequenceForm[If[ Sign[x]==-1,
                                              "",
                                              " "
                                          ],man,"E",If[ exp>=0,
                                                        "+",
                                                        "-"
                                                    ],PaddedForm[Abs[exp],1,NumberPadding->{"0",""}]]),
                            (SequenceForm[If[ Sign[x]==-1,
                                              "",
                                              " "
                                          ],man,"E",If[ exp>=0,
                                                        "+",
                                                        "-"
                                                    ],PaddedForm[Abs[exp],1,NumberPadding->{"",""}]])
                        ]),
                       SequenceForm[If[ Sign[x]==-1,
                                        "",
                                        " "
                                    ],man,"E","+00"]
                                    (* changed "***" to "+00" *)
                   ])
              ]),
             Message[Eformat::toomany]
         ])
    ];
Eformat[x_,{tot_Integer,ndigits_Integer}] :=
    Eformat[N[x],{tot,ndigits}];

(* fracOut macht fuer die Fraktionierungsreaktionen was fortranOut fuer die restliche Datenbank macht. *)
fracOut[{n_,a_,b_,d_,e_,h_,i_,j_,k__}] :=
    Module[ {},
(* Mitglieder der liste doppelteR auskommentarisieren, das sind die Reaktionen, die mehrere Reaktioneraten fuer verschiedene Temperaturen haben. *)
        If[ MemberQ[doppelteR[[All,1]],n],
            "*",
            " "
        ]<>ToString@PaddedForm[n,5]<>" "<>padString[a,15]<>" "<>padString[b,15]<>" "<>padString[d,15]<>" "<>padString[e,15]<>" "<>padString["",10]<>" "<>padString["",9](* nur padding 9 weil die nachfolgende konvertierung einen String an den Anfang setzt *)<>ToString@Rest@Eformat[h,{9,2}]<>" "<>printF2[i,5,2]<>" "<>printF2[j,8,1]
    ];


(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpPhoto[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&refline[[7]]==="e-"&&neutral@refline[[6]]==neutral@refline[[3]],(*reine Photoionization*)
        If[ debug,
            Print["Photo: pure ionization"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;a<>"+"===d||a<>"-"===d];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&refline[[7]]==="e-"&&refline[[8]]===""&&neutral@refline[[6]]!=neutral@refline[[3]],(*reine Photoionization*)
        If[ debug,
            Print["Photo: restructuring ionization"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,c,d,e,f,g}&&sticky18O@{a,b,c,d,e,f,g}&&If[ isCHnbound@a==isCHnbound@d&&isCObound@a==isCObound@d&&countCOgroups@a==countCOgroups@d==1,
                                                                                                                                                            isCHnisotop@a==isCHnisotop@d&&isCOisotop@a==isCOisotop@d,
                                                                                                                                                            True
                                                                                                                                                        ]&&If[ isCNbound@a==isCNbound@d&&isCHnbound@a==isCHnbound@d,
                                                                                                                                                               isCHnisotop@a==isCHnisotop@d&&isCNisotop@a==isCNisotop@d,
                                                                                                                                                               True
                                                                                                                                                           ]&&If[ isCNbound@a==isCNbound@d&&isCHnbound@a!=isCHnbound@d,
                                                                                                                                                                  isCNisotop@a==isCNisotop@d,
                                                                                                                                                                  True
                                                                                                                                                              ]];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&Apply[Or,refline[[3]]===#&/@StringJoin/@Permutations[{refline[[6]],refline[[7]],refline[[8]]}]],
        If[ debug,
            Print["Photo: pure dissociation into 3 parts"]
        ];
        (*reine Photoionization in 3 Teile*)
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@(a===#&/@StringJoin/@Permutations[{d,e,f}])];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&(refline[[3]]===refline[[6]]<>refline[[7]]||refline[[3]]===refline[[7]]<>refline[[6]]),(*reine Photoionization*)
        If[ debug,
            Print["Photo: pure ionization into 2 parts"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;d<>e===a||e<>d===a];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&isCHnbound@refline[[3]]&&isCObound@refline[[3]]&&countCatoms@refline[[3]]>=(countCOgroups@refline[[3]]+countCHngroups@refline[[3]]),(*reine Photoionization*)
        If[ debug,
            Print["Photo: dissociation into CO and CHn"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&If[ isCObound@d&&isCHnbound@e,
                                                                                                                                 isCOisotop@d==isCOisotop@a&&isCHnisotop@e==isCHnisotop@a,
                                                                                                                                 True
                                                                                                                             ]&&If[ isCObound@e&&isCHnbound@d,
                                                                                                                                    isCOisotop@e==isCOisotop@a&&isCHnisotop@d==isCHnisotop@a,
                                                                                                                                    True
                                                                                                                                ]];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&countCHngroups@refline[[3]]==countCHngroups@refline[[6]]==1,(*reine Photoionization*)
        If[ debug,
            Print["Photo: dissociation, keeping CHn at a and d"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&isCHnisotop@a==isCHnisotop@d];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&countCHngroups@refline[[3]]==countCHngroups@refline[[7]]==1,(*reine Photoionization*)
        If[ debug,
            Print["Photo: dissociation, keeping CHn at a and e"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&isCHnisotop@a==isCHnisotop@e];
        Prepend[rest,refline],
        (refline[[2]]==="PH"||refline[[2]]==="CP"||refline[[2]]==="CR")&&MemberQ[photons,refline[[4]]]&&isCNbound@refline[[3]]==isCNbound@refline[[7]]==True&&isCNbound@refline[[6]]==False,(*reine Photoionization*)
        If[ debug,
            Print["Photo: dissociation, keeping CN at a and e"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&isCNisotop@a==isCNisotop@e];
        Prepend[rest,refline],
        True,If[ debug,
                 Print["Photo: No match"]
             ];
             list]
    ];

(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpRA[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[
        refline[[2]]==="RA"&&isIon[refline[[6]]]&&Or@@StringMatchQ[Union[#<>"+"&/@StringJoin/@Permutations[(If[ isIon[#],
                                                                                                                StringDrop[#,-1],
                                                                                                                #
                                                                                                            ])&/@{refline[[3]],refline[[4]],refline[[5]]}]],refline[[6]]],
        If[ debug,
            Print["RA: Case 1"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@(d===#&/@Union[#<>"+"&/@StringJoin/@Permutations[(If[ isIon[#],
                                                                                                                                                            StringDrop[#,-1],
                                                                                                                                                            #
                                                                                                                                                        ])&/@{a,b,c}]])];
        Prepend[rest,refline],
        refline[[2]]==="RA"&&isIon[refline[[6]]]&&isCObound@refline[[4]]&&isCObound@refline[[6]],
        (* CO in b und d *)
        If[ debug,
            Print["RA: keep CO binding"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;isCOisotop@d==isCOisotop@b];
        Prepend[rest,refline],
        True,If[ debug,
                 Print["RA: No match"]
             ];
             list]
    ]

(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpAD[list_List] :=
    Module[ {rest,refline},
        refline = list[[1]];
        rest = Rest[list];
        Which[
        refline[[2]]==="AD"&&refline[[7]]==="e-"&&Or@@StringMatchQ[Union[StringJoin/@Permutations[(If[ isIon[#],
                                                                                                       StringDrop[#,-1],
                                                                                                       #
                                                                                                   ])&/@{refline[[3]],refline[[4]],refline[[5]]}]],refline[[6]]],
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@(d===#&/@Union[StringJoin/@Permutations[(If[ isIon[#],
                                                                                                                                                   StringDrop[#,-1],
                                                                                                                                                   #
                                                                                                                                               ])&/@{a,b,c}]])];
        Prepend[rest,refline],
        True,list]
    ]

(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpDRV2[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[(* einfaches zusammensetzen aus allen 3 Produkten*)
        (* 2 CO bindungen rechts wie links, allerdings nicht gleichzeitig in d und e*)
        refline[[2]]==="DR"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]==2&&Nand@@isCObound@refline[[6;;7]],
        If[ debug,
            Print["DR:Case Subscript[1.5, 2CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        If[ countCOgroups@d==2&&countCHngroups@d==0&&countCHngroups@e==1,
            If[ Implies[isCOisotop@d,isCOisotop@a&&!isCHnisotop@a],
                True,
                False
            ]&&If[ Implies[isCHnisotop@e,isCHnisotop@a],
                   True,
                   False
               ],
            (Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})
        ](* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* Maximal 1 CO Bindung rechts bzw. links, und nicht mehr als 2 CHn Gruppen *)
        refline[[2]]==="DR"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]<3&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]<=1,
        If[ debug,
            Print["DR:Case 1.5"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ MatchQ[\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,0,_,-1,_,_,_,_}],
                                                                                                      is13Cisotop@{a,b}==is13Cisotop@{d,e},
                                                                                                      True
                                                                                                  ]&&
        If[ MatchQ[\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,0,_,1,_,_,_,_}],
            is13Cisotop@{a,b}==is13Cisotop@{e,d},
            True
        ]&&
        If[ \[CapitalDelta]AX@refline[[{3,4,6,7}]]=={0,0,1,0,0,0,0,0,0},
            If[ Implies[is13Cisotop@a,is13Cisotop@d],
                True,
                False
            ],
            True
        ]&&
        If[ !MatchQ[Abs[\[CapitalDelta]AD@refline[[{3,4,6,7}]]],{_,_,_,_,1,_,_,_,_}],
            (Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f}),
            True
        ](* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&Or@@(StringDrop[refline[[3]],-1]===#&/@StringJoin/@Permutations[{refline[[6]],refline[[7]],refline[[8]]}]),
        If[ debug,
            Print["Case 1"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@(StringDrop[a,-1]===#&/@StringJoin/@Permutations[{d,e,f}])];
        Prepend[rest,refline],
        (* einfaches zusammensetzen aus 2 Produkten*)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&(StringDrop[refline[[3]],-1]===refline[[6]]<>refline[[7]]||StringDrop[refline[[3]],-1]===refline[[7]]<>refline[[6]]),
        If[ debug,
            Print["Case 2"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@(StringDrop[a,-1]===#&/@StringJoin/@Permutations[{d,e,f}])];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[6]]]>=4&&StringLength[refline[[3]]]>=4&&StringTake[refline[[6]],4]===StringTake[refline[[3]],4],
        If[ debug,
            Print["Case 3"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[d,4]===StringTake[a,4]];
        Prepend[rest,refline],
        (*refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[6]]]>=3&&StringTake[refline[[6]],3]===StringTake[refline[[3]],3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Anfang bleiben erhalten *)
        rest=Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[d,3]===StringTake[a,3]];
        Prepend[rest,refline],*)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[6]]]>=3&&StringLength[refline[[3]]]>=3&&StringTake[refline[[6]],-3]===StringTake[StringDrop[refline[[3]],-1 ],-3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["Case 4"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[d,-3]===StringTake[StringDrop[a,-1],-3]];
        Prepend[rest,refline],(* Unklar ob -3 ausreichend ist. TESTEN!!!*)
        (* Produkt 1 oder 2 stimmen mit Anfang von Edukt 1 ueberein*)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&Or@@{(StringMatchQ[StringDrop[refline[[3]],-1],refline[[7]]~~__]&&!StringMatchQ[StringTake[refline[[3]],{StringLength[refline[[7]]]+1}],NumberString]),(StringMatchQ[StringDrop[refline[[3]],-1],refline[[6]]~~__]&&!StringMatchQ[StringTake[refline[[3]],{StringLength[refline[[6]]]+1}],NumberString])},
        If[ debug,
            Print["Case 5"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@{StringMatchQ[StringDrop[a,-1],e~~__],StringMatchQ[StringDrop[a,-1],d~~__]}];
        Prepend[rest,refline],(* CH3 -> CH2 aber complex bleibt erhalten *)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[3]]]>3&&StringTake[refline[[3]],3]=="CH3"&&(If[ StringLength[refline[[6]]]>2,
                                                                                                                                             StringTake[StringReplacePart[refline[[3]],"2",{3,3}],Min[4,StringLength[refline[[6]]]]]===StringTake[refline[[6]],Min[4,StringLength[refline[[6]]]]],
                                                                                                                                             False
                                                                                                                                         ]||If[ StringLength[refline[[7]]]>2,
                                                                                                                                                StringTake[StringReplacePart[refline[[3]],"2",{3,3}],Min[4,StringLength[refline[[7]]]]]===StringTake[refline[[7]],Min[4,StringLength[refline[[7]]]]],
                                                                                                                                                False
                                                                                                                                            ]||If[ StringLength[refline[[8]]]>2,
                                                                                                                                                   StringTake[StringReplacePart[refline[[3]],"2",{3,3}],Min[4,StringLength[refline[[8]]]]]===StringTake[refline[[8]],Min[4,StringLength[refline[[8]]]]],
                                                                                                                                                   False
                                                                                                                                               ]),
        If[ debug,
            Print["Case 6"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[a,"CH3"->"CH2"],Min[5,StringLength[d]]]===StringTake[d,Min[5,StringLength[d]]]||StringTake[StringReplace[a,"CH3"->"CH2"],Min[5,StringLength[e]]]===StringTake[e,Min[5,StringLength[e]]]||If[ f=!="",
                                                                                                                                                                                                                                                                                                                         StringTake[StringReplace[a,"CH3"->"CH2"],Min[5,StringLength[f]]]===StringTake[f,Min[5,StringLength[f]]],
                                                                                                                                                                                                                                                                                                                         False
                                                                                                                                                                                                                                                                                                                     ])];
        Prepend[rest,refline],
        (* CH3 -> CH4 aber complex bleibt erhalten *)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[3]]]>3&&StringTake[refline[[3]],3]=="CH3"&&(If[ StringLength[refline[[6]]]>2,
                                                                                                                                             StringTake[StringReplacePart[refline[[3]],"4",{3,3}],Min[4,StringLength[refline[[6]]]]]===StringTake[refline[[6]],Min[4,StringLength[refline[[6]]]]],
                                                                                                                                             False
                                                                                                                                         ]||If[ StringLength[refline[[7]]]>2,
                                                                                                                                                StringTake[StringReplacePart[refline[[3]],"4",{3,3}],Min[4,StringLength[refline[[7]]]]]===StringTake[refline[[7]],Min[4,StringLength[refline[[7]]]]],
                                                                                                                                                False
                                                                                                                                            ]||If[ StringLength[refline[[8]]]>2,
                                                                                                                                                   StringTake[StringReplacePart[refline[[3]],"4",{3,3}],Min[4,StringLength[refline[[8]]]]]===StringTake[refline[[8]],Min[4,StringLength[refline[[8]]]]],
                                                                                                                                                   False
                                                                                                                                               ]),
        If[ debug,
            Print["Case 7"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[a,"CH3"->"CH4"],Min[4,StringLength[d]]]===StringTake[d,Min[4,StringLength[d]]]||StringTake[StringReplace[a,"CH3"->"CH4"],Min[4,StringLength[e]]]===StringTake[e,Min[4,StringLength[e]]]||If[ f=!="",
                                                                                                                                                                                                                                                                                                                         StringTake[StringReplace[a,"CH3"->"CH4"],Min[4,StringLength[f]]]===StringTake[f,Min[4,StringLength[f]]],
                                                                                                                                                                                                                                                                                                                         False
                                                                                                                                                                                                                                                                                                                     ])];
        Prepend[rest,refline],
        (* CH2 -> CH aber complex bleibt erhalten *)
        refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[3]]]>3&&StringTake[refline[[3]],3]=="CH2"&&(If[ StringLength[refline[[6]]]>0,
                                                                                                                                             StringTake[StringReplacePart[refline[[3]],"",{3,3}],Min[4,StringLength[refline[[6]]]]]===StringTake[refline[[6]],Min[4,StringLength[refline[[6]]]]],
                                                                                                                                             False
                                                                                                                                         ]||If[ StringLength[refline[[7]]]>0,
                                                                                                                                                StringTake[StringReplacePart[refline[[3]],"",{3,3}],Min[4,StringLength[refline[[7]]]]]===StringTake[refline[[7]],Min[4,StringLength[refline[[7]]]]],
                                                                                                                                                False
                                                                                                                                            ]||If[ StringLength[refline[[8]]]>0,
                                                                                                                                                   StringTake[StringReplacePart[refline[[3]],"",{3,3}],Min[4,StringLength[refline[[8]]]]]===StringTake[refline[[8]],Min[4,StringLength[refline[[8]]]]],
                                                                                                                                                   False
                                                                                                                                               ]),
        If[ debug,
            Print["Case 8"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[a,"CH2"->"CH"],Min[5,StringLength[d]]]===StringTake[d,Min[5,StringLength[d]]]||StringTake[StringReplace[a,"CH2"->"CH"],Min[5,StringLength[e]]]===StringTake[e,Min[5,StringLength[e]]]||If[ f=!="",
                                                                                                                                                                                                                                                                                                                       StringTake[StringReplace[a,"CH2"->"CH"],Min[5,StringLength[f]]]===StringTake[f,Min[5,StringLength[f]]],
                                                                                                                                                                                                                                                                                                                       False
                                                                                                                                                                                                                                                                                                                   ])];
        Prepend[rest,refline],
        True,If[ debug,
                 Print["DR:No match"]
             ];
             list]
    ]


(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpCE[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[(* CE: charge exchange => isotopomere links und rechts 
        duerfen den platz nicht tauschen*)
        refline[[2]]==="CE"&&refline[[8]]==""&&(neutral@refline[[3]]==neutral@refline[[7]]&&neutral@refline[[4]]==neutral@refline[[6]])||(neutral@refline[[3]]==neutral@refline[[6]]&&neutral@refline[[4]]==neutral@refline[[7]]),
        If[ debug,
            Print["CE:Case 1"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        (If[ isIon[a],
             StringDrop[a,-1],
             a
         ]==If[ isIon[d],
                StringDrop[d,-1],
                d
            ]||If[ isIon[a],
                   StringDrop[a,-1],
                   a
               ]==If[ isIon[e],
                      StringDrop[e,-1],
                      e
                  ])&&(If[ isIon[b],
                           StringDrop[b,-1],
                           b
                       ]==If[ isIon[d],
                              StringDrop[d,-1],
                              d
                          ]||If[ isIon[b],
                                 StringDrop[b,-1],
                                 b
                             ]==If[ isIon[e],
                                    StringDrop[e,-1],
                                    e
                                ])
        ];
        Prepend[rest,refline],
        refline[[2]]==="CE"&&refline[[8]]==""&&(countHCNOS@refline[[3]]==countHCNOS@refline[[6]]||countHCNOS@refline[[3]]==countHCNOS@refline[[7]]),
        If[ debug,
            Print["CE:Case 2 - restructuring"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&sticky13C@{a,b,c,d,e,f,g}&&
        If[ countHCNOS@refline[[3]]==countHCNOS@refline[[6]],
            If[ isCObound@b==isCObound@e,
                isCOisotop@b==isCOisotop@e,
                True
            ]&&If[ isCObound@a==isCObound@d,
                   isCOisotop@a==isCOisotop@d,
                   True
               ]&&If[ isCHnbound@b==isCHnbound@e,
                      isCHnisotop@b==isCHnisotop@e,
                      True
                  ]&&If[ isCHnbound@a==isCHnbound@d,
                         isCHnisotop@a==isCHnisotop@d,
                         True
                     ]&&
            is13Cisotop@a==is13Cisotop@d&&is13Cisotop@b==is13Cisotop@e&&is18Oisotop@a==is18Oisotop@d&&is18Oisotop@b==is18Oisotop@e,
            If[ isCObound@b==isCObound@d,
                isCOisotop@b==isCOisotop@d,
                True
            ]&&If[ isCObound@a==isCObound@e,
                   isCOisotop@a==isCOisotop@e,
                   True
               ]&&If[ isCHnbound@b==isCHnbound@d,
                      isCHnisotop@b==isCHnisotop@d,
                      True
                  ]&&If[ isCHnbound@a==isCHnbound@e,
                         isCHnisotop@a==isCHnisotop@e,
                         True
                     ]&&is13Cisotop@a==is13Cisotop@e&&is13Cisotop@b==is13Cisotop@d&&is18Oisotop@a==is18Oisotop@e&&is18Oisotop@b==is18Oisotop@d
        ](*&&
        If[countNatoms@b==countNatoms@e>0&&countCatoms@b==countCatoms@e>0&&isCNbound@e,If[Implies[isCNbound@e,is13Cisotop@b],True,False],True]*)
        ];
        Prepend[rest,refline],
        refline[[2]]==="CE"&&refline[[8]]==""&&(countHCNOS@refline[[3]]!=countHCNOS@refline[[6]]||countHCNOS@refline[[3]]!=countHCNOS@refline[[7]]),
        If[ debug,
            Print["CE:Case 3 - dissociation"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&sticky13C@{a,b,c,d,e,f,g}(*&&
        If[countNatoms@b==countNatoms@e>0&&countCatoms@b==countCatoms@e>0&&isCNbound@e,If[Implies[isCNbound@e,is13Cisotop@b],True,False],True]*)
        ];
        Prepend[rest,refline],
        True,list]
    ]


(* -------------------------------------------------------------------------------------------------------------------------------*)

cleanUpIN[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[(* IN: ion-neutral reactions *)
        (* Spezial Fall # 862 *)
        refline[[2;;9]]=={"IN","H3+","CH3COCH3","","C2H3+","CH4","H2O",""},
        If[ debug,
            Print["IN: Custom Case #862"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        (*If[(!isCOisotop@b&&is13Cisotop@d)||(isCOisotop@b&&!is13Cisotop@d),False,True]*)
        If[ Implies[isCOisotop@b,is13Cisotop@d],
            True,
            False
        ]];(* Implies sichert die einseitige Verknuepfung, also dass der umkehrschluss nicht gelten muss. Wenn 13CO -> C13CH3+ aber nicht umgekehrt! *)
        Prepend[rest,refline],
        (* Spezial Fall # 860 *)
        refline[[2;;9]]=={"IN","H3+","CH3COCH3","","CH3+","C2H5OH","",""},
        If[ debug,
            Print["IN: Custom Case #860"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        (*If[(!isCOisotop@b&&is13Cisotop@d)||(isCOisotop@b&&!is13Cisotop@d),False,True]*)
        If[ Implies[isCOisotop@b,is13Cisotop@e],
            True,
            False
        ]];(* Implies sichert die einseitige Verknuepfung, also dass der umkehrschluss nicht gelten muss. Wenn 13CO -> C13CH3+ aber nicht umgekehrt! *)
        Prepend[rest,refline],
        (* Spezial Fall # 2652 *)
        (*refline[[2;;9]]=={"IN","HCO+","HCOOCH3","","H5C2O2+","CO","",""},
        If[debug,Print["IN: Custom Case #2652"]];
        rest=Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        isCOisotop@a==isCOisotop@e&&isC18Obound@a==isC18Obound@e];
        Prepend[rest,refline],*)
        (* Spezial Fall # 2652, O-Atom in H3O+ und H2O ist das gleiche *)
        (*refline[[2;;9]]=={"IN","H3O+","HCOOCH3","","H5C2O2+","H2O","",""},If[debug,Print["IN: Custom Case #2160"]];
        rest=Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        is18Oisotop@a==is18Oisotop@e];
        Prepend[rest,refline],*)
        (* Spezial Fall # 2755, O-Atom in H3O+ und H2O ist das gleiche *)
        refline[[2;;9]]=={"IN","CH3OH","CH3OH2+","","CH3OCH4+","H2O","",""},
        If[ debug,
            Print["IN: Custom Case #2755"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        is18Oisotop@a==is18Oisotop@d&&is18Oisotop@b==is18Oisotop@e];
        Prepend[rest,refline],
        (* Trivial Case: nicht mehr als 1 C und 1 O atom insgesamt *)
        refline[[2]]==="IN"&&Total@countCatoms@refline[[3;;5]]<=1&&Total@countOatoms@refline[[3;;5]]<=1,
        If[ debug,
            Print["IN:Trivial Case - no action."]
        ];
        Prepend[rest,refline],
        (* Arbitrary Case: \[CapitalDelta]AD = -\[CapitalDelta]AE && keine gemischten Vorzeichen und nur 4 Reaktanden *)
        refline[[2]]==="IN"&&refline[[8]]==""&&!mixedSigns@\[CapitalDelta]AD@refline[[{3,4,6,7}]]&&!mixedSigns@\[CapitalDelta]AE@refline[[{3,4,6,7}]]&&\[CapitalDelta]AD@refline[[{3,4,6,7}]]==-\[CapitalDelta]AE@refline[[{3,4,6,7}]],
        If[ debug,
            Print["IN:Arbitrary Case - no decision possible"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ And@@isCObound@refline[[{3,4,6,7}]]&&!MatchQ[Sign@Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,1,_,_,_,_,_,_}]&&!MatchQ[Sign@Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,_,_,1,_,_,_,_}],
                                                                                                      Sort@MapThread[And,{is13Cisotop@{a,b},isC18Obound@{a,b}}]==Sort@MapThread[And,{is13Cisotop@{d,e},isC18Obound@{d,e}}],
                                                                                                      True
                                                                                                  ]];
        Prepend[rest,refline],
        (* Klarer Protonen/H - Transfer, i.e. \[CapitalDelta]AD|\[CapitalDelta]AE={1,0,0,0,0....} *)
        refline[[2]]==="IN"&&refline[[8]]==""&&Total@countCatoms@refline[[3;;5]]>1&&Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]!=Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]]&&(Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0}||Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0}),
        If[ debug,
            Print["IN: H/Proton-Transfer"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,d,e}&&If[ Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0},
                                                                                                                           If[ isCNbound@b==isCNbound@d,
                                                                                                                               isCNisotop@b==isCNisotop@d&&isleft13CNbound@b==isleft13CNbound@d,
                                                                                                                               True
                                                                                                                           ]&&If[ isCNbound@a==isCNbound@e,
                                                                                                                                  isCNisotop@a==isCNisotop@e&&isleft13CNbound@a==isleft13CNbound@e,
                                                                                                                                  True
                                                                                                                              ]&&If[ isCObound@b==isCObound@d,
                                                                                                                                     isleft18Obound@b==isleft18Obound@d,
                                                                                                                                     True
                                                                                                                                 ]&&If[ isCObound@a==isCObound@e,
                                                                                                                                        isleft18Obound@a==isleft18Obound@e,
                                                                                                                                        True
                                                                                                                                    ]&&If[ isCOCHnbound@b==isCOCHnbound@d==False,
                                                                                                                                           isCOisotop@b==isCOisotop@d,
                                                                                                                                           True
                                                                                                                                       ]&&If[ isCOCHnbound@b!=isCOCHnbound@d==False,
                                                                                                                                              If[ Implies[isCOisotop@d,isCOisotop@b],
                                                                                                                                                  True,
                                                                                                                                                  False
                                                                                                                                              ],
                                                                                                                                              True
                                                                                                                                          ]&&If[ isCOCHnbound@a==isCOCHnbound@e==False,
                                                                                                                                                 isCOisotop@a==isCOisotop@e,
                                                                                                                                                 True
                                                                                                                                             ]&&If[ isCOCHnbound@a!=isCOCHnbound@e==False,
                                                                                                                                                    If[ Implies[isCOisotop@a,isCOisotop@e],
                                                                                                                                                        True,
                                                                                                                                                        False
                                                                                                                                                    ],
                                                                                                                                                    True
                                                                                                                                                ]&&is13Cisotop@b==is13Cisotop@d&&is13Cisotop@a==is13Cisotop@e&&is18Oisotop@b==is18Oisotop@d&&is18Oisotop@a==is18Oisotop@e&&If[ isCHnbound@a==isCHnbound@e&&isCOCHnbound@a==isCOCHnbound@e,
                                                                                                                                                                                                                                                                               isCHnisotop@a==isCHnisotop@e,
                                                                                                                                                                                                                                                                               True
                                                                                                                                                                                                                                                                           ]&&If[ isCHnbound@b==isCHnbound@d&&isCOCHnbound@b==isCOCHnbound@d,
                                                                                                                                                                                                                                                                                  isCHnisotop@b==isCHnisotop@d,
                                                                                                                                                                                                                                                                                  True
                                                                                                                                                                                                                                                                              ],
                                                                                                                           If[ isCNbound@b==isCNbound@e,
                                                                                                                               isCNisotop@b==isCNisotop@e&&isleft13CNbound@b==isleft13CNbound@e,
                                                                                                                               True
                                                                                                                           ]&&If[ isCNbound@a==isCNbound@d,
                                                                                                                                  isCNisotop@a==isCNisotop@d&&isleft13CNbound@a==isleft13CNbound@d,
                                                                                                                                  True
                                                                                                                              ]&&If[ isCObound@b==isCObound@e,
                                                                                                                                     isleft18Obound@b==isleft18Obound@e,
                                                                                                                                     True
                                                                                                                                 ]&&If[ isCObound@a==isCObound@d,
                                                                                                                                        isleft18Obound@a==isleft18Obound@d,
                                                                                                                                        True
                                                                                                                                    ]&&
                                                                                                                           isCOisotop@b==isCOisotop@e&&isCOisotop@a==isCOisotop@d&&is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d&&is18Oisotop@b==is18Oisotop@e&&is18Oisotop@a==is18Oisotop@d&&If[ isCHnbound@a==isCHnbound@d,
                                                                                                                                                                                                                                                                                                               isCHnisotop@a==isCHnisotop@d,
                                                                                                                                                                                                                                                                                                               True
                                                                                                                                                                                                                                                                                                           ]&&If[ isCHnbound@b==isCHnbound@e,
                                                                                                                                                                                                                                                                                                                  isCHnisotop@b==isCHnisotop@e,
                                                                                                                                                                                                                                                                                                                  True
                                                                                                                                                                                                                                                                                                              ]
                                                                                                                       ]];
        Prepend[rest,refline],
        (* CN UND CO Bindung in Edukt und Produkt - Annahme: Bindung bleibt erhalten*)
        refline[[2]]==="IN"&&((isCNbound@refline[[3]]&&isCObound@refline[[3]])||(isCNbound@refline[[4]]&&isCObound@refline[[4]])),
        If[ debug,
            Print["IN:Case 0a"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCNisotop@{a,b,c}&&Xor@@isCNisotop@{d,e,f})||(Or@@isNotCNisotop@{a,b,c}&&Or@@isNotCNisotop@{d,e,f})(*CO und CN in a  bzw. b -> CN bleibt erhalten - allerdings auch von 1b geleistet*)];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&Total[countOatoms/@refline[[3;;5]]]<2&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]]&&Xor@@isCNbound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? Nur wenn die 2 CO Bindungen nicht in einem Molekuel auftreten, dann funktioniert das nicht! *)
        If[ debug,
            Print["IN:Case 0b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))||(Or@@isCNisotop@{a,b,c}&&(Xor@@isCNisotop@{d,e,f}))||(And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f})&&(And@@isNotCNisotop@{d,e,f}))(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten*)];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&Total[countOatoms/@refline[[3;;5]]]>1&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]]&&Xor@@isCNbound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? Nur wenn die 2 CO Bindungen nicht in einem Molekuel auftreten, dann funktioniert das nicht! *)
        If[ debug,
            Print["IN:Case 0Subscript[b, 1]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;((Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))||((Or@@isCNisotop@{a,b,c}&&(Xor@@isCNisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))||((And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f})&&(And@@isNotCNisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten, und 18O bleibt auch an der korrekten Stelle (check #2904)*)];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]],(* SPEZIALFALL: CO und CN Bindung links aber keine klare CN Bindung rechts, Entweder aufgebrochen, oder versteckt in beispielsweise Subscript[C, n]N *)
        If[ debug,
            Print["IN:Case 0Subscript[b, 2]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))||(Or@@isCNisotop@{a,b,c}&&(And@@isNotCOisotop@{d,e,f}))||(And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f}))(* FUnktioniert wahrscheinlich nicht immer*)];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Total[countCatoms/@refline[[3;;5]]]<3&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? *)
        If[ debug,
            Print["IN:Case 0c"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&Xor@@isCOisotop@{d,e,f})||(And@@isNotCOisotop@{a,b,c}&&And@@isNotCOisotop@{d,e,f})(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten*)];
        Prepend[rest,refline],
        (* CN Bindung in Edukt und Produkt - Annahme: Bindung bleibt erhalten*)
        refline[[2]]==="IN"&&Or@@isCNbound@refline[[3;;5]]&&Or@@isCNbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countNatoms@refline[[3;;5]]==1,
        If[ debug,
            Print["IN:Case 1a"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCNisotop@{a,b,c}&&Xor@@isCNisotop@{d,e,f})||(And@@isNotCNisotop@{a,b,c}&&And@@isNotCNisotop@{d,e,f})(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Or@@isCNbound@refline[[4]]&&Or@@isCNbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&countNatoms@refline[[4]]==1,
        If[ debug,
            Print["IN:Case 1b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(isCNisotop@b&&Xor@@isCNisotop@{d,e,f})||(isNotCNisotop@b&&Or@@isNotCNisotop@{d,e,f})(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* alle Molekuele haben CHn Bindungen -> nur Austausch von H+ oder H2+ ,
        a entspricht e, b entspricht d*)
        refline[[2]]==="IN"&&And@@isCHnbound@refline[[{3,4,6,7}]],
        If[ debug,
            Print["IN:Case Subscript[1.5, 4CHn]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ countCHngroups@{a,b}==countCHngroups@{e,d},
                                                                                                      isCHnisotop@a==isCHnisotop@e&&isCHnisotop@b==isCHnisotop@d&&isCOisotop@a==isCOisotop@e&&isCOisotop@b==isCOisotop@d&&isC18Obound@a==isC18Obound@e&&isC18Obound@b==isC18Obound@d,
                                                                                                      If[ Implies[isCHnisotop@a,isCHnisotop@e],
                                                                                                          True,
                                                                                                          False
                                                                                                      ]&&isCOisotop@a==isCOisotop@e&&isCOisotop@b==isCOisotop@d&&isC18Obound@a==isC18Obound@e&&isC18Obound@b==isC18Obound@d
                                                                                                  ]];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* 2 bindungen rechts wie links, allerdings nicht gleichzeitig in d und e*)
        refline[[2]]==="IN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]==2&&Nand@@isCObound@refline[[6;;7]],
        If[ debug,
            Print["IN:Case Subscript[1.5, 2CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})(* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* Maximal 1 CO Bindung rechts bzw. links, und nicht mehr als 2 CHn Gruppen *)
        refline[[2]]==="IN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]<3&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]<=1,
        If[ debug,
            Print["IN:Case 1.5"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ MatchQ[\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,0,_,-1,_,_,_,_}],
                                                                                                      is13Cisotop@{a,b}==is13Cisotop@{d,e},
                                                                                                      True
                                                                                                  ]&&
        If[ MatchQ[\[CapitalDelta]AD@refline[[{3,4,6,7}]],{_,_,0,_,1,_,_,_,_}],
            is13Cisotop@{a,b}==is13Cisotop@{e,d},
            True
        ]&&
        If[ \[CapitalDelta]AX@refline[[{3,4,6,7}]]=={0,0,1,0,0,0,0,0,0},
            If[ Implies[is13Cisotop@a,is13Cisotop@d],
                True,
                False
            ],
            True
        ]&&
        If[ f!="",
            If[ isCObound@b&&isCObound@d,
                isCOisotop@d==isCOisotop@b,
                True
            ],
            True
        ]&&
        If[ !MatchQ[Abs[\[CapitalDelta]AD@refline[[{3,4,6,7}]]],{_,_,_,_,1,_,_,_,_}],
            (Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f}),
            True
        ](* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* geiche Zahl bindungen rechts wie links, und entweder 3 CO Bindungen rechts und links oder ungleich viele CO Bindungen links und rechts*)
        refline[[2]]==="IN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]&&(Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]==3||Total@countCOgroups@refline[[3;;5]]!=Total@countCOgroups@refline[[6;;8]])&&findPartnerA@refline[[{3,4,6,7}]]!=0&&findPartnerB@refline[[{3,4,6,7}]]!=0,
        If[ debug,
            Print["IN:Case Subscript[1.5, 3/!= CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        If[ findPartnerA@refline[[{3,4,6,7}]]==1,
        (* Partner A \[LongLeftRightArrow] D : *)
            Which[\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[1]]!=0,isCHnisotop@a==isCHnisotop@d&&isCOisotop@a==isCOisotop@d&&is18Oisotop@a==is18Oisotop@d&&isleft18Obound@a==isleft18Obound@d&&isleft18Obound@b==isleft18Obound@e&&isCNisotop@a==isCNisotop@d,\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[5]]!=0(* 5=> O*),isCHnisotop@a==isCHnisotop@d&&isCNisotop@a==isCNisotop@d&&isleft18Obound@a==isleft18Obound@d&&isleft18Obound@b==isleft18Obound@e,True,True],(* Partner A \[LongLeftRightArrow] E : *)
            Which[\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[1]]!=0,isCHnisotop@a==isCHnisotop@e&&isCOisotop@a==isCOisotop@e&&is18Oisotop@a==is18Oisotop@e&&isCNisotop@a==isCNisotop@e&&isleft18Obound@a==isleft18Obound@e&&isleft18Obound@b==isleft18Obound@d,\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[5]]!=0(* 5=> O*),isCHnisotop@a==isCHnisotop@e&&isCNisotop@a==isCNisotop@e&&isleft18Obound@a==isleft18Obound@e&&isleft18Obound@b==isleft18Obound@d,True,True]
        ]
        (* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
        (* Maximal 1 CO Bindung rechts 2 links, und nicht mehr als 2 CHn Gruppen, und 4 Produkte, bsp: 1036*)
        refline[[2]]==="IN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;9]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;9]]<3&&Total@countCOgroups@refline[[3;;5]]>Total@countCOgroups@refline[[6;;9]]<=1&&refline[[9]]!="",
        If[ debug,
            Print["IN:Case Subscript[1.5, 4Produkte]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f,g})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f,g})(* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
        Prepend[rest,refline],
        (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus , ungleiche Anzahl CHn Gruppen
        aber bei erhaltener CO Bindung! geht nicht mit bsp: #862*)
        refline[[2]]==="IN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&refline[[8]]===""&&Or@@isCObound@refline[[3;;5]]==Or@@isCObound@refline[[6;;8]]&&Total@countCHngroups@refline[[3;;5]]!=Total@countCHngroups@refline[[6;;8]]&&findPartnerA@refline[[{3,4,6,7}]]!=0,
        If[ debug,
            Print["IN:Case 1.6"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ (\[CapitalDelta]AX@{a,b,d,e})[[3]]==0&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={3,0,0,0,0,0,0,0,0}&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={2,0,0,0,0,0,0,0,0}(* no C-atom transfer *),
                                                                                                      If[ findPartnerA@{a,b,d,e}==1,
                                                                                                          is13Cisotop@{a,b}==is13Cisotop@{d,e},
                                                                                                          is13Cisotop@{a,b}==is13Cisotop@{e,d}
                                                                                                      ],
                                                                                                      True
                                                                                                  ]&&
        If[ (\[CapitalDelta]AX@{a,b,d,e}=={-3,0,0,0,0,0,0,0,0}||\[CapitalDelta]AX@{a,b,d,e}=={-2,0,0,0,0,0,0,0,0})&&countCHngroups@refline[[4]]==2,
            If[ isCHnbound@b==isCHnbound@e,
                If[ Implies[isCHnisotop@e,isCHnisotop@b],
                    True,
                    False
                ],
                True
            ],
            True
        ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[5]]==0(* no O-atom transfer *),
               If[ findPartnerA@{a,b,d,e}==1,
                   is18Oisotop@{a,b}==is18Oisotop@{d,e},
                   is18Oisotop@{a,b}==is18Oisotop@{e,d}
               ],
               True
           ]&&
        If[ findPartnerA@{a,b,d,e}==1&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={3,0,0,0,0,0,0,0,0}&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={2,0,0,0,0,0,0,0,0},
            If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-2,-1}&&isCHnbound@a==isCHnbound@d,
                If[ Implies[isCHnisotop@d,isCHnisotop@a],
                    True,
                    False
                ],
                True
            ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-2,-1}&&isCHnbound@b==isCHnbound@e,
                   If[ Implies[isCHnisotop@e,isCHnisotop@b],
                       True,
                       False
                   ],
                   True
               ]&&
            If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@a==isCHnbound@d,
                If[ Implies[isCHnisotop@d,isCHnisotop@a],
                    True,
                    False
                ],
                True
            ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@b==isCHnbound@e,
                   If[ Implies[isCHnisotop@e,isCHnisotop@b],
                       True,
                       False
                   ],
                   True
               ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@a==isCObound@d,
                      isCOisotop@d==isCOisotop@a,
                      True
                  ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@b==isCObound@e,
                         isCOisotop@b==isCOisotop@e,
                         True
                     ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@a==isCObound@d,
                            isCOisotop@d==isCOisotop@a,
                            True
                        ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@b==isCObound@e,
                               isCOisotop@b==isCOisotop@e,
                               True
                           ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={2,1}&&isCHnbound@a==isCHnbound@d,
                                  isCHnisotop@a==isCHnisotop@d,
                                  True
                              ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={2,1}&&isCHnbound@b==isCHnbound@e,
                                     isCHnisotop@b==isCHnisotop@e,
                                     True
                                 ],
            True
        ]&&If[ findPartnerA@{a,b,d,e}==2&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={3,0,0,0,0,0,0,0,0}&&Abs[\[CapitalDelta]AX@{a,b,d,e}]!={2,0,0,0,0,0,0,0,0},
               If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-2,-1}&&isCHnbound@a==isCHnbound@e,
                   If[ Implies[isCHnisotop@e,isCHnisotop@a],
                       True,
                       False
                   ],
                   True
               ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-2,-1}&&isCHnbound@b==isCHnbound@d,
                      If[ Implies[isCHnisotop@d,isCHnisotop@b],
                          True,
                          False
                      ],
                      True
                  ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={2,1}&&isCHnbound@a==isCHnbound@e,
                         isCHnisotop@a==isCHnisotop@e,
                         True
                     ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={2,1}&&isCHnbound@b==isCHnbound@d,
                            isCHnisotop@b==isCHnisotop@d,
                            True
                        ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@a==isCHnbound@e,
                               If[ Implies[isCHnisotop@e,isCHnisotop@a],
                                   True,
                                   False
                               ],
                               True
                           ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@b==isCHnbound@d,
                                  If[ Implies[isCHnisotop@d,isCHnisotop@b],
                                      True,
                                      False
                                  ],
                                  True
                              ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@a==isCObound@e,
                                     isCOisotop@e==isCOisotop@a,
                                     True
                                 ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@b==isCObound@d,
                                        isCOisotop@b==isCOisotop@d,
                                        True
                                    ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@a==isCObound@e,
                                           isCOisotop@e==isCOisotop@a,
                                           True
                                       ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@b==isCObound@d,
                                              isCOisotop@b==isCOisotop@d,
                                              True
                                          ](*&&If[isCObound@b==isCObound@d,isCOisotop@b==isCOisotop@d,True]&&If[isCObound@a==isCObound@e,isCOisotop@a==isCOisotop@e,True]*),
               True
           ]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)(* SPezialFall: beide Edukte und Produkte stimmen ueberein: #2787*)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=5&&StringLength[neutral@refline[[4]]]>=5&&StringLength[neutral@refline[[7]]]>=5&&StringLength[neutral@refline[[3]]]>=5&&StringTake[neutral@refline[[6]],5]===StringTake[neutral@refline[[4]],5]&&StringTake[neutral@refline[[7]],5]===StringTake[neutral@refline[[3]],5],
        If[ debug,
            Print["IN:Case Subscript[2, 0]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,5]===StringTake[neutral@b,5]&&StringTake[neutral@e,5]===StringTake[neutral@a,5]];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&(StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4]||StringTake[neutral@refline[[6]],-3]===StringTake[neutral@refline[[4]],-3])&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],
        If[ debug,
            Print["IN:Case Subscript[2, CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[neutral@d,4]===StringTake[neutral@b,4]||StringTake[neutral@d,-3]===StringTake[neutral@b,-3])&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4],
        If[ debug,
            Print["IN:Case 2"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,4]===StringTake[neutral@b,4]];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[7]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&(StringTake[neutral@refline[[7]],4]===StringTake[neutral@refline[[4]],4]||StringTake[neutral@refline[[7]],-3]===StringTake[neutral@refline[[4]],-3])&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],
        If[ debug,
            Print["IN:Case 2Subscript[b, CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[neutral@e,4]===StringTake[neutral@b,4]||StringTake[neutral@e,-3]===StringTake[neutral@b,-3])&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[7]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[7]],4]===StringTake[neutral@refline[[4]],4],
        If[ debug,
            Print["IN:Case 2b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,4]===StringTake[neutral@b,4]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
        (* wechsel von ST (_,4) -> 3 *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[3]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[3]],4]&&StringLength[neutral@refline[[7]]]>=2&&StringTake[neutral@refline[[6]],2]=!=StringTake[neutral@refline[[7]],2],
        If[ debug,
            Print["IN:Case 3"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,3]===StringTake[neutral@a,3]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4],
        If[ debug,
            Print["IN:Case 3b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,4]===StringTake[neutral@b,4]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringLength[neutral@refline[[6]]]>=3&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[3]],3]&&StringTake[neutral@refline[[7]],2]=!=StringTake[neutral@refline[[6]],2],
        If[ debug,
            Print["IN:Case 3c - First 3 characters of a==e"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,2]===StringTake[neutral@a,2]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringLength[neutral@refline[[3]]]>=2&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[4]],3]&&StringTake[neutral@refline[[3]],2]=!=StringTake[neutral@refline[[4]],2],
        If[ debug,
            Print["IN:Case 3d - First 3 characters of b==e"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,2]===StringTake[neutral@b,2]];
        Prepend[rest,refline],
        (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), Ladung kennzeichnet das Molekuel - 1 oder 2 H-atome wechseln den Partner *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringLength[neutral@refline[[6]]]>=2&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[3]],3]&&StringTake[neutral@refline[[7]],2]===StringTake[neutral@refline[[6]],2]&&Total@countCatoms@refline[[3;;5]]>1,
        If[ debug,
            Print["IN:Case 3e - First 3 characters of a==e \[And] d==e - charge sticks to molecule"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ isIon@e===isIon@a,
                                                                                                      StringTake[neutral@e,2]===StringTake[neutral@a,2],
                                                                                                      StringTake[neutral@d,2]===StringTake[neutral@a,2]
                                                                                                  ]];
        Prepend[rest,refline],
        (*refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[6]]]>=3&&StringTake[refline[[6]],3]===StringTake[refline[[3]],3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Anfang bleiben erhalten *)
        rest=Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[d,3]===StringTake[a,3]];
        Prepend[rest,refline],*)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[3]]],-3]&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["IN: Case Subscript[4, CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[a],-3]&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
        Prepend[rest,refline],(* Unklar ob -3 ausreichend ist. TESTEN!!!*)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[3]]],-3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["IN: Case 4"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[a],-3]];
        Prepend[rest,refline],(* Unklar ob -3 ausreichend ist. TESTEN!!!*)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[4]]],-3]&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["IN: Case 4Subscript[b, CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[b],-3]&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[4]]],-3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["IN: Case 4b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[b],-3]];
        Prepend[rest,refline],
        (* Produkt 1 oder 2 stimmen mit Anfang von Edukt 1 ueberein*)
        refline[[2]]==="IN"&&Or@@{(StringMatchQ[neutral[refline[[3]]],neutral[refline[[7]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[3]]],{StringLength[neutral[refline[[7]]]]+1}],NumberString]),(StringMatchQ[neutral[refline[[3]]],neutral[refline[[6]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[3]]],{StringLength[neutral[refline[[6]]]]+1}],NumberString])},
        If[ debug,
            Print["IN: Case 5"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@{StringMatchQ[neutral[a],neutral[e]~~__],StringMatchQ[neutral[a],neutral[d]~~__]}];
        Prepend[rest,refline],(* CH3 -> CH2 aber complex bleibt erhalten *)
        (* Produkt 1 oder 2 stimmen mit Anfang von Edukt 1 ueberein*)
        refline[[2]]==="IN"&&Or@@{(StringMatchQ[neutral[refline[[4]]],neutral[refline[[7]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[4]]],{StringLength[neutral[refline[[7]]]]+1}],NumberString]),(StringMatchQ[neutral[refline[[4]]],neutral[refline[[6]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[4]]],{StringLength[neutral[refline[[6]]]]+1}],NumberString])},
        If[ debug,
            Print["IN: Case 5b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@{StringMatchQ[neutral[b],neutral[e]~~__],StringMatchQ[neutral[b],neutral[d]~~__]}];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral[refline[[3]]],3]=="CH3"&&(If[ StringLength[neutral[refline[[6]]]]>2,
                                                                                                                     StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[6]]]]]]===StringTake[neutral[refline[[6]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[6]]]]]],
                                                                                                                     False
                                                                                                                 ]||If[ StringLength[neutral[refline[[7]]]]>2,
                                                                                                                        StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[7]]]]]]===StringTake[neutral[refline[[7]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[7]]]]]],
                                                                                                                        False
                                                                                                                    ]||If[ StringLength[neutral[refline[[8]]]]>2,
                                                                                                                           StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[8]]]]]]===StringTake[neutral[refline[[8]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[8]]]]]],
                                                                                                                           False
                                                                                                                       ]),
        If[ debug,
            Print["IN: Case 6"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[neutral[a],"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@d]]]===StringTake[neutral@d,Min[5,StringLength[neutral@a],StringLength[neutral@d]]]||StringTake[StringReplace[neutral@a,"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@e]]]===StringTake[neutral@e,Min[5,StringLength[neutral@a],StringLength[neutral@e]]]||If[ neutral@f=!="",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          StringTake[StringReplace[neutral@a,"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@f]]]===StringTake[neutral@f,Min[5,StringLength[neutral@a],StringLength[neutral@f]]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          False
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ])];
        Prepend[rest,refline],
        (* CH3 -> CH2 aber complex bleibt erhalten *)
        refline[[2]]==="IN"&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral[refline[[4]]],3]=="CH3"&&(If[ StringLength[neutral[refline[[6]]]]>2,
                                                                                                                     StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[6]]]]]]===StringTake[neutral[refline[[6]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[6]]]]]],
                                                                                                                     False
                                                                                                                 ]||If[ StringLength[neutral[refline[[7]]]]>2,
                                                                                                                        StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[7]]]]]]===StringTake[neutral[refline[[7]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[7]]]]]],
                                                                                                                        False
                                                                                                                    ]||If[ StringLength[neutral[refline[[8]]]]>2,
                                                                                                                           StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[8]]]]]]===StringTake[neutral[refline[[8]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[8]]]]]],
                                                                                                                           False
                                                                                                                       ]),
        If[ debug,
            Print["IN: Case 6b"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[neutral[b],"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@d]]]===StringTake[neutral@d,Min[5,StringLength[neutral@b],StringLength[neutral@d]]]||StringTake[StringReplace[neutral@b,"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@e]]]===StringTake[neutral@e,Min[5,StringLength[neutral@b],StringLength[neutral@e]]]||If[ neutral@f=!="",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          StringTake[StringReplace[neutral@b,"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@f]]]===StringTake[neutral@f,Min[5,StringLength[neutral@b],StringLength[neutral@f]]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          False
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ])];
        Prepend[rest,refline],
        (* keine vorherige Regel trifft zu - nur CO bindungen erhalten *)
        refline[[2]]==="IN"&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
        If[ debug,
            Print["IN: Case Subscript[X, iso-CO]"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(*(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})*)(* hier von cleanUpNN eingefuegt *)If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&MatchQ[Abs@\[CapitalDelta]BX@{a,b,d,e},{_,_,_,_,0,_,_,_,_}],
                                                                                                                                                                                                                                                (Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})&&(* 13C18O bleibt zusammen *)Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}],
                                                                                                                                                                                                                                                True
                                                                                                                                                                                                                                            ]&&If[ f==""&&Sort@countCOgroups@{a,b}!=Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==1&&MatchQ[Abs@\[CapitalDelta]BE@{a,b,d,e},{_,_,_,_,1,_,_,_,_}],
                                                                                                                                                                                                                                                   If[ Implies[is18Oisotop@e,is18Oisotop@b],
                                                                                                                                                                                                                                                       True,
                                                                                                                                                                                                                                                       False
                                                                                                                                                                                                                                                   ]&&If[ Implies[is13Cisotop@e,is13Cisotop@b],
                                                                                                                                                                                                                                                          True,
                                                                                                                                                                                                                                                          False
                                                                                                                                                                                                                                                      ]&&If[ Implies[is13Cisotop@d,is13Cisotop@a],
                                                                                                                                                                                                                                                             True,
                                                                                                                                                                                                                                                             False
                                                                                                                                                                                                                                                         ],
                                                                                                                                                                                                                                                   True
                                                                                                                                                                                                                                               ]&&If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==0&&Abs@\[CapitalDelta]AE@{a,b,d,e}!=Abs@\[CapitalDelta]AD@{a,b,d,e}&&MatchQ[Abs@\[CapitalDelta]AE@{a,b,d,e},{1,0,0,0,0,0,0,0,0}],
                                                                                                                                                                                                                                                      is18Oisotop@a==is18Oisotop@e&&is18Oisotop@b==is18Oisotop@d&&If[ Implies[is13Cisotop@e,is13Cisotop@a],
                                                                                                                                                                                                                                                                                                                      True,
                                                                                                                                                                                                                                                                                                                      False
                                                                                                                                                                                                                                                                                                                  ]&&If[ Implies[is13Cisotop@d,is13Cisotop@b],
                                                                                                                                                                                                                                                                                                                         True,
                                                                                                                                                                                                                                                                                                                         False
                                                                                                                                                                                                                                                                                                                     ],
                                                                                                                                                                                                                                                      True
                                                                                                                                                                                                                                                  ]&&If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==0&&Abs@\[CapitalDelta]AE@{a,b,d,e}==Abs@\[CapitalDelta]AD@{a,b,d,e}&&MatchQ[Abs@\[CapitalDelta]AE@{a,b,d,e},{1,0,0,0,0,0,0,0,0}],
                                                                                                                                                                                                                                                         If[ isIon@a==isIon@e,
                                                                                                                                                                                                                                                             If[ Sign@countCatoms@a==Sign@countCatoms@e==1,
                                                                                                                                                                                                                                                                 is13Cisotop@a==is13Cisotop@e,
                                                                                                                                                                                                                                                                 True
                                                                                                                                                                                                                                                             ]&&If[ Sign@countOatoms@a==Sign@countOatoms@e==1,
                                                                                                                                                                                                                                                                    is18Oisotop@a==is18Oisotop@e,
                                                                                                                                                                                                                                                                    True
                                                                                                                                                                                                                                                                ]&&If[ Sign@countCatoms@b==countCatoms@d==1,
                                                                                                                                                                                                                                                                       is13Cisotop@b==is13Cisotop@d,
                                                                                                                                                                                                                                                                       True
                                                                                                                                                                                                                                                                   ]&&If[ Sign@countOatoms@b==Sign@countOatoms@d==1,
                                                                                                                                                                                                                                                                          is18Oisotop@b==is18Oisotop@d,
                                                                                                                                                                                                                                                                          True
                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                             If[ Sign@countCatoms@b==Sign@countCatoms@e==1,
                                                                                                                                                                                                                                                                 is13Cisotop@b==is13Cisotop@e,
                                                                                                                                                                                                                                                                 True
                                                                                                                                                                                                                                                             ]&&If[ Sign@countOatoms@b==Sign@countOatoms@e==1,
                                                                                                                                                                                                                                                                    is18Oisotop@b==is18Oisotop@e,
                                                                                                                                                                                                                                                                    True
                                                                                                                                                                                                                                                                ]&&If[ Sign@countCatoms@a==countCatoms@d==1,
                                                                                                                                                                                                                                                                       is13Cisotop@a==is13Cisotop@d,
                                                                                                                                                                                                                                                                       True
                                                                                                                                                                                                                                                                   ]&&If[ Sign@countOatoms@a==Sign@countOatoms@d==1,
                                                                                                                                                                                                                                                                          is18Oisotop@a==is18Oisotop@d,
                                                                                                                                                                                                                                                                          True
                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                         ],
                                                                                                                                                                                                                                                         True
                                                                                                                                                                                                                                                     ]&&If[ f==""&&Total@countCOgroups@{a,b}==Total@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==0&&MatchQ[Abs@\[CapitalDelta]AE@{a,b,d,e},{0,0,0,0,1,0,0,0,0}],
                                                                                                                                                                                                                                                            If[ isIon@a==isIon@e,
                                                                                                                                                                                                                                                                is13Cisotop@a==is13Cisotop@e&&is13Cisotop@b==is13Cisotop@d,
                                                                                                                                                                                                                                                                is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d
                                                                                                                                                                                                                                                            ],
                                                                                                                                                                                                                                                            True
                                                                                                                                                                                                                                                        ]&&If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==0&&Abs@\[CapitalDelta]AE@{a,b,d,e}!=Abs@\[CapitalDelta]AD@{a,b,d,e}&&!MatchQ[Abs@\[CapitalDelta]AD@{a,b,d,e},{_,_,1,_,_,_,_,_,_}],(* no C transfer *)
                                                                                                                                                                                                                                                               If[ isIon@a==isIon@e,
                                                                                                                                                                                                                                                                   If[ Sign@countCatoms@a==Sign@countCatoms@e==1,
                                                                                                                                                                                                                                                                       is13Cisotop@a==is13Cisotop@e,
                                                                                                                                                                                                                                                                       True
                                                                                                                                                                                                                                                                   ]&&If[ Sign@countCatoms@b==countCatoms@d==1,
                                                                                                                                                                                                                                                                          is13Cisotop@b==is13Cisotop@d,
                                                                                                                                                                                                                                                                          True
                                                                                                                                                                                                                                                                      ],
                                                                                                                                                                                                                                                                   If[ Sign@countCatoms@b==Sign@countCatoms@e==1,
                                                                                                                                                                                                                                                                       is13Cisotop@b==is13Cisotop@e,
                                                                                                                                                                                                                                                                       True
                                                                                                                                                                                                                                                                   ]&&If[ Sign@countCatoms@a==countCatoms@d==1,
                                                                                                                                                                                                                                                                          is13Cisotop@a==is13Cisotop@d,
                                                                                                                                                                                                                                                                          True
                                                                                                                                                                                                                                                                      ]
                                                                                                                                                                                                                                                               ],
                                                                                                                                                                                                                                                               True
                                                                                                                                                                                                                                                           ]&&If[ f!=""&&f!="e-",
                                                                                                                                                                                                                                                                  (Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})&&(* 13C18O bleibt zusammen *)Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}],
                                                                                                                                                                                                                                                                  True
                                                                                                                                                                                                                                                              ]
        ];
        Prepend[rest,refline],
        refline[[2]]==="IN"&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]-Total@countCOgroups@refline[[6;;8]]==1,(* CO Bindungen vorhanden, allerdings weniger bei den Produkten *)
        If[ debug,
            Print["IN: Case Subscript[X, non-iso-CO] O-atom switches partner "]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
        If[ f=="",
            If[ countCatoms@a==countCatoms@d,
                (is13Cisotop@a==is13Cisotop@d)&&(is13Cisotop@b==is13Cisotop@e),
                (is13Cisotop@a==is13Cisotop@e)&&(is13Cisotop@b==is13Cisotop@d)
            ],
            True
        ]&&If[ f=="",
               If[ countCatoms@b==countCatoms@e&&isCObound@b==isCObound@e==True&&(\[CapitalDelta]BE@refline[[{3,4,6,7}]])[[{3,5}]]=={0,1},
                   If[ Implies[isC18Obound@e,isC18Obound@b],
                       True,
                       False
                   ],
                   True
               ],
               True
           ]&&If[ (a==="He+"&&f==="He")||(a==="\[DoubleStruckCapitalH]+"&&f==="\[DoubleStruckCapitalH]"),
                  If[ countOatoms@b==countOatoms@d,
                      is18Oisotop@b==is18Oisotop@d,
                      True
                  ]&&If[ isCHnbound@b==isCHnbound@d,
                         isCHnisotop@b==isCHnisotop@d,
                         True
                     ]&&If[ isCHnbound@b==isCHnbound@e,
                            isCHnisotop@b==isCHnisotop@e,
                            True
                        ],
                  True
              ]&&If[ f!=""&&Total@countCatoms@{a,b,c}==1,
                     If[ countCatoms@b==countCatoms@e,
                         If[ Implies[is18Oisotop@e,is18Oisotop@b],
                             True,
                             False
                         ],
                         If[ Implies[is18Oisotop@d,is18Oisotop@b],
                             True,
                             False
                         ]
                     ],
                     True
                 ]];
        Prepend[rest,refline],
        (* alle C atome links oder echts in einem Molekuel *)
        refline[[2]]==="IN"&&(Total@Sign@countCatoms@refline[[3;;5]]==1||Total@Sign@countCatoms@refline[[6;;9]]==1),
        If[ debug,
            Print["IN:Bundled C atoms - no action"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ Or@@isCPbound@{a,b,c}&&Or@@isCPbound@{d,e,f,g},
                                                                                                      Or@@isCPisotop@{a,b,c}==Or@@isCPisotop@{d,e,f,g},
                                                                                                      True
                                                                                                  ]&&If[ isCObound@a&&isCObound@d&&(isCObound@b==isCObound@e==False)&&(\[CapitalDelta]AD@refline[[{3,4,6,7}]])[[{3,5}]]=={0,-1},
                                                                                                         If[ Implies[isC18Obound@a,isC18Obound@d],
                                                                                                             True,
                                                                                                             False
                                                                                                         ],
                                                                                                         True
                                                                                                     ]&&If[ isCObound@a&&isCObound@e&&(isCObound@b==isCObound@d==False)&&(\[CapitalDelta]AE@refline[[{3,4,6,7}]])[[{3,5}]]=={0,-1},
                                                                                                            If[ Implies[isC18Obound@a,isC18Obound@e],
                                                                                                                True,
                                                                                                                False
                                                                                                            ],
                                                                                                            True
                                                                                                        ]];
        Prepend[rest,refline],
        (* C atome in 4 oder mehr Reaktanden *)
        refline[[2]]==="IN"&&Total@Sign@countCatoms@refline[[3;;9]]>=4,
        If[ debug,
            Print["IN:C atoms everywhere"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,d,e}&&If[ findPartnerB@refline[[{3,4,6,7}]]==1,
                                                                                                                           If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]>0(* C Abgabe von B an E *),
                                                                                                                               If[ Implies[is13Cisotop@d,is13Cisotop@b],
                                                                                                                                   True,
                                                                                                                                   False
                                                                                                                               ],
                                                                                                                               True
                                                                                                                           ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]<0(* C Abgabe von B an E *),
                                                                                                                                  If[ Implies[is13Cisotop@b,is13Cisotop@d],
                                                                                                                                      True,
                                                                                                                                      False
                                                                                                                                  ],
                                                                                                                                  True
                                                                                                                              ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]==0(* keine C Abgabe von B an E *),
                                                                                                                                     is13Cisotop@b==is13Cisotop@d&&is13Cisotop@a==is13Cisotop@e,
                                                                                                                                     True
                                                                                                                                 ],
                                                                                                                           True
                                                                                                                       ]&&If[ findPartnerB@refline[[{3,4,6,7}]]==2,(*findPartnerB==2*)
                                                                                                                              If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]>0(* C Abgabe von B an D *),
                                                                                                                                  If[ Implies[is13Cisotop@e,is13Cisotop@b],
                                                                                                                                      True,
                                                                                                                                      False
                                                                                                                                  ],
                                                                                                                                  True
                                                                                                                              ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]<0(* C Abgabe von B an D *),
                                                                                                                                     If[ Implies[is13Cisotop@b,is13Cisotop@e],
                                                                                                                                         True,
                                                                                                                                         False
                                                                                                                                     ],
                                                                                                                                     True
                                                                                                                                 ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]==0(* keine C Abgabe von B an D *),
                                                                                                                                        is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d,
                                                                                                                                        True
                                                                                                                                    ],
                                                                                                                              True
                                                                                                                          ]&&If[ findPartnerB@refline[[{3,4,6,7}]]==0,
                                                                                                                                 If[ isCNbound@e&&countNatoms@b==1&&(\[CapitalDelta]BD@{a,b,d,e})[[3;;4]]!={-1,1},
                                                                                                                                     If[ Implies[isCNisotop@e,is13Cisotop@b],
                                                                                                                                         True,
                                                                                                                                         False
                                                                                                                                     ],
                                                                                                                                     True
                                                                                                                                 ]&&If[ (countNatoms@d==countNatoms@b==1)&&(countCatoms@b>=countCatoms@d),
                                                                                                                                        If[ Implies[is13Cisotop@d,is13Cisotop@b],
                                                                                                                                            True,
                                                                                                                                            False
                                                                                                                                        ],
                                                                                                                                        True
                                                                                                                                    ]&&If[ isCPbound@d&&isCPbound@b&&(countCatoms@b>=countCatoms@d),
                                                                                                                                           If[ Implies[isCPisotop@d,isCPisotop@b],
                                                                                                                                               True,
                                                                                                                                               False
                                                                                                                                           ],
                                                                                                                                           True
                                                                                                                                       ]&&If[ (Not@isCPbound@b&&isCPbound@d)&&(countPatoms@b==countPatoms@d)&&(countCatoms@b>=countCatoms@d),
                                                                                                                                              If[ Implies[isCPisotop@d,is13Cisotop@b],
                                                                                                                                                  True,
                                                                                                                                                  False
                                                                                                                                              ],
                                                                                                                                              True
                                                                                                                                          ],
                                                                                                                                 True
                                                                                                                             ]&&
        If[ findPartnerB@refline[[{3,4,6,7}]]==0,
            If[ (MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,1,_,_,_,_,_,_}]||MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,2,_,_,_,_,_,_}])&&Norm@\[CapitalDelta]BD@{a,b,d,e}<Norm@\[CapitalDelta]BE@{a,b,d,e},
                If[ Implies[is13Cisotop@d,is13Cisotop@b],
                    True,
                    False
                ]&&If[ Implies[is13Cisotop@a,is13Cisotop@e],
                       True,
                       False
                   ],
                True
            ]&&If[ (MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,-1,_,_,_,_,_,_}]||MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,-2,_,_,_,_,_,_}])&&Norm@\[CapitalDelta]BD@{a,b,d,e}<Norm@\[CapitalDelta]BE@{a,b,d,e},
                   If[ Implies[is13Cisotop@b,is13Cisotop@d],
                       True,
                       False
                   ]&&If[ Implies[is13Cisotop@e,is13Cisotop@a],
                          True,
                          False
                      ],
                   True
               ]&&If[ (MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,1,_,_,_,_,_,_}]||MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,2,_,_,_,_,_,_}])&&Norm@\[CapitalDelta]BD@{a,b,d,e}>Norm@\[CapitalDelta]BE@{a,b,d,e},
                      If[ Implies[is13Cisotop@e,is13Cisotop@b],
                          True,
                          False
                      ]&&If[ Implies[is13Cisotop@a,is13Cisotop@d],
                             True,
                             False
                         ],
                      True
                  ]&&If[ (MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,-1,_,_,_,_,_,_}]||MatchQ[\[CapitalDelta]BD@{a,b,d,e},{_,_,-2,_,_,_,_,_,_}])&&Norm@\[CapitalDelta]BD@{a,b,d,e}>Norm@\[CapitalDelta]BE@{a,b,d,e},
                         If[ Implies[is13Cisotop@e,is13Cisotop@b],
                             True,
                             False
                         ]&&If[ Implies[is13Cisotop@a,is13Cisotop@d],
                                True,
                                False
                            ],
                         True
                     ],
            True
        ]
        ];
        Prepend[rest,refline],
        (* Find Partner findet keine Partner  *)
        refline[[2]]==="IN"&&refline[[8]]==""&&findPartnerA@refline[[{3,4,6,7}]]===0,
        If[ debug,
            Print["IN: Letzter Filter"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,d,e}&&If[ \[CapitalDelta]AE@refline[[{3,4,6,7}]]=={-3,0,-1,0,0,0,0,0,0},
                                                                                                                           If[ countCatoms@refline[[3]]>0&&countCatoms@refline[[7]]>1,
                                                                                                                               If[ Implies[is13Cisotop@a,is13Cisotop@e],
                                                                                                                                   True,
                                                                                                                                   False
                                                                                                                               ],
                                                                                                                               True
                                                                                                                           ],
                                                                                                                           True
                                                                                                                       ]];
        Prepend[rest,refline],
        True,list]
    ]


(* -------------------------------------------------------------------------------------------------------------------------------*)
regelCheckNN[num_Integer] :=
    Module[ {alt,neu},
        alt = showR@num;
        neu = cleanUpNN@showR@num;
        Map[If[ MemberQ[neu,#],
                Style[#,Bold],
                Style[#,FontVariations->{"StrikeThrough"->True}]
            ]&,alt]
    ]
Clear[cleanUpNN];
cleanUpNN[list_List] :=
    Module[ {rest,refline,debug},
    debug = False;
    refline = list[[1]];
    rest = Rest[list];
    Which[(* NN: neutral-neutral reactions *)
    (* Spezial Fall # 118 *)
    refline[[2;;9]]=={"NN","C","NCCN","","CN","C2N","",""},
    If[ debug,
        Print["IN: Custom Case #118 - C-C bond breaks"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
    (*If[(!isCOisotop@b&&is13Cisotop@d)||(isCOisotop@b&&!is13Cisotop@d),False,True]*)
    If[ Implies[is13Cisotop@a,is13Cisotop@e],
        True,
        False
    ]];
    Prepend[rest,refline],
    (* Trivial Case: nicht mehr als 1 C und 1 O atom insgesamt *)
    refline[[2]]==="NN"&&Total@countCatoms@refline[[3;;5]]<=1&&Total@countOatoms@refline[[3;;5]]<=1,
    If[ debug,
        Print["NN:Trivial Case - no action."]
    ];
    Prepend[rest,refline],
    (* Arbitrary Case: \[CapitalDelta]AD = -\[CapitalDelta]AE && keine gemischten Vorzeichen und nur 4 Reaktanden *)
    refline[[2]]==="NN"&&refline[[8]]==""&&!mixedSigns@\[CapitalDelta]AD@refline[[{3,4,6,7}]]&&!mixedSigns@\[CapitalDelta]AE@refline[[{3,4,6,7}]]&&\[CapitalDelta]AD@refline[[{3,4,6,7}]]==-\[CapitalDelta]AE@refline[[{3,4,6,7}]],
    If[ debug,
        Print["NN:Arbitrary Case - almost no decision possible"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ Abs@\[CapitalDelta]AD@{a,b,d,e}=={1,0,0,0,0,0,0,0,0},
                                                                                                  Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}],
                                                                                                  True
                                                                                              ]];
    Prepend[rest,refline],
    (* Klarer Protonen/H - Transfer, i.e. \[CapitalDelta]AD|\[CapitalDelta]AE={1,0,0,0,0....} *)
    refline[[2]]==="NN"&&refline[[8]]==""&&Total@countCatoms@refline[[3;;5]]>1&&Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]!=Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]]&&(Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0}||Abs@\[CapitalDelta]AD@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0}),
    If[ debug,
        Print["NN: H/Proton-Transfer"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,d,e}&&If[ Abs@\[CapitalDelta]AE@refline[[{3,4,6,7}]]=={1,0,0,0,0,0,0,0,0},
                                                                                                                       If[ isCNbound@b==isCNbound@d,
                                                                                                                           isCNisotop@b==isCNisotop@d&&isleft13CNbound@b==isleft13CNbound@d,
                                                                                                                           True
                                                                                                                       ]&&If[ isCNbound@a==isCNbound@e,
                                                                                                                              isCNisotop@a==isCNisotop@e&&isleft13CNbound@a==isleft13CNbound@e,
                                                                                                                              True
                                                                                                                          ]&&If[ isCObound@b==isCObound@d,
                                                                                                                                 isleft18Obound@b==isleft18Obound@d,
                                                                                                                                 True
                                                                                                                             ]&&If[ isCObound@a==isCObound@e,
                                                                                                                                    isleft18Obound@a==isleft18Obound@e,
                                                                                                                                    True
                                                                                                                                ]&&If[ isCOCHnbound@b==isCOCHnbound@d==False,
                                                                                                                                       isCOisotop@b==isCOisotop@d,
                                                                                                                                       True
                                                                                                                                   ]&&If[ isCOCHnbound@b!=isCOCHnbound@d==False,
                                                                                                                                          If[ Implies[isCOisotop@d,isCOisotop@b],
                                                                                                                                              True,
                                                                                                                                              False
                                                                                                                                          ],
                                                                                                                                          True
                                                                                                                                      ]&&If[ isCOCHnbound@a==isCOCHnbound@e==False,
                                                                                                                                             isCOisotop@a==isCOisotop@e,
                                                                                                                                             True
                                                                                                                                         ]&&If[ isCOCHnbound@a!=isCOCHnbound@e==False,
                                                                                                                                                If[ Implies[isCOisotop@a,isCOisotop@e],
                                                                                                                                                    True,
                                                                                                                                                    False
                                                                                                                                                ],
                                                                                                                                                True
                                                                                                                                            ]&&is13Cisotop@b==is13Cisotop@d&&is13Cisotop@a==is13Cisotop@e&&is18Oisotop@b==is18Oisotop@d&&is18Oisotop@a==is18Oisotop@e&&If[ isCHnbound@a==isCHnbound@e&&isCOCHnbound@a==isCOCHnbound@e,
                                                                                                                                                                                                                                                                           isCHnisotop@a==isCHnisotop@e,
                                                                                                                                                                                                                                                                           True
                                                                                                                                                                                                                                                                       ]&&If[ isCHnbound@b==isCHnbound@d&&isCOCHnbound@b==isCOCHnbound@d,
                                                                                                                                                                                                                                                                              isCHnisotop@b==isCHnisotop@d,
                                                                                                                                                                                                                                                                              True
                                                                                                                                                                                                                                                                          ],
                                                                                                                       If[ isCNbound@b==isCNbound@e,
                                                                                                                           isCNisotop@b==isCNisotop@e&&isleft13CNbound@b==isleft13CNbound@e,
                                                                                                                           True
                                                                                                                       ]&&If[ isCNbound@a==isCNbound@d,
                                                                                                                              isCNisotop@a==isCNisotop@d&&isleft13CNbound@a==isleft13CNbound@d,
                                                                                                                              True
                                                                                                                          ]&&If[ isCObound@b==isCObound@e,
                                                                                                                                 isleft18Obound@b==isleft18Obound@e,
                                                                                                                                 True
                                                                                                                             ]&&If[ isCObound@a==isCObound@d,
                                                                                                                                    isleft18Obound@a==isleft18Obound@d,
                                                                                                                                    True
                                                                                                                                ]&&
                                                                                                                       isCOisotop@b==isCOisotop@e&&isCOisotop@a==isCOisotop@d&&is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d&&is18Oisotop@b==is18Oisotop@e&&is18Oisotop@a==is18Oisotop@d&&If[ isCHnbound@a==isCHnbound@d,
                                                                                                                                                                                                                                                                                                           isCHnisotop@a==isCHnisotop@d,
                                                                                                                                                                                                                                                                                                           True
                                                                                                                                                                                                                                                                                                       ]&&If[ isCHnbound@b==isCHnbound@e,
                                                                                                                                                                                                                                                                                                              isCHnisotop@b==isCHnisotop@e,
                                                                                                                                                                                                                                                                                                              True
                                                                                                                                                                                                                                                                                                          ]
                                                                                                                   ]];
    Prepend[rest,refline],
    (* CN UND CO Bindung in Edukt und Produkt - Annahme: Bindung bleibt erhalten*)
    refline[[2]]==="NN"&&((isCNbound@refline[[3]]&&isCObound@refline[[3]])||(isCNbound@refline[[4]]&&isCObound@refline[[4]])),
    If[ debug,
        Print["NN:Case 0a"]
    ];
        (* special cases: C in alen reaktanden, bzw. O in allen Reaktanden*)
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCNisotop@{a,b,c}&&Xor@@isCNisotop@{d,e,f})||(Or@@isNotCNisotop@{a,b,c}&&Or@@isNotCNisotop@{d,e,f})&&If[ Total@Sign@countCatoms@{a,b,d,e}==4,
                                                                                                                                                                                                             is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d,
                                                                                                                                                                                                             True
                                                                                                                                                                                                         ]
    &&If[ Total@Sign@countOatoms@{a,b,d,e}==4&&findPartnerA@{a,b,d,e}==0,
          If[ Implies[is18Oisotop@a,is18Oisotop@d],
              True,
              False
          ]&&If[ Implies[is18Oisotop@b,is18Oisotop@e],
                 True,
                 False
             ],
          True
      ]&&If[ Total@Sign@countOatoms@{a,b,d,e}==4&&findPartnerA@{a,b,d,e}==1&&(Abs@\[CapitalDelta]AX@{a,b,d,e})[[5]]==0(* no O-atom movement*),
             If[ Implies[is18Oisotop@a,is18Oisotop@d],
                 True,
                 False
             ]&&If[ Implies[is18Oisotop@b,is18Oisotop@e],
                    True,
                    False
                ],
             True
         ]&&If[ Total@Sign@countOatoms@{a,b,d,e}==4&&findPartnerA@{a,b,d,e}==2&&(Abs@\[CapitalDelta]AX@{a,b,d,e})[[5]]==0(* no O-atom movement*),
                If[ Implies[is18Oisotop@a,is18Oisotop@e],
                    True,
                    False
                ]&&If[ Implies[is18Oisotop@b,is18Oisotop@d],
                       True,
                       False
                   ],
                True
            ](*CO und CN in a  bzw. b -> CN bleibt erhalten - allerdings auch von 1b geleistet*)];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&Total[countOatoms/@refline[[3;;5]]]<2&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]]&&Xor@@isCNbound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? Nur wenn die 2 CO Bindungen nicht in einem Molekuel auftreten, dann funktioniert das nicht! *)
    If[ debug,
        Print["NN:Case 0b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))||(Or@@isCNisotop@{a,b,c}&&(Xor@@isCNisotop@{d,e,f}))||(And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f})&&(And@@isNotCNisotop@{d,e,f}))(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten*)];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&Total[countOatoms/@refline[[3;;5]]]>1&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]]&&Xor@@isCNbound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? Nur wenn die 2 CO Bindungen nicht in einem Molekuel auftreten, dann funktioniert das nicht! *)
    If[ debug,
        Print["NN:Case 0Subscript[b, 1]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;((Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))||((Or@@isCNisotop@{a,b,c}&&(Xor@@isCNisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))||((And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f})&&(And@@isNotCNisotop@{d,e,f}))&&((Or@@isC18Obound@{a,b,c}&&Or@@isC18Obound@{d,e,f})||(Nor@@isC18Obound@{a,b,c}&&Nor@@isC18Obound@{d,e,f})))(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten, und 18O bleibt auch an der korrekten Stelle (check #2904)*)];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Total[countCatoms/@refline[[3;;5]]]>=3&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]],(* SPEZIALFALL: CO und CN Bindung links aber keine klare CN Bindung rechts, Entweder aufgebrochen, oder versteckt in beispielsweise Subscript[C, n]N *)
    If[ debug,
        Print["NN:Case 0Subscript[b, 2]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&(Xor@@isCOisotop@{d,e,f}))||(Or@@isCNisotop@{a,b,c}&&(And@@isNotCOisotop@{d,e,f}))||(And@@isNotCOisotop@{a,b,c}&&(And@@isNotCNisotop@{a,b,c})&&(And@@isNotCOisotop@{d,e,f}))(* FUnktioniert wahrscheinlich nicht immer*)];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Total[countCatoms/@refline[[3;;5]]]<3&&((isCNbound@refline[[3]]&&isCObound@refline[[4]])||(isCNbound@refline[[4]]&&isCObound@refline[[3]]))&&Xor@@isCObound@refline[[6;;8]],(*Xor, um die Reaktionen mit 2 CO bindungen auszuschliessen, die landen dann bei 1b? *)
    If[ debug,
        Print["NN:Case 0c"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCOisotop@{a,b,c}&&Xor@@isCOisotop@{d,e,f})||(And@@isNotCOisotop@{a,b,c}&&And@@isNotCOisotop@{d,e,f})(*CO in a und CN in b  bzw. umgekehrt -> CO bleibt erhalten*)];
    Prepend[rest,refline],
    (* CN Bindung in Edukt und Produkt - Annahme: Bindung bleibt erhalten*)
    refline[[2]]==="NN"&&Or@@isCNbound@refline[[3;;5]]&&Or@@isCNbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countNatoms@refline[[3;;5]]==1,
    If[ debug,
        Print["NN:Case 1a"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCNisotop@{a,b,c}&&Xor@@isCNisotop@{d,e,f})||(And@@isNotCNisotop@{a,b,c}&&And@@isNotCNisotop@{d,e,f})(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Or@@isCNbound@refline[[4]]&&Or@@isCNbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&countNatoms@refline[[4]]==1,
    If[ debug,
        Print["NN:Case 1b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(isCNisotop@b&&Xor@@isCNisotop@{d,e,f})||(isNotCNisotop@b&&Or@@isNotCNisotop@{d,e,f})(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
    Prepend[rest,refline],
    (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
    (* alle Molekuele haben CHn Bindungen -> nur Austausch von H+ oder H2+ ,
    a entspricht e, b entspricht d*)
    refline[[2]]==="NN"&&And@@isCHnbound@refline[[{3,4,6,7}]]&&findPartnerA@refline[[{3,4,6,7}]]!=0,
    If[ debug,
        Print["NN:Case Subscript[1.5, 4CHn]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ countCHngroups@{a,b}==countCHngroups@{e,d},
                                                                                                  isCHnisotop@a==isCHnisotop@e&&isCHnisotop@b==isCHnisotop@d&&isCOisotop@a==isCOisotop@e&&isCOisotop@b==isCOisotop@d&&isC18Obound@a==isC18Obound@e&&isC18Obound@b==isC18Obound@d,
                                                                                                  If[ Implies[isCHnisotop@a,isCHnisotop@e],
                                                                                                      True,
                                                                                                      False
                                                                                                  ]&&isCOisotop@a==isCOisotop@e&&isCOisotop@b==isCOisotop@d&&isC18Obound@a==isC18Obound@e&&isC18Obound@b==isC18Obound@d
                                                                                              ]];
    Prepend[rest,refline],
    (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
    (* 2 bindungen rechts wie links, allerdings nicht gleichzeitig in d und e*)
    refline[[2]]==="NN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]==2&&Nand@@isCObound@refline[[6;;7]],
    If[ debug,
        Print["NN:Case Subscript[1.5, 2CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})(* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
    Prepend[rest,refline],
    (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
    (* Maximal 1 CO Bindung rechts bzw. links, und nicht mehr als 2 CHn Gruppen *)
    refline[[2]]==="NN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]<3&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]<=1,
    If[ debug,
        Print["NN:Case 1.5"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(Or@@isCHnisotop@{a,b,c}==Or@@isCHnisotop@{d,e,f})&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})(* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
    Prepend[rest,refline],
    (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus *)
    (* geiche Zahl bindungen rechts wie links, und entweder 3 CO Bindungen rechts und links oder ungleich viele CO Bindungen links und rechts*)
    refline[[2]]==="NN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&Total@countCHngroups@refline[[3;;5]]==Total@countCHngroups@refline[[6;;8]]&&(Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]]==3||Total@countCOgroups@refline[[3;;5]]!=Total@countCOgroups@refline[[6;;8]])&&findPartnerA@refline[[{3,4,6,7}]]!=0&&findPartnerB@refline[[{3,4,6,7}]]!=0,
    If[ debug,
        Print["NN:Case SubscriptBox[\(1.5\),\(\(\(3\)\(/\)\)!=\(\(\\)\(CO\)\)\)]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
    If[ findPartnerA@refline[[{3,4,6,7}]]==1,
        (* Partner A \[LongLeftRightArrow] D : *)
        Which[\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[1]]!=0,isCHnisotop@a==isCHnisotop@d&&isCOisotop@a==isCOisotop@d&&is18Oisotop@a==is18Oisotop@d&&isleft18Obound@a==isleft18Obound@d&&isleft18Obound@b==isleft18Obound@e&&isCNisotop@a==isCNisotop@d,\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[5]]!=0(* 5=> O*),isCHnisotop@a==isCHnisotop@d&&isCNisotop@a==isCNisotop@d&&isleft18Obound@a==isleft18Obound@d&&isleft18Obound@b==isleft18Obound@e,True,True],(* Partner A \[LongLeftRightArrow] E : *)
        Which[\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[1]]!=0,isCHnisotop@a==isCHnisotop@e&&isCOisotop@a==isCOisotop@e&&is18Oisotop@a==is18Oisotop@e&&isCNisotop@a==isCNisotop@e&&isleft18Obound@a==isleft18Obound@e&&isleft18Obound@b==isleft18Obound@d,\[CapitalDelta]AX[refline[[{3,4,6,7}]]][[5]]!=0(* 5=> O*),isCHnisotop@a==isCHnisotop@e&&isCNisotop@a==isCNisotop@e&&isleft18Obound@a==isleft18Obound@e&&isleft18Obound@b==isleft18Obound@d,True,True]
    ]
    (* Falls CHn in allen {a,b,d,e}, dann nur C18O und COiso erhalten *)(*(Not@isCNisotop@b&&Nor@@isCNisotop@{d,e,f})*)];
    Prepend[rest,refline],
    (* sucht nach CHn Gruppen erhaelt deren Isotopenstatus , ungleiche Anzahl CHn Gruppen
    aber bei erhaltener CO Bindung! geht nicht mit bsp: #862*)
    refline[[2]]==="NN"&&Or@@isCHnbound@refline[[3;;5]]&&Or@@isCHnbound@refline[[6;;8]]&&Total[countCatoms/@refline[[3;;5]]]>1&&refline[[8]]===""&&Or@@isCObound@refline[[3;;5]]==Or@@isCObound@refline[[6;;8]]&&Total@countCHngroups@refline[[3;;5]]!=Total@countCHngroups@refline[[6;;8]]&&findPartnerA@refline[[{3,4,6,7}]]!=0,
    If[ debug,
        Print["NN:Case 1.6"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ (\[CapitalDelta]AX@{a,b,d,e})[[3]]==0(* no C-atom transfer *),
                                                                                                  If[ findPartnerA@{a,b,d,e}==1,
                                                                                                      is13Cisotop@{a,b}==is13Cisotop@{d,e},
                                                                                                      is13Cisotop@{a,b}==is13Cisotop@{e,d}
                                                                                                  ],
                                                                                                  True
                                                                                              ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[5]]==0(* no O-atom transfer *),
                                                                                                     If[ findPartnerA@{a,b,d,e}==1,
                                                                                                         is18Oisotop@{a,b}==is18Oisotop@{d,e},
                                                                                                         is18Oisotop@{a,b}==is18Oisotop@{e,d}
                                                                                                     ],
                                                                                                     True
                                                                                                 ]&&
    If[ findPartnerA@{a,b,d,e}==1,
        If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&isCHnbound@a==isCHnbound@d,
            isCHnisotop@a==isCHnisotop@d,
            True
        ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&isCHnbound@b==isCHnbound@e,
               isCHnisotop@b==isCHnisotop@e,
               True
           ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@a==isCHnbound@d,
                  If[ Implies[isCHnisotop@d,isCHnisotop@a],
                      True,
                      False
                  ],
                  True
              ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@b==isCHnbound@e,
                     If[ Implies[isCHnisotop@e,isCHnisotop@b],
                         True,
                         False
                     ],
                     True
                 ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@a==isCObound@d,
                        isCOisotop@d==isCOisotop@a,
                        True
                    ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@b==isCObound@e,
                           isCOisotop@b==isCOisotop@e,
                           True
                       ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@a==isCObound@d,
                              isCOisotop@d==isCOisotop@a,
                              True
                          ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@b==isCObound@e,
                                 isCOisotop@b==isCOisotop@e,
                                 True
                             ],
        True
    ]&&If[ findPartnerA@{a,b,d,e}==2,
           If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&isCHnbound@a==isCHnbound@e,
               isCHnisotop@a==isCHnisotop@e,
               True
           ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]!={3,1}&&isCHnbound@b==isCHnbound@d,
                  isCHnisotop@b==isCHnisotop@d,
                  True
              ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@a==isCHnbound@e,
                     If[ Implies[isCHnisotop@e,isCHnisotop@a],
                         True,
                         False
                     ],
                     True
                 ]&&If[ Abs[\[CapitalDelta]AX@{a,b,d,e}][[{1,3}]]=={3,1}&&isCHnbound@b==isCHnbound@d,
                        If[ Implies[isCHnisotop@d,isCHnisotop@b],
                            True,
                            False
                        ],
                        True
                    ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@a==isCObound@e,
                           isCOisotop@e==isCOisotop@a,
                           True
                       ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@b==isCObound@d,
                              isCOisotop@b==isCOisotop@d,
                              True
                          ]&&If[ (\[CapitalDelta]AX@{a,b,d,e})[[{1,3}]]=={3,1}&&isCObound@a==isCObound@e,
                                 isCOisotop@e==isCOisotop@a,
                                 True
                             ]&&If[ (\[CapitalDelta]BX@{a,b,d,e})[[{1,3}]]=={-3,-1}&&isCObound@b==isCObound@d,
                                    isCOisotop@b==isCOisotop@d,
                                    True
                                ](*&&If[isCObound@b==isCObound@d,isCOisotop@b==isCOisotop@d,True]&&If[isCObound@a==isCObound@e,isCOisotop@a==isCOisotop@e,True]*),
           True
       ]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)(* SPezialFall: beide Edukte und Produkte stimmen ueberein: #2787*)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=5&&StringLength[neutral@refline[[4]]]>=5&&StringLength[neutral@refline[[7]]]>=5&&StringLength[neutral@refline[[3]]]>=5&&StringTake[neutral@refline[[6]],5]===StringTake[neutral@refline[[4]],5]&&StringTake[neutral@refline[[7]],5]===StringTake[neutral@refline[[3]],5],
    If[ debug,
        Print["NN:Case Subscript[2, 0]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,5]===StringTake[neutral@b,5]&&StringTake[neutral@e,5]===StringTake[neutral@a,5]];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&(StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4]||StringTake[neutral@refline[[6]],-3]===StringTake[neutral@refline[[4]],-3])&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],
    If[ debug,
        Print["NN:Case Subscript[2, CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[neutral@d,4]===StringTake[neutral@b,4]||StringTake[neutral@d,-3]===StringTake[neutral@b,-3])&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4],
    If[ debug,
        Print["NN:Case 2"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,4]===StringTake[neutral@b,4]];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[7]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&(StringTake[neutral@refline[[7]],4]===StringTake[neutral@refline[[4]],4]||StringTake[neutral@refline[[7]],-3]===StringTake[neutral@refline[[4]],-3])&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],
    If[ debug,
        Print["NN:Case 2Subscript[b, CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[neutral@e,4]===StringTake[neutral@b,4]||StringTake[neutral@e,-3]===StringTake[neutral@b,-3])&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[7]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[7]],4]===StringTake[neutral@refline[[4]],4],
    If[ debug,
        Print["NN:Case 2b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,4]===StringTake[neutral@b,4]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
    (* wechsel von ST (_,4) -> 3 *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[3]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[3]],4]&&StringLength[neutral@refline[[7]]]>=2&&StringTake[neutral@refline[[6]],2]=!=StringTake[neutral@refline[[7]],2],
    If[ debug,
        Print["NN:Case 3"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,3]===StringTake[neutral@a,3]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=4&&StringLength[neutral@refline[[4]]]>=4&&StringTake[neutral@refline[[6]],4]===StringTake[neutral@refline[[4]],4],
    If[ debug,
        Print["NN:Case 3b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@d,4]===StringTake[neutral@b,4]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringLength[neutral@refline[[6]]]>=2&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[3]],3]&&StringTake[neutral@refline[[7]],2]=!=StringTake[neutral@refline[[6]],2],
    If[ debug,
        Print["NN:Case 3c - First 3 characters of a==e"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,2]===StringTake[neutral@a,2]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), funktionale Gruppen am Anfang bleiben erhalten *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringLength[neutral@refline[[3]]]>=2&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[4]],3]&&StringTake[neutral@refline[[3]],2]=!=StringTake[neutral@refline[[4]],2],
    If[ debug,
        Print["NN:Case 3d - First 3 characters of b==e"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral@e,2]===StringTake[neutral@b,2]];
    Prepend[rest,refline],
    (* partieller ueberlapp von edukt und produkt ( _ 4_ Buchstaben!), Ladung kennzeichnet das Molekuel - 1 oder 2 H-atome wechseln den Partner *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[7]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringLength[neutral@refline[[6]]]>=2&&StringTake[neutral@refline[[7]],3]===StringTake[neutral@refline[[3]],3]&&StringTake[neutral@refline[[7]],2]===StringTake[neutral@refline[[6]],2]&&Total@countCatoms@refline[[3;;5]]>1,
    If[ debug,
        Print["NN:Case 3e - First 3 characters of a==e \[And] d==e - charge sticks to molecule"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ isIon@e===isIon@a,
                                                                                                  StringTake[neutral@e,2]===StringTake[neutral@a,2],
                                                                                                  StringTake[neutral@d,2]===StringTake[neutral@a,2]
                                                                                              ]];
    Prepend[rest,refline],
    (*refline[[2]]==="DR"&&refline[[4]]==="e-"&&isIon[refline[[3]]]&&StringLength[refline[[6]]]>=3&&StringTake[refline[[6]],3]===StringTake[refline[[3]],3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Anfang bleiben erhalten *)
    rest=Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[d,3]===StringTake[a,3]];
    Prepend[rest,refline],*)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[3]]],-3]&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
    If[ debug,
        Print["NN: Case Subscript[4, CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[a],-3]&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
    Prepend[rest,refline],(* Unklar ob -3 ausreichend ist. TESTEN!!!*)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[3]]],-3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
    If[ debug,
        Print["NN: Case 4"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[a],-3]];
    Prepend[rest,refline],(* Unklar ob -3 ausreichend ist. TESTEN!!!*)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[4]]],-3]&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
    If[ debug,
        Print["NN: Case 4Subscript[b, CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[b],-3]&&(Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[6]]]>=3&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral@refline[[6]],-3]===StringTake[neutral[refline[[4]]],-3],(* partieller ueberlapp von edukt und produkt, funktionale Gruppen am Ende bleiben erhalten *)
    If[ debug,
        Print["NN: Case 4b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;StringTake[neutral[d],-3]===StringTake[neutral[b],-3]];
    Prepend[rest,refline],
    (* Produkt 1 oder 2 stimmen mit Anfang von Edukt 1 ueberein*)
    refline[[2]]==="NN"&&Or@@{(StringMatchQ[neutral[refline[[3]]],neutral[refline[[7]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[3]]],{StringLength[neutral[refline[[7]]]]+1}],NumberString]),(StringMatchQ[neutral[refline[[3]]],neutral[refline[[6]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[3]]],{StringLength[neutral[refline[[6]]]]+1}],NumberString])},
    If[ debug,
        Print["NN: Case 5"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@{StringMatchQ[neutral[a],neutral[e]~~__],StringMatchQ[neutral[a],neutral[d]~~__]}];
    Prepend[rest,refline],(* CH3 -> CH2 aber complex bleibt erhalten *)
    (* reactions of type Y + C2X -> CY + CX , (X = N or O ) if Y = C or O rule 5b fails*)
    refline[[2]]==="NN"&&(MemberQ[refline[[3;;5]],"C2N"]||MemberQ[refline[[3;;5]],"C2O"]),
    If[ debug,
        Print["NN: Case 5b - b==C2X"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ Implies[is13Cisotop@e,is13Cisotop@b],
                                                                                                  True,
                                                                                                  False
                                                                                              ]&&If[ Implies[is18Oisotop@a,is18Oisotop@d],
                                                                                                     True,
                                                                                                     False
                                                                                                 ]&&If[ Implies[is13Cisotop@a,is13Cisotop@d],
                                                                                                        True,
                                                                                                        False
                                                                                                    ]&&If[ Implies[is18Oisotop@e,is18Oisotop@b],
                                                                                                           True,
                                                                                                           False
                                                                                                       ]
    ];
    Prepend[rest,refline],
    (* Produkt 1 oder 2 stimmen mit Anfang von Edukt 1 ueberein*)
    refline[[2]]==="NN"&&Or@@{StringLength@refline[[4]]>=3&&StringLength@refline[[7]]>=3&&(StringMatchQ[neutral[refline[[4]]],neutral[refline[[7]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[4]]],{StringLength[neutral[refline[[7]]]]+1}],NumberString]),StringLength@refline[[4]]>=3&&StringLength@refline[[6]]>=3&&(StringMatchQ[neutral[refline[[4]]],neutral[refline[[6]]]~~__]&&!StringMatchQ[StringTake[neutral[refline[[4]]],{StringLength[neutral[refline[[6]]]]+1}],NumberString])},
    If[ debug,
        Print["NN: Case 5b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;Or@@{StringMatchQ[neutral[b],neutral[e]~~__],StringMatchQ[neutral[b],neutral[d]~~__]}];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&StringLength[neutral@refline[[3]]]>=3&&StringTake[neutral[refline[[3]]],3]=="CH3"&&(If[ StringLength[neutral[refline[[6]]]]>2,
                                                                                                                 StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[6]]]]]]===StringTake[neutral[refline[[6]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[6]]]]]],
                                                                                                                 False
                                                                                                             ]||If[ StringLength[neutral[refline[[7]]]]>2,
                                                                                                                    StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[7]]]]]]===StringTake[neutral[refline[[7]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[7]]]]]],
                                                                                                                    False
                                                                                                                ]||If[ StringLength[neutral[refline[[8]]]]>2,
                                                                                                                       StringTake[StringReplacePart[neutral[refline[[3]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[8]]]]]]===StringTake[neutral[refline[[8]]],Min[4,StringLength[neutral@refline[[3]]],StringLength[neutral[refline[[8]]]]]],
                                                                                                                       False
                                                                                                                   ]),
    If[ debug,
        Print["NN: Case 6"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[neutral[a],"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@d]]]===StringTake[neutral@d,Min[5,StringLength[neutral@a],StringLength[neutral@d]]]||StringTake[StringReplace[neutral@a,"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@e]]]===StringTake[neutral@e,Min[5,StringLength[neutral@a],StringLength[neutral@e]]]||If[ neutral@f=!="",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      StringTake[StringReplace[neutral@a,"CH3"->"CH2"],Min[5,StringLength[neutral@a],StringLength[neutral@f]]]===StringTake[neutral@f,Min[5,StringLength[neutral@a],StringLength[neutral@f]]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      False
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ])];
    Prepend[rest,refline],
    (* CH3 -> CH2 aber complex bleibt erhalten *)
    refline[[2]]==="NN"&&StringLength[neutral@refline[[4]]]>=3&&StringTake[neutral[refline[[4]]],3]=="CH3"&&(If[ StringLength[neutral[refline[[6]]]]>2,
                                                                                                                 StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[6]]]]]]===StringTake[neutral[refline[[6]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[6]]]]]],
                                                                                                                 False
                                                                                                             ]||If[ StringLength[neutral[refline[[7]]]]>2,
                                                                                                                    StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[7]]]]]]===StringTake[neutral[refline[[7]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[7]]]]]],
                                                                                                                    False
                                                                                                                ]||If[ StringLength[neutral[refline[[8]]]]>2,
                                                                                                                       StringTake[StringReplacePart[neutral[refline[[4]]],"2",{3,3}],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[8]]]]]]===StringTake[neutral[refline[[8]]],Min[4,StringLength[neutral@refline[[4]]],StringLength[neutral[refline[[8]]]]]],
                                                                                                                       False
                                                                                                                   ]),
    If[ debug,
        Print["NN: Case 6b"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;(StringTake[StringReplace[neutral[b],"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@d]]]===StringTake[neutral@d,Min[5,StringLength[neutral@b],StringLength[neutral@d]]]||StringTake[StringReplace[neutral@b,"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@e]]]===StringTake[neutral@e,Min[5,StringLength[neutral@b],StringLength[neutral@e]]]||If[ neutral@f=!="",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      StringTake[StringReplace[neutral@b,"CH3"->"CH2"],Min[5,StringLength[neutral@b],StringLength[neutral@f]]]===StringTake[neutral@f,Min[5,StringLength[neutral@b],StringLength[neutral@f]]],
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      False
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ])];
    Prepend[rest,refline],
    (* keine vorherige Regel trifft zu - nur CO bindungen erhalten *)
    refline[[2]]==="NN"&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]==Total@countCOgroups@refline[[6;;8]],(* Problem : Bsp # 162, Zahl der Bindungen bleibt erhalten, aber trotzdem wechselt O Atom den Partner, von CO2 -> ... *)
    If[ debug,
        Print["NN: Case Subscript[X, iso-CO]"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&MatchQ[Abs@\[CapitalDelta]BX@{a,b,d,e},{_,_,_,_,0,_,_,_,_}],
                                                                                                  (Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})&&(* 13C18O bleibt zusammen *)Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}],
                                                                                                  True
                                                                                              ]&&If[ f==""&&Sort@countCOgroups@{a,b}!=Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==1&&MatchQ[Abs@\[CapitalDelta]BE@{a,b,d,e},{_,_,_,_,1,_,_,_,_}],
                                                                                                     If[ Implies[is18Oisotop@e,is18Oisotop@b],
                                                                                                         True,
                                                                                                         False
                                                                                                     ]&&If[ Implies[is13Cisotop@e,is13Cisotop@b],
                                                                                                            True,
                                                                                                            False
                                                                                                        ]&&If[ Implies[is13Cisotop@d,is13Cisotop@a],
                                                                                                               True,
                                                                                                               False
                                                                                                           ],
                                                                                                     True
                                                                                                 ]&&If[ f==""&&Sort@countCOgroups@{a,b}==Sort@countCOgroups@{d,e}&&findPartnerA@{a,b,d,e}==0&&MatchQ[Abs@\[CapitalDelta]AE@{a,b,d,e},{1,0,0,0,0,0,0,0,0}],
                                                                                                        is18Oisotop@a==is18Oisotop@e&&is18Oisotop@b==is18Oisotop@d&&If[ Implies[is13Cisotop@e,is13Cisotop@a],
                                                                                                                                                                        True,
                                                                                                                                                                        False
                                                                                                                                                                    ]&&If[ Implies[is13Cisotop@d,is13Cisotop@b],
                                                                                                                                                                           True,
                                                                                                                                                                           False
                                                                                                                                                                       ],
                                                                                                        True
                                                                                                    ]&&If[ f!="",
                                                                                                           (Or@@isC18Obound@{a,b,c}==Or@@isC18Obound@{d,e,f})&&(Or@@isCOisotop@{a,b,c}==Or@@isCOisotop@{d,e,f})&&(* 13C18O bleibt zusammen *)Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}],
                                                                                                           True
                                                                                                       ]
    ];
    Prepend[rest,refline],
    refline[[2]]==="NN"&&Or@@isCObound@refline[[3;;5]]&&Or@@isCObound@refline[[6;;8]]&&Total@countCOgroups@refline[[3;;5]]-Total@countCOgroups@refline[[6;;8]]==1,(* CO Bindungen vorhanden, allerdings weniger bei den Produkten *)
    If[ debug,
        Print["NN: Case Subscript[X, non-iso-CO] O-atom switches partner "]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;
    If[ f=="",
        If[ countCatoms@a==countCatoms@d,
            (is13Cisotop@a==is13Cisotop@d)&&(is13Cisotop@b==is13Cisotop@e),
            (is13Cisotop@a==is13Cisotop@e)&&(is13Cisotop@b==is13Cisotop@d)
        ],
        True
    ]&&If[ f=="",
           If[ countOatoms@a+1==countOatoms@d,
               If[ Implies[is18Oisotop@a,is18Oisotop@d],
                   True,
                   False
               ],
               True
           ]
       ]&&If[ (a==="He+"&&f==="He")||(a==="\[DoubleStruckCapitalH]+"&&f==="\[DoubleStruckCapitalH]"),
              If[ countOatoms@b==countOatoms@d,
                  is18Oisotop@b==is18Oisotop@d,
                  True
              ]&&If[ isCHnbound@b==isCHnbound@d,
                     isCHnisotop@b==isCHnisotop@d,
                     True
                 ]&&If[ isCHnbound@b==isCHnbound@e,
                        isCHnisotop@b==isCHnisotop@e,
                        True
                    ],
              True
          ]];
    Prepend[rest,refline],
    (* alle C atome links oder echts in einem Molekuel *)
    refline[[2]]==="NN"&&(Total@Sign@countCatoms@refline[[3;;5]]==1||Total@Sign@countCatoms@refline[[6;;9]]==1),
    If[ debug,
        Print["NN:Bundled C atoms - no action"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&If[ Or@@isCPbound@{a,b,c}&&Or@@isCPbound@{d,e,f,g},
                                                                                                                             Or@@isCPisotop@{a,b,c}==Or@@isCPisotop@{d,e,f,g},
                                                                                                                             True
                                                                                                                         ]&&If[ isCObound@a&&isCObound@d&&(isCObound@b==isCObound@e==False)&&(\[CapitalDelta]AD@refline[[{3,4,6,7}]])[[{3,5}]]=={0,-1},
                                                                                                                                If[ Implies[isC18Obound@a,isC18Obound@d],
                                                                                                                                    True,
                                                                                                                                    False
                                                                                                                                ],
                                                                                                                                True
                                                                                                                            ]];
    Prepend[rest,refline],
    (* C atome in 4 oder mehr Reaktanden *)
    refline[[2]]==="NN"&&Total@Sign@countCatoms@refline[[3;;9]]>=4&&(findPartnerA@refline[[{3,4,6,7}]]!=0||findPartnerB@refline[[{3,4,6,7}]]!=0),
    If[ debug,
        Print["NN:C atoms everywhere"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&sticky13C@{a,b,d,e}&&If[ findPartnerB@refline[[{3,4,6,7}]]==1,
                                                                                                                                                  If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]>0(* C Abgabe von B an E *),
                                                                                                                                                      If[ Implies[is13Cisotop@d,is13Cisotop@b],
                                                                                                                                                          True,
                                                                                                                                                          False
                                                                                                                                                      ],
                                                                                                                                                      True
                                                                                                                                                  ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]<0(* C Abgabe von B an E *),
                                                                                                                                                         If[ Implies[is13Cisotop@b,is13Cisotop@d],
                                                                                                                                                             True,
                                                                                                                                                             False
                                                                                                                                                         ],
                                                                                                                                                         True
                                                                                                                                                     ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]==0(* keine C Abgabe von B an E *),
                                                                                                                                                            is13Cisotop@b==is13Cisotop@d&&is13Cisotop@a==is13Cisotop@e,
                                                                                                                                                            True
                                                                                                                                                        ],(*findPartnerB==2*)
                                                                                                                                                  If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]>0(* C Abgabe von B an D *),
                                                                                                                                                      If[ Implies[is13Cisotop@e,is13Cisotop@b],
                                                                                                                                                          True,
                                                                                                                                                          False
                                                                                                                                                      ],
                                                                                                                                                      True
                                                                                                                                                  ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]<0(* C Abgabe von B an D *),
                                                                                                                                                         If[ Implies[is13Cisotop@b,is13Cisotop@e],
                                                                                                                                                             True,
                                                                                                                                                             False
                                                                                                                                                         ],
                                                                                                                                                         True
                                                                                                                                                     ]&&If[ (\[CapitalDelta]BX@refline[[{3,4,6,7}]])[[3]]==0(* keine C Abgabe von B an D *),
                                                                                                                                                            is13Cisotop@b==is13Cisotop@e&&is13Cisotop@a==is13Cisotop@d,
                                                                                                                                                            True
                                                                                                                                                        ]
                                                                                                                                              ]];
    Prepend[rest,refline],
    (* Find Partner findet keine Partner  *)
    refline[[2]]==="NN"&&refline[[8]]==""&&findPartnerA@refline[[{3,4,6,7}]]===0,
    If[ debug,
        Print["NN: Letzter Filter"]
    ];
    rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky18O@{a,b,c,d,e,f,g}&&sticky13C@{a,b,d,e}&&If[ \[CapitalDelta]AE@refline[[{3,4,6,7}]]=={-3,0,-1,0,0,0,0,0,0},
                                                                                                                                                  If[ countCatoms@refline[[3]]>0&&countCatoms@refline[[7]]>1,
                                                                                                                                                      If[ Implies[is13Cisotop@a,is13Cisotop@e],
                                                                                                                                                          True,
                                                                                                                                                          False
                                                                                                                                                      ],
                                                                                                                                                      True
                                                                                                                                                  ],
                                                                                                                                                  True
                                                                                                                                              ]];
    Prepend[rest,refline],
    True,list]
]

(* -------------------------------------------------------------------------------------------------------------------------------*)
cleanUpCL[list_List] :=
    Module[ {rest,refline,debug},
        debug = False;
        refline = list[[1]];
        rest = Rest[list];
        Which[
        refline[[2]]==="CL",(*reine Photoionization*)
        If[ debug,
            Print["Photo: restructuring ionization"]
        ];
        rest = Cases[rest,{n_,t_,a_,b_,c_,d_,e_,f_,g_,h_,i_,j_,k__}:>{n,t,a,b,c,d,e,f,g,h,i,j,k}/;sticky13C@{a,b,c,d,e,f,g}&&sticky18O@{a,b,c,d,e,f,g}&&(* 13C18O bleibt zusammen *)Or@@MapThread[And,{isCOisotop@{a,b,c},isC18Obound@{a,b,c}}]==Or@@MapThread[And,{isCOisotop@{d,e,f},isC18Obound@{d,e,f}}]];
        Prepend[rest,refline],
        True,If[ debug,
                 Print["CL: No match"]
             ];
             list]
    ]
(* -------------------------------------------------------------------------------------------------------------------------------*)    
cleanUpMN[list_List] := Module[{rest, refline, debug},
  debug = True;
  refline = list[[1]];
  rest = Rest[list];
  Which[
   (*MN:mutual neutralisation => 
   isotopomere links und rechts duerfen den platz nicht tauschen
   nur Elektronen-Austausch*)
   
   refline[[2]] === "MN" && 
     refline[[8]] == 
      "" && (neutral@refline[[3]] == neutral@refline[[7]] && 
       neutral@refline[[4]] == neutral@refline[[6]]) || (neutral@
        refline[[3]] == neutral@refline[[6]] && 
      neutral@refline[[4]] == neutral@refline[[7]]),
   If[debug, Print["MN:Case 1"]];
   rest = 
    Cases[rest, {n_, t_, a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, 
       k__} :> {n, t, a, b, c, d, e, f, g, h, i, j, 
        k} /; (If[isIon[a], StringDrop[a, -1], a] == 
           If[isIon[d], StringDrop[d, -1], d] || 
          If[isIon[a], StringDrop[a, -1], a] == 
           If[isIon[e], StringDrop[e, -1], e]) && (If[isIon[b], 
            StringDrop[b, -1], b] == 
           If[isIon[d], StringDrop[d, -1], d] || 
          If[isIon[b], StringDrop[b, -1], b] == 
           If[isIon[e], StringDrop[e, -1], e])];
   Prepend[rest, refline],
   
   refline[[2]] === "MN" && 
    refline[[8]] == 
     "" && (countHCNOS@refline[[3]] == countHCNOS@refline[[6]] || 
      countHCNOS@refline[[3]] == countHCNOS@refline[[7]]), 
   If[debug, Print["MN:Case 2 - restructuring"]];
   rest = 
    Cases[rest, {n_, t_, a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, 
       k__} :> {n, t, a, b, c, d, e, f, g, h, i, j, k} /; 
       sticky18O@{a, b, c, d, e, f, g} && 
        sticky13C@{a, b, c, d, e, f, g} && 
        If[countHCNOS@refline[[3]] == countHCNOS@refline[[6]], 
         If[isCObound@b == isCObound@e, isCOisotop@b == isCOisotop@e, 
           True] && 
          If[isCObound@a == isCObound@d, isCOisotop@a == isCOisotop@d,
            True] && 
          If[isCHnbound@b == isCHnbound@e, 
           isCHnisotop@b == isCHnisotop@e, True] && 
          If[isCHnbound@a == isCHnbound@d, 
           isCHnisotop@a == isCHnisotop@d, True] &&
          is13Cisotop@a == is13Cisotop@d &&
          is13Cisotop@b == is13Cisotop@e &&
          is18Oisotop@a == is18Oisotop@d &&
          is18Oisotop@b == is18Oisotop@e,
         If[isCObound@b == isCObound@d, isCOisotop@b == isCOisotop@d, 
           True] && 
          If[isCObound@a == isCObound@e, isCOisotop@a == isCOisotop@e,
            True] && 
          If[isCHnbound@b == isCHnbound@d, 
           isCHnisotop@b == isCHnisotop@d, True] && 
          If[isCHnbound@a == isCHnbound@e, 
           isCHnisotop@a == isCHnisotop@e, True] &&
          is13Cisotop@a == is13Cisotop@e &&
          is13Cisotop@b == is13Cisotop@d &&
          is18Oisotop@a == is18Oisotop@e &&
          is18Oisotop@b == is18Oisotop@d](*&&If[countNatoms@b==
     countNatoms@e>0&&countCatoms@b==countCatoms@e>0&&isCNbound@e,If[
     Implies[isCNbound@e,is13Cisotop@b],True,False],True]*)];
   Prepend[rest, refline],
   
   refline[[2]] === "MN" && 
    refline[[8]] == 
     "" && (countHCNOS@refline[[3]] != countHCNOS@refline[[6]] || 
      countHCNOS@refline[[3]] != countHCNOS@refline[[7]]), 
   If[debug, Print["MN:Case 3 - dissociation"]];
   rest = 
    Cases[rest, {n_, t_, a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, 
       k__} :> {n, t, a, b, c, d, e, f, g, h, i, j, k} /; 
       sticky18O@{a, b, c, d, e, f, g} && 
        sticky13C@{a, b, c, d, e, f, g}(*&&If[countNatoms@b==
     countNatoms@e>0&&countCatoms@b==countCatoms@e>0&&isCNbound@e,If[
     Implies[isCNbound@e,is13Cisotop@b],True,False],True]*)];
   Prepend[rest, refline],
   
   refline[[2]] === "MN" && refline[[8]] != "",
   If[debug, Print["MN:Case 4 - dissociative recombination"]];
   rest = 
    Cases[rest, {n_, t_, a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, 
       k__} :> {n, t, a, b, c, d, e, f, g, h, i, j, 
        k} /; (neutral@a == neutral@d && 
          countHCNOS@b == (countHCNOS@e + countHCNOS@f)) ||
        (neutral@b == neutral@d && 
          countHCNOS@a == (countHCNOS@e + countHCNOS@f)) ||
        (neutral@a == neutral@e && 
          countHCNOS@b == (countHCNOS@d + countHCNOS@f)) ||
        (neutral@a == neutral@f && 
          countHCNOS@b == (countHCNOS@d + countHCNOS@e)) || (neutral@
            b == neutral@e && 
          countHCNOS@a == (countHCNOS@d + countHCNOS@f)) ||
        (neutral@b == neutral@f && 
          countHCNOS@a == (countHCNOS@d + countHCNOS@e))
     ];
   Prepend[rest, refline],
   
   True, If[debug, Print["MN: unidentified case"]]; list]]
 
 InsertIsotopes[list_] :=
 Block[{updatedData, newData2, showR, len, data, monitorCell,reactnumbers},
  updatedData = 
   Map[Flatten@{#[[{1, 2}]], 
       StringReplace[#[[3 ;; 9]], norm2doubleStruck], #[[
        10 ;; Length@#]]} &, list];
  (* Einfuegen der Isotope *)
  newData2 = Sort[in13C18O[updatedData]];
  len = Length[newData2];
  showR[num_] := 
   Select[newData2, MemberQ[{num}, #[[1]]] &] // Sort // Reverse;
  monitorCell[i_] := 
   CellPrint@
    Cell[BoxData[
      FrameBox[
       StyleBox[
        ToBoxes@StringForm[
          "Processing entry: `1` of " <> ToString[len], i], 
        FontFamily -> "Verdana", FontSize -> 11, 
        FontColor -> RGBColor[0.2, 0.4, 0.6]], 
       Background -> RGBColor[0.96, 0.98, 1.], 
       FrameMargins -> {{24, 24}, {8, 8}}, 
       FrameStyle -> RGBColor[0.2, 0.4, 0.6], StripOnInput -> False]],
      "PrintTemporary"];
  log = {};
  Monitor[data = DeleteCases[
     Table[
      If[showR@i != {},(*to catch missing reactions*)
       Which[
        showR[i][[1, 2]] == "NN", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpNN@showR@i),
        showR[i][[1, 2]] == "MN", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpMN@showR@i),
        showR[i][[1, 2]] == "IN", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpIN@showR@i),
        showR[i][[1, 2]] == "RA", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpRA@showR@i),
        showR[i][[1, 2]] == "AD", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpAD@showR@i),
        showR[i][[1, 2]] == "DR", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpDRV2@showR@i),
        showR[i][[1, 2]] == "CE", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpCE@showR@i),
        showR[i][[1, 2]] == "CL", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpCL@showR@i),
        showR[i][[1, 2]] == "PH" || showR[i][[1, 2]] == "CR" || 
         showR[i][[1, 2]] == "CP", 
        log = {ToString@showR[i][[1, 1 ;; 2]], log};
        Sequence @@ (cleanUpPhoto@showR@i),
        True, 
        Print["Unknown reaction " <> ToString@showR[i][[1, 1 ;; 2]]];
        log = {"Unknown reaction " <> ToString@showR[i][[1, 1 ;; 2]], 
          log};
        Sequence @@ (showR@i)], 
       Print["Missing reaction " <> ToString@i];
       log = {"Missing reaction " <> ToString@i, log};], {i, newData2[[All, 1]] // Union}]
     , Null], ProgressIndicator[i, {1, Length[list]}]];
  log = Reverse[Flatten[log]];
  Map[Flatten@{#[[{1, 2}]], 
      StringReplace[#[[3 ;; 9]], doubleStruck2norm], #[[
       10 ;; Length@#]]} &, data]
  ]

rescaleRates[list_] := 
 Module[{tstSplt, tab, rpl, cleanDatabase = list},
  (* Reskalierung der Reaktionsraten *)
  tstSplt = 
   Split[cleanDatabase, #1[[1 ;; 5]] === #2[[1 ;; 5]] &] // MatrixForm;
   (* !! Funktioniert aus irgendeinem Grund nur mit MatrixForm *)
  tab = Table[tstSplt[[1, i]] // Length, {i, 1, Length[tstSplt[[1]]]}];
  rpl = MapThread[#1[[All, 10]]/#2 &, {tstSplt[[1, All]], tab}] // 
    Flatten; 
  cleanDatabase[[All, 10]] = cleanDatabase[[#, 10]] & @@@ rpl;
  Map[Flatten@{#[[{1, 2}]], 
      StringReplace[#[[3 ;; 9]], doubleStruck2norm], #[[
       10 ;; Length@#]]} &, cleanDatabase]]
          
fromJ[s_String] /; StringTake[s, 1] == "J" := StringDrop[s, 1]
fromJ[s_String] := s
toJ[a_String] /; StringTake[a, 1] != "J" := "J" <> a
toJ[a_String] := a


   
End[]
EndPackage[]