(* Content-type: application/vnd.wolfram.mathematica *)

(*** Wolfram Notebook File ***)
(* http://www.wolfram.com/nb *)

(* CreatedBy='Mathematica 13.3' *)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[       158,          7]
NotebookDataLength[     10225,        255]
NotebookOptionsPosition[      9869,        241]
NotebookOutlinePosition[     10270,        257]
CellTagsIndexPosition[     10227,        254]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 RowBox[{
  RowBox[{"(*", 
   RowBox[{"File", ":", 
    RowBox[{"MonteCarloPiModul", ".", "m"}]}], "*)"}], "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{
    RowBox[{"BeginPackage", "[", "\"\<MonteCarloPiModul`\>\"", "]"}], ";"}], 
   "\n", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"MonteCarloPi", "::", "uporaba"}], "=", 
     "\"\<MonteCarloPi[] Oceni vrednost Pi z uporabu Monte Carlo metode, za \
uporabo ni potreben noben argument\>\""}], ";"}], "\n", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"Begin", "[", "\"\<`Private`\>\"", "]"}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{"Funkcija", ",", " ", 
     RowBox[{
     "ki", " ", "preveri", " ", "\[CHacek]e", " ", "je", " ", "to\[CHacek]ka",
       " ", "znotraj", " ", "ali", " ", "zunaj", " ", "enotske", " ", 
      "kro\[ZHacek]nice"}]}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"ZnotrajKroga", "[", 
      RowBox[{"{", 
       RowBox[{"x_", ",", "y_"}], "}"}], "]"}], ":=", 
     RowBox[{
      RowBox[{
       RowBox[{"x", "^", "2"}], "+", 
       RowBox[{"y", "^", "2"}]}], "<=", "1"}]}], ";"}], "\n", 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{"Funkcija", ",", " ", 
     RowBox[{
     "ki", " ", "izri\[SHacek]e", " ", "to\[CHacek]ke", " ", "in", " ", 
      "enotsko", " ", "kro\[ZHacek]nico"}]}], "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"IzrisTockinKroga", "[", 
      RowBox[{"tocke_", ",", "tockeNotri_"}], "]"}], ":=", 
     "\[IndentingNewLine]", 
     RowBox[{"ListPlot", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
         RowBox[{
          RowBox[{
           RowBox[{"Tooltip", "[", 
            RowBox[{"#", ",", "\"\<To\[CHacek]ka zunaj kroga\>\""}], "]"}], 
           "&"}], "/@", 
          RowBox[{"Complement", "[", 
           RowBox[{"tocke", ",", "tockeNotri"}], "]"}]}], ",", 
         RowBox[{
          RowBox[{
           RowBox[{"Tooltip", "[", 
            RowBox[{"#", ",", "\"\<To\[CHacek]ka znotraj kroga\>\""}], "]"}], 
           "&"}], "/@", "tockeNotri"}]}], "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"PlotStyle", "->", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"PointSize", "[", "0.01", "]"}], ",", "Red"}], "}"}], ",", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"PointSize", "[", "0.01", "]"}], ",", "Blue"}], "}"}]}], 
         "}"}]}], ",", "\[IndentingNewLine]", 
       RowBox[{"AspectRatio", "->", "1"}], ",", "\[IndentingNewLine]", 
       RowBox[{"PlotRange", "->", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"-", "1.1"}], ",", "1.1"}], "}"}], ",", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"-", "1.1"}], ",", "1.1"}], "}"}]}], "}"}]}], ",", 
       "\[IndentingNewLine]", 
       RowBox[{"PlotLegends", "->", 
        RowBox[{"{", 
         RowBox[{
         "\"\<To\[CHacek]ke zunaj kroga\>\"", ",", 
          "\"\<To\[CHacek]ke znotraj kroga\>\""}], "}"}]}], ",", 
       "\[IndentingNewLine]", 
       RowBox[{"ImageSize", "->", "500"}]}], "]"}]}], ";"}], " ", 
   RowBox[{"(*", 
    RowBox[{"Prilagoditev", " ", "velikosti", " ", "izrisa"}], "*)"}], 
   "\[IndentingNewLine]", "\n", 
   RowBox[{"(*", 
    RowBox[{
     RowBox[{"Glavna", " ", "funkcija"}], ",", " ", 
     RowBox[{
     "ki", " ", "oceni", " ", "vrednost", " ", "Pi", " ", "s", " ", 
      "pomo\[CHacek]jo", " ", "Monte", " ", "Carlo", " ", "metode"}]}], 
    "*)"}], "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"MonteCarloPi", "[", "]"}], ":=", 
     RowBox[{"DynamicModule", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{
        "tocke", ",", "tockeNotri", ",", "ocenaPi", ",", "plot", ",", 
         "krog"}], "}"}], ",", "\[IndentingNewLine]", 
       RowBox[{"Manipulate", "[", "\[IndentingNewLine]", 
        RowBox[{"(*", 
         RowBox[{
         "Generiranje", " ", "naklju\[CHacek]nih", " ", "to\[CHacek]k", " ", 
          "znotraj", " ", "enotskega", " ", "kvadrata"}], "*)"}], 
        "\[IndentingNewLine]", 
        RowBox[{
         RowBox[{
          RowBox[{"tocke", "=", 
           RowBox[{"RandomReal", "[", 
            RowBox[{
             RowBox[{"{", 
              RowBox[{
               RowBox[{"-", "1"}], ",", "1"}], "}"}], ",", 
             RowBox[{"{", 
              RowBox[{"n", ",", "2"}], "}"}]}], "]"}]}], ";", 
          "\[IndentingNewLine]", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
            RowBox[{"Izbira", " ", "to\[CHacek]k"}], ",", " ", 
            RowBox[{
            "ki", " ", "so", " ", "znotraj", " ", "kro\[ZHacek]nice"}]}], 
           "*)"}], "\[IndentingNewLine]", 
          RowBox[{"tockeNotri", "=", 
           RowBox[{"Select", "[", 
            RowBox[{"tocke", ",", "ZnotrajKroga"}], "]"}]}], ";", 
          "\[IndentingNewLine]", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{"Ocena", " ", "\[SHacek]tevila", " ", "Pi"}], "*)"}], 
          "\[IndentingNewLine]", 
          RowBox[{"ocenaPi", "=", 
           RowBox[{"N", "[", 
            RowBox[{"4", " ", 
             RowBox[{
              RowBox[{"Length", "[", "tockeNotri", "]"}], "/", "n"}]}], 
            "]"}]}], ";", "\[IndentingNewLine]", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{"Plot", " ", "to\[CHacek]k"}], "*)"}], 
          "\[IndentingNewLine]", 
          RowBox[{"plot", "=", 
           RowBox[{"IzrisTockinKroga", "[", 
            RowBox[{"tocke", ",", "tockeNotri"}], "]"}]}], ";", 
          "\[IndentingNewLine]", "\[IndentingNewLine]", 
          RowBox[{"(*", "Kro\[ZHacek]nica", "*)"}], "\[IndentingNewLine]", 
          RowBox[{"krog", "=", 
           RowBox[{"Graphics", "[", 
            RowBox[{"{", 
             RowBox[{"Thick", ",", 
              RowBox[{"Circle", "[", "]"}]}], "}"}], "]"}]}], ";", 
          "\[IndentingNewLine]", "\[IndentingNewLine]", 
          RowBox[{"(*", 
           RowBox[{
           "Poka\[ZHacek]e", " ", "plot", " ", "z", " ", 
            "razdeljujo\[CHacek]o", " ", "kro\[ZHacek]nico", " ", "in", " ", 
            "oceno", " ", "\[SHacek]tevila", " ", "Pi"}], "*)"}], 
          "\[IndentingNewLine]", 
          RowBox[{"Column", "[", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Row", "[", 
              RowBox[{"{", 
               RowBox[{"\"\<Ocena \[SHacek]tevila Pi: \>\"", ",", "ocenaPi"}],
                "}"}], "]"}], ",", "\[IndentingNewLine]", 
             RowBox[{"Show", "[", 
              RowBox[{"plot", ",", "krog"}], "]"}]}], "}"}], "]"}]}], ",", 
         "\[IndentingNewLine]", "\[IndentingNewLine]", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
            "n", ",", "100", ",", 
             "\"\<\[CapitalSHacek]tevilo to\[CHacek]k\>\""}], "}"}], ",", 
           "100", ",", "4000", ",", "390", ",", 
           RowBox[{"Appearance", "->", "\"\<Labeled\>\""}]}], "}"}], ",", 
         "\[IndentingNewLine]", "\[IndentingNewLine]", 
         RowBox[{"TrackedSymbols", ":>", 
          RowBox[{"{", "n", "}"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{"SynchronousUpdating", "->", "False"}], ",", 
         RowBox[{"(*", 
          RowBox[{
           RowBox[{"Izklop", " ", "sinhronega", " ", "updatanja"}], " ", "->",
            " ", 
           RowBox[{"manj", " ", "lagganja"}]}], "*)"}], "\[IndentingNewLine]", 
         RowBox[{"ContinuousAction", "->", "False"}], ",", 
         RowBox[{"(*", 
          RowBox[{
           RowBox[{"Updataj", " ", "sliko", " ", "samo"}], ",", " ", 
           RowBox[{
            RowBox[{"ko", " ", "spusti\[SHacek]", " ", "drsnik"}], " ", "->", 
            " ", 
            RowBox[{"manj", " ", "lagganja"}]}]}], "*)"}], 
         "\[IndentingNewLine]", 
         RowBox[{"ControlPlacement", "->", "Top"}]}], " ", 
        RowBox[{"(*", 
         RowBox[{"Drsnik", " ", "nad", " ", "grafom"}], "*)"}], "]"}]}], 
      "]"}]}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"End", "[", "]"}], ";"}], "\[IndentingNewLine]", "\n", 
   RowBox[{
    RowBox[{"EndPackage", "[", "]"}], ";"}], "\n"}]}]], "Input",
 CellChangeTimes->{{3.9087319875242863`*^9, 3.908731987526286*^9}, {
  3.9087320850194216`*^9, 3.908732097363307*^9}, {3.9087321341046295`*^9, 
  3.9087321378982043`*^9}, {3.908732245492408*^9, 3.9087322498667808`*^9}, {
  3.908732282739236*^9, 3.908732284919464*^9}, {3.908732341145464*^9, 
  3.9087324675263042`*^9}, {3.9087325088902884`*^9, 3.908732521302972*^9}, {
  3.9087325583695927`*^9, 3.90873259831227*^9}, {3.90873264568381*^9, 
  3.9087326457161627`*^9}, {3.9087326761321077`*^9, 3.9087326761406174`*^9}, {
  3.9087327087610765`*^9, 3.9087327802567058`*^9}, {3.9087328611739674`*^9, 
  3.9087328916623697`*^9}, {3.908732927944966*^9, 3.9087330498480806`*^9}, {
  3.908733083096327*^9, 3.908733141405278*^9}, {3.908733191797392*^9, 
  3.9087332037344027`*^9}, {3.9087332355022564`*^9, 3.9087335874118085`*^9}, {
  3.9087336682331467`*^9, 3.9087336771911125`*^9}, {3.9087338941109395`*^9, 
  3.9087338963501625`*^9}},
 CellLabel->"In[1]:=",ExpressionUUID->"2b0015b1-d664-46f2-bc6f-b24e632a3e39"]
},
WindowSize->{1440, 741.75},
WindowMargins->{{-6, Automatic}, {Automatic, -6}},
FrontEndVersion->"13.3 for Microsoft Windows (64-bit) (June 3, 2023)",
StyleDefinitions->"Default.nb",
ExpressionUUID->"ff8ca58d-fcbc-4839-9bd9-f3fcd5a5b8b6"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[558, 20, 9307, 219, 1064, "Input",ExpressionUUID->"2b0015b1-d664-46f2-bc6f-b24e632a3e39"]
}
]
*)

