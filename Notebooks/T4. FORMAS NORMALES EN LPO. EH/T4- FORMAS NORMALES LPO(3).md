---
elm:
  source-directories:
    - ../../elm-sources/src
  dependencies:
      elm/browser: latest
      elm/core: latest
      elm/file: latest
      elm/html: latest
      elm/parser: latest
      elm-community/graph: latest
      elm-community/list-extra: latest
      elm-community/maybe-extra: latest
      munksgaard/char-extra: latest
      gicentre/elm-vegalite: latest
      avh4/elm-fifo: latest
      elm/bytes: latest
      elm/json: latest
      elm/time: latest
      elm/url: latest
      elm/virtual-dom: latest
      elm-community/intdict: latest
      Chadtech/elm-bool-extra: latest
---

<!-- Cargamos algunas funciones js que necesitamos -->

<script src="../js/d3.min.js"></script>
<script src="../js/viz.js" type="javascript/worker"></script>
<script src="../js/d3-graphviz.js"></script>
<script type="text/javascript">

    function plotDot(graphId){
        d3.select('#svgContainer' + graphId)
        .graphviz()
        .renderDot(document.getElementById("inDOT" + graphId).value.replace(/\\n/g, "\n").replace(/\\"/gm, '"'));

        document.getElementById("outputGraph" + graphId).style.display = "inline-block";
        document.getElementById("inputGraph" + graphId).style.display = "none";
    };

    function generatePng(graphId){
        var svgElement = document.getElementById('svgContainer' + graphId).querySelector('svg');
        var svgString = new XMLSerializer().serializeToString(svgElement);
        var canvas = document.getElementById("canvas" + graphId);
        canvas.width = svgElement.getBBox().width*1.33;
        canvas.height = svgElement.getBBox().height*1.33;
        var ctx = canvas.getContext("2d");
        var DOMURL = self.URL || self.webkitURL || self;
        var img = new Image();
        var svg = new Blob([svgString], {type: "image/svg+xml;charset=utf-8"});
        var url = DOMURL.createObjectURL(svg);
        img.onload = function() {
            ctx.drawImage(img, 0, 0);
            var png = canvas.toDataURL("image/png");

            document.querySelector("#pngContainer" + graphId).innerHTML = '<a id="downloadPNG'+ graphId +'" href="'+png+'" download/> PNG Image </a>';
            imgdownload = document.getElementById("downloadPNG"+ graphId)
            imgdownload.click();
            DOMURL.revokeObjectURL(png);
        };
        img.src = url;
    };
</script>


## **TEMA 4** - PARTE 3
Formas Normales en LPO
====================

4.4. De *LPO* a *LP*. Teorema de Herdbrand
----------------------------

En este apartado vamos a exponer cómo podemos reducir la consistencia de un conjunto de fórmulas en un lenguaje de primer orden a la de un conjunto de fórmulas proposicionales.

Para ello vamos a ver los trabajos que, en esta dirección, realizó el matemático FRANCÉS *Jaques Herdbrand*, quien propuso un método de verificación de la consistencia de un conjunto de fórmulas en LPO a través de la verificación de la consistencia de un conjunto de fórmulas en LP.

Pero para poder llegar hasta ese punto, en la conocida como *Extensión de Herdbrand*, hemos de ver una serie de definiciones y conceptos previos.

### 4.4.1. Universo, Interpretaciones y Modelos de Herdbrand.

Sea $L$ un lenguaje de Primer Orden.

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic;">
<u> Definiciones</u><br> <br>

Sea $L$ un lenguaje de primer orden:

- Se denomina **signatura** de una fórmula, $F$, de $L$ a una terna formada por el conjunto de constantes que participan en $F$, $cs_F$, el conjunto de símbolos de función que participan en $F$ junto a su aridad, $fs_F$, y el conjunto de símbolos de predeicado del lenguaje junto a su aridad, $ps_F$. De forma que $$signatura(F) = (cs_F, fs_F, ps_F) = \left( \{a, b, \ldots\}, \{(f_1, a_1), (f_2, a_2), \ldots \},  \{(P_1, a'_1), (P_2, a'_2),  \ldots \}\right)$$<br>

- El **Universo de Herbrand**, $UH(L)$, de un lenguaje de primer orden, $L$, corresponde al conjunto de todos los posibles términos cerrados de $L$. De manera que corresponde al conjunto de constantes de $L$ (si fuera vacío se tomaría un conjunto con una única constante, $\{a\}$) y la aplicación sucesiva del conjunto de funciones, según la aridad de las mismas.<br>
De forma que basta que el conjunto de funciones no sea vacío para que el $UH(L)$ sea infinito. De forma que podemos definir una versión acotada de $UH(L)$ a través del **Universo de Herbrand de nivel $n$**, $UH_n(L)$, de forma que:<br>$U_0(L) = \left\lbrace \begin{array}{l l} cs & si \, cs \neq \emptyset \\ \{a\} & e.o.c.\end{array}\right.$ <br>
$U_n(L) = U_{n-1}(L)  \, \cup \bigcup \limits_{i=1}^{|fs|} \{f_i (x_1, \ldots, x_m) / (x_1, \ldots, x_m) \in (U_{n-1}(L))^{m}; \; m = aridad(f_i)\}$<br><br>

- La **Base de Herbrand**, $BH(L)$, de un lenguaje de primer orden, $L$, corresponde al conjunto de todos los posibles átomos (predicados) cerrados de $L$.De manera que corresponde a la aplicación de los símbolos de predicado, según la aridad, sobre $UH(L)$.<br>
De forma analoga a $UH_n(L)$, se define la **Base de Herbrand de nivel $n$**, $BH_n(L)$<br> de forma que:<br> $BH_n(L) = \bigcup \limits_{i=1}^{|ps|} \{p_i (x_1, \ldots, x_m) / (x_1, \ldots, x_m) \in (UH_n(L))^{m}; \; m = aridad(P_i)\}$<br><br>

- Una $L$-estructura, $M$, es una **Estructura de Herbrand**, de $L$ tal que:
    1. El universo de $M$ corresponde a $UH(L)$.
    2. La interpretación de toda constante y función es un término que corresponde a ella misma.
<br>

- Una **Interpretación de Herdbrand**, $IH(L)$, de $L$ corresponde a un subconjunto de la base de Herdbrand, que representa los predicados considerados verdaderos. Los no presentes en dicho subconjunto son considerados falsos. De forma analoga se define la **Interpretación de Herbrand de nivel $n$**, $IH_n(L)$, de forma que:<br> $IH_n(L) \subseteq BH_n(L))\}$<br><br>

- Un **Modelo de Herdbrand** de un conjunto $\Gamma$ de fórmulas de $L$ es una estructura de Herdbrand de $L$ que es modelo de $\Gamma$. De forma que con el uso de los Modelos de Herbrand
podemos intentar generar modelos para conjuntos de fórmulas LPO, cosa que hasta ahora no habíamos presentado.<br><br>

- <u> Teorema 4.5.</u> Si un conjunto de fórmulas abiertas (o, equivalentemente cláusulas) es consistente, entonces posee un modelo de Herdbrand, además numerable. <br><br> <u> Corolario </u>  Un conjunto de fórmulas abiertas (o, equivalentemente cláusulas) es inconsistente si y solo si no posee ningún modelo de Herdbrand. <br><br>

- Sea $\Sigma$ un conjunto de fórmulas abiertas (o equivalentemente, cláusulas), la **Extensión de Herdbrand** de $\Sigma$, $EH(\Sigma)$, corresponde a un conjunto formato por todas las fórmulas cerradas (y son también abiertas. que pueden obtenerse sustituyendo, de todas las formas posibles, las variables de las fórmulas de $\Sigma$ por términos cerrados del $UH(L)$. <br><br> Como anteriormente hemos venido presentando, se puede definir **Extensión de Herdbrand de nivel $n$** de forma análoga pero tomando los términos de $UH_n(L)$.

- <u> Teorema 4.6.</u> **Teorema de Herdbrand** - Sea $\Sigma$ un conjunto de fórmulas abiertas (o equivalentemente, cláusulas), resultan equivalentes:<br><br>
    1. $\Sigma$ tiene un modelo (es consistente).
    2. $EH(\Sigma)$ tiene un modelo.
    3. $EH(\Sigma)$ es satisfactible proposicionalmente. Es decir, si identificamos cada fórmula $EH(\Sigma)$ con su esqueleto proposicional, entonces el conjunto de fórmulas proposicionales es satisfactible.<br><br>

    De forma que:
    - Este resultado reduce la consistencia de un conjunto de fórmulas LPO (o de una Forma de Skolem de dicho conjunto) a la consistencia de un conjunto de fórmulas proposicionales.

    - $EH(\Sigma)$ puede ser infinito, pero se tiene,
        <u>Teorema 4.7.</u> (Compacidad). Si $EH(\Sigma)$ es inconsistente, entonces existe un subconjunto finito de $EH(\Sigma)$ que es inconsistente, esto es existe $n \in \mathbb{N}$ tal que $EH_n(\Sigma)$ es inconsistente
</div><br>


De forma que bajo el uso conjunto de la *Forma de Skolem* y la *Extensión de Herdbrand* podemos lograr el objetivo marcado al inicio del tema.

Veámos para finalizar algún ejemplo del trabajo con *Herdbrand*.

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black; font-style:italic;">
<b><i><u>Ejemplo 4.6.</u></i></b><br><br>

Dada el conjunto de fórmulas abiertas $\Sigma \equiv \{P(x) \rightarrow P(f(f(x))), \neg P(f(f(f(x)))), P(f(a))\} $, demostrar que es inconsitente, haciendo uso del Teorema de Herdbrand. <br>
</div><br>

Teniendo en cuenta que si $\Sigma$ es inconsistente entonces existe un subconjunto finito de $EH(\Sigma)$ tambien inconsistente, entonces basta ir comprobando, de manera incremental, si $EH_n(\Sigma)$ es inconsistente, o bien intentar buscar dicho subconjunto.

Podemos expresar el el conjunto $\Sigma$ como un conjunto de cláusulas: $$\{\{\neg P(x), P(f(f(x)))\}, \{\neg P(f(f(f(x))))\}, \{P(f(a))\}\}$$

Tomemos un lenguaje, $L$ formado por las constantes, funciones y predicados presentes en el conjunto. Dado que se tiene un símbolo de función entonces, $UH(L)$ es infinito y correspondería a: $a, f(a), f(f(a)), f(f(f(a))), \ldots$, de forma que la extensión de Herdbrand correspondería a:

1. $EH_0(\Sigma)$ $$\{\{\neg P(a), P(f(f(a)))\}, \{\neg P(f(f(f(a))))\}, \{P(f(a))\}\}$$ que es claramente consistente.

1. $EH_1(\Sigma) = EH_0 \, \cup$ $$\{\{\neg P(f(a)), P(f(f(f(a))))\}, \{\neg P(f(f(f(f(a)))))\}\}$$ que ya es claramente inconsistente. ($\{\{\neg P(f(a)), P(f(f(f(a))))\}, \{\neg P(f(f(f(a))))\}, \{P(f(a))\}\} \subset EH_1(\Sigma)$ es claramenten inconsistente)
 </div><br>

 <div style="text-align:justify; padding:0.3cm; background-color:lightgray">
 <i><b><u>Herdbrand en Logicus</i></b></u><br><br>

 -El cálculo de la signatura de una fórmula ABIERTA está implementado en el módulo *HerbrandLPO*, a través de la función *signatureFormulaLPO* o de un conjunto de fórmulas a través de *signatureLPOSet*

 -El cálculo del *Universo de Herbrand de nivel $n$* está implementado en el módulo *HerbrandLPO*, a través de la funciones *signatureHerbrandUniverse* (dada una signatura), *formulaLPOHerbrandUniverse* (dada una formula ABIERTA), *lpoSetHerbrandUniverse* (dado un conjunto de fórmulas ABIERTAS) .

-El cálculo de la *Base de Herbrand de nivel $n$* está implementado en el módulo *HerbrandLPO*, a través de la funciones *signatureHerbrandBase, formulaLPOHerbrandBase, lpoSetHerbrandBase*.

-El cálculo de todas las *Interpretaciones de Herbrand de nivel $n$* está implementado en el módulo *HerbrandLPO*, a través de la funciones *signatureHerbrandInterpSets, formulaLPOHerbrandInterpSets, lpoSetHerbrandInterpSets*.

-El cálculo de todos los *Modelos de Herbrand de nivel $n$* está implementado en el módulo *HerbrandLPO*, a través de la funciones *formulaLPOHerbrandModels, lpoSetHerbrandModels*.

-El cálculo de la *Extensión de Herbrand de nivel $n$* está implementado en el módulo *HerbrandLPO*, a través de la funciones *formulaLPOHerbrandExtension, lpoSetHerbrandExtension*.

NOTA: Todas estas funciones trabajan exclusivamente con fórmulas ABIERTAS, y devuelven objetos Maybe, por lo que si se intentan usar con fórmulas no abiertas, devolverán Nothing, en otro caso devolverán objetos Just.

 Veámos la aplicación de los métodos presentados en algunos ejemplos

 <div style="padding:0.3cm; border: 0.02cm solid black; font-style:italic;">
 <b><i><u>Ejemplo 4.6.</u></i></b><br><br>

 Dada el conjunto de fórmulas abiertas $\Sigma \equiv \{P(x) \rightarrow P(f(f(x))), \neg P(f(f(f(x)))), P(f(a))\} $ demostrar que es inconsitente, haciendo uso del Teorema de Herdbrand. <br>

 Necesitamos encontrar algún subconjunto de $EH(\Sigma)$ que sea inconsistente, o análogamente podemos buscar $EH_{n}(\Sigma)$ que sea inconsistente, de forma que:
 </div><br>

 ```elm {l context="1"}
 import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO, toLatexLPOSet)
 import Logicus.NormalFormsLPO exposing (toSkolemForm, toCNF, toClause, toLatexClauseSet, Clause)
 import Logicus.SintaxSemanticsLPO exposing (FormulaLPO, SetLPO)
 import Logicus.SintaxSemanticsLP exposing (LPSet, isConsistent)
 import Logicus.IO_LP exposing (toLatexLPSet, toLatexFLP, toLatexLPSetInLines)
 import Logicus.HerbrandLPO exposing (lpoSetHerbrandExtension)
 ```
 (1) Definimos el conjunto de fórmulas:

 ```elm {l context="1"}
f1 :  FormulaLPO
f1 = fromStringToFLPO "P[x;] IMPLIES P[_f[_f[x;];];]" |> extractReadFLPO

f2 :  FormulaLPO
f2 = fromStringToFLPO "NOT P[_f[_f[_f[x;];];];]" |> extractReadFLPO

f3 :  FormulaLPO
f3 = fromStringToFLPO "P[_f[_a;];]" |> extractReadFLPO

s : SetLPO
s = [f1, f2, f3]
 ```

 ^^^elm {m=(toLatexLPOSet s) context="1"}^^^

 (2) Calculamos, incrementalmente las extensiones de Herbrand de nivel $n$ y comprobamos su factibilidad.

$-$ Para $EH_0$

 ```elm {l context="1"}
 eh0 : LPSet
 eh0 = lpoSetHerbrandExtension s 0 |> Maybe.withDefault []
 ```

^^^elm {m=(toLatexLPSetInLines eh0) context="1"}^^^

Comprobamos si es consistente. Ejecutamos *"isConsistent eh0"*:

 ^^^elm {r=(isConsistent eh0) context="1"}^^^

$-$ Para $EH_1$

```elm {l context="1"}
eh1 : LPSet
eh1 = lpoSetHerbrandExtension s 1 |> Maybe.withDefault []
```
^^^elm {m=(toLatexLPSetInLines eh1) context="1"}^^^

Comprobamos si es consistente. Ejecutamos *"isConsistent eh1"*:

^^^elm {r=(isConsistent eh1) context="1"}^^^

Es iconsistente, luego teniendo en cuenta el *Teorema de Herdbrand*, el conjunto original es inconsistente.
 </div>
