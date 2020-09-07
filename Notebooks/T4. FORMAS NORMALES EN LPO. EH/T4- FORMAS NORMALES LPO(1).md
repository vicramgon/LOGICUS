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


## **TEMA 4** - PARTE 1
Formas Normales En LPO.
====================

Descripción general del capítulo
--------------------------------

En el tema anterior estudiamos las formas normales para LP (FND, FNC). Estas mismas formas se pueden aplicar en el caso LPO, bajo la aplicación de las mismas reglas sobre las conectivas, de forma que basta dar un tratamiento para los cuantificadores para poder plantear las formas vistas anteriormente, ahora aplicadas en el plano LPO. Para realizar dicho tratamiento vamos a realizar el estudio de dos nuevas formas, Prenex y Skolem), que servirán como modo de preprocesado de fórmulas LPO, de forma que nos permitan, después, plantear las fórmulas en formato FNC o FND, o incluso llevar a cabo una reducción, al menos parcial, de LPO a LP, a través de los trabajos del matemático Jaques Herbrand (que abordaremos en la segunda parte del tema).

De forma que dicha reducción nos permitirá aplicar algoritmos exclusivos para LP (como DPLL) a fórmulas que modelan escenarios en LPO.

### <u> Estructura del capítulo </u>

El capítulo se encuentra estructurado en distintas secciones, a través de las cuales se abordan los conceptos fundamentales para el trabajo con fórmulas equivalentes, planteando el concepto de equivalencia en LPO. formas Prenex, Skolem, FNC y FND, forma clausal y los Trabajos de Herdbrand destinados al estudio del problema fundamental (satisfactibilidad de conjuntos) para la Lógica de Primer Orden.

Para el estudio práctico se hará uso de los módulos:

-   **Módulo NormalFormsLPO**. Recoge la implementación de los métodos de transformación a forma Prenex, Skolem. FNC, FND y Forma Clausal.

-   **Módulo HerbrandLPO**. Recoge la implementación de los métodos de reducción de LP a LPO a través de los distintos elementos de Herdbrand: Signatura, Universo de Herdbrand, Interpretaciones de Herdbrand, Modelos de Herdbrans, Extensión de Herdbrand, Teorema de Herdbrand.

- Otros módulos secundarios (ya vistos con anterioridad)

4.1. Introducción.
------------------

En el tema anterior estudiamos el concepto de equivalencia entre fórmulas proposicionales, de manera que dos fórmulas eran equivalentes si los modelos de una equivalían a los modelos de la otra. La traslación de este concepto a LPO es casi directa, de forma que:

> Dos fórmulas $F$ y $G$ de un lenguaje de primer orden son equivalentes si y sólo si para toda valoración, se tiene $v(F) = v(G)$, o equivalentemente, $F \leftrightarrow G$ es lógicamente válida.

Nótese que si ambas fórmulas son cerradas entonces se tiene la misma definición que en el caso LP, esto es, $F$ y $G$ son equivalentes si y sólo si poseen, exactamente, los mismos modelos. Para las fórmulas no cerradas la equivalencia queda como $F \equiv G \Leftrightarrow \models \forall\vec{x} (F(\vec{x}) \leftrightarrow G\vec{x})$

El uso de la equivalencia resultará indispensable en este tema y el siguiente, para poder realizar un procesamiento de las fórmulas, reescribiéndolas de forma adecuada, para poder aplicar sobre ellas otras técnicas.Veremos en los sucesivos apartados un serie de transformaciones sobre las fórmulas que nos llevará, finalmente, al planteamiento de las formas normales (*FNC, FND, Clausulas*), análogas a las presentadas en LP.


4.2. Formas Normales en LPO.
----------------------------
 > Una fórmula $F$ de un lenguaje LPO está expresada en *Forma Prenex* si todos los cuantificadores se encuentran en la cabeza de la fórmula. Esto es, $F$ es de la forma: $$ Q_1x_1 \, Q_2x_2 \, \ldots Q_nx_n G(x_1, x_2, \ldots, x_n)$$ donde $Q_i \in \{\exists, \forall\}$ y $G(x_1, \ldots, x_n)$ es abierta.

 Se dice que una fórmula, $H$, es una forma normal prenexa de otra, $F$, si se tiene que $F\equiv H$ y $H$ está en forma prenex.

 Tal que resulta sencillo el cálculo de formas normales prenexas, dado que se tiene el *Lema*:
 > Si $F'$ se obtiene de $F$ por aplicación de una operación prenex, entonces $F \equiv F'$

de forma que se tienen las siguientes operaciones prenex:

 <center> <img src="images/T4Img1.png" style="width:60%" title=""></center><br>

 Además se tiene el siguiente resultado:

 > <u><i>Teorema</i></u>
 > Para toda fórmula, $F$, existe otra, $G$, en forma prenex que equivalente a $F$

 De hecho, basta seguir los pasos siguientes para el cálculo de una forma prenex:

 1. Renombrado de las variables tal que no se produzcan interferencias en las cuantificaciones. (P1)
 2. Sustitución de la conectiva $\leftrightarrow$ por la conjunción de las implicaciones.
 3. Interiorización de las negaciones (P2)
 4. Aplicación de las reglas (P3)-(P6)

 Veámos un par de ejemplos de aplicación del método anterior.

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.1.</u></i></b><br><br>

<i>Dada la fórmula</i> $F(z,y) \equiv \forall x \, \exists y \, (x + z = y) \rightarrow \exists z \, \forall x (x·y = 0 + z)$, <i>calcular una forma prenex para $F$:</i><br>
</div><br>

1. Renombramos las variables: $$\forall x_1 \, \exists y_1 \, (x_1 + z = y_1) \rightarrow \exists z_1 \, \forall x_2 (x_2·y = 0 + z_1)$$

2. Sacamos a la cabeza los cuantificadores del antecedente (conmutados) y del consecuente priorizando los existenciales en la cabeza:
$$\begin{array}{c}\exists x_1 \, \left(\exists y_1 \, (x_1 + z = y_1) \rightarrow \exists z_1 \, \forall x_2 (x_2·y = 0 + z_1) \right) \\
\exists x_1 \exists z_1 \, \left(\exists y_1 \, (x_1 + z = y_1) \rightarrow \forall x_2 (x_2·y = 0 + z_1) \right) \\
\exists x_1 \exists z_1 \forall y_1 \, \left((x_1 + z = y_1) \rightarrow \forall x_2 (x_2·y = 0 + z_1) \right) \\
\exists x_1 \exists z_1 \forall y_1 \forall x_2 \,\left((x_1 + z = y_1) \rightarrow (x_2·y = 0 + z_1) \right)\\
\end{array}$$
</div>

<br>

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.2.</u></i></b><br><br>

<i>Dada la fórmula</i> $G(x) \equiv \exists x \, P(x,a) \rightarrow \left( \forall z \, P(x,z) \rightarrow \forall x \, P(x,a) \right)$, <i>calcular una forma prenex para $G$:</i><br>
</div><br>


1. Renombramos las variables: $$\exists x_1 \, P(x_1,a) \rightarrow \left( \forall z_1 \, P(x,z_1) \rightarrow \forall x_2 \, P(x_2,a) \right)$$

2. Sacamos a la cabeza los cuantificadores del antecedente (conmutados) y del consecuente priorizando los existenciales en la cabeza:

    - Primero hemos de calcular una forma prenex para el consecuente. Como es el consecuente, tratando de extraer primero los quedan como existenciales: $$ \exists x_1 \, P(x_1,a) \rightarrow \exists z_1 \forall x_2 \, (P(x,z_1) \rightarrow P(x_2, a))$$

    - Y ahora podemos ya, obtener una fórmula prenex para $G$: $$ \exists z_1 \forall x_1 \forall x_2 \, P(x_1,a) \rightarrow (P(x,z_1) \rightarrow P(x_2, a))$$

</div><br>

<div style="text-align:justify; padding:0.3cm; background-color:lightgray">
<i><b><u>Forma Prenex en Logicus</i></b></u><br><br>

El cálculo de la <i>Forma Prenex</i> está implementado en el módulo *NormalFormsLPO*, a través de la función *toPrenexForm*. Esta función recibe una *FormulaLPO* y calcula una forma prenex, siguiendo los pasos descritos en el método general, anteponiendo los cuantificadores extraídos como existenciales sobre los extraídos como universales.

<u>IMPORTANTE:</u> Debemos darle la función correctamente renombrada (podemos utilizar la función <i>renameVars</i> del módulo <i>SintaxSemanticsLPO</i>)



<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.1.</u></i></b><br><br>

<i>Dada la fórmula</i> $F(z,y) \equiv \forall x \, \exists y \, (x + z = y) \rightarrow \exists z \, \forall x (x·y = 0 + z)$, <i>calcular una forma prenex para $F$:</i><br>
</div><br>

```elm {l context="1"}
import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO)
import Logicus.NormalFormsLPO exposing (toPrenexForm)
import Logicus.SintaxSemanticsLPO exposing (FormulaLPO, renameVars)
```
(1) Definimos la fórmula $F$

```elm {l context="1"}
f : FormulaLPO
f = fromStringToFLPO "FORALL{x} EXISTS{y} (_+[x;z;] = y) IMPLIES EXISTS{z} FORALL{x}(_·[x;y;] = _+[_0;z;])" |> extractReadFLPO
```

^^^elm {m=("$F :$ " ++ toLatexFLPO f) context="1"}^^^

(2) Renombramos las variables

```elm {l context="1"}
f2 : FormulaLPO
f2 = renameVars f
```
^^^elm {m=("$F_2 :$ " ++ toLatexFLPO f2) context="1"}^^^

(3) Obtenemos la forma prenex con la aplicación de la función *toPrenexForm*:

```elm {l context="1"}
fp : FormulaLPO
fp = toPrenexForm f2
```
^^^elm {m=("$F_p :$ " ++ toLatexFLPO fp) context="1"}^^^

Que corresponde exactamente con la que nosotros hemos calculado anteriormente.
<br>
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.2.</u></i></b><br><br>

<i>Dada la fórmula</i> $G(x) \equiv \exists x \, P(x,a) \rightarrow \left( \forall z \, P(x,z) \rightarrow \forall x \, P(x,a) \right)$, <i>calcular una forma prenex para $G$:</i><br>
</div><br>

```elm {l context="2"}
import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO)
import Logicus.NormalFormsLPO exposing (toPrenexForm)
import Logicus.SintaxSemanticsLPO exposing (FormulaLPO, renameVars)
```
(1) Definimos la fórmula $F$

```elm {l context="2"}
g: FormulaLPO
g = fromStringToFLPO "EXISTS{x} P[x;_a;] IMPLIES (FORALL{z} P[x;z;] IMPLIES FORALL{x} P[x;_a;])" |> extractReadFLPO
```

^^^elm {m=("$G :$ " ++ toLatexFLPO g) context="2"}^^^

(2) Renombramos las variables

```elm {l context="2"}
g2 : FormulaLPO
g2 = renameVars g
```
^^^elm {m=("$G_2:$ " ++ toLatexFLPO g2) context="2"}^^^

(3) Obtenemos la forma prenex con la aplicación de la función *toPrenexForm*:

```elm {l context="2"}
gp: FormulaLPO
gp = toPrenexForm g2
```
^^^elm {m=("$G_p :$ " ++ toLatexFLPO gp) context="2"}^^^
</div>
