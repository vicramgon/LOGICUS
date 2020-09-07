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


## **TEMA 4** - PARTE 2
Formas Normales En LPO.
====================

4.3. Esqueleto Proposicional
----------------------------

En este apartado vamos a tratar de reducir un problema en LPO a un problema LP, prácticamente, equivalente. Para ello vamos a ver algunos conceptos y resultados, de forma que seguiremos la siguiente metodología:

- Transformaremos las fórmulas LPO a otras, también en LPO, pero mucho más sencillas, tanto, que pudieran parecer, casi proposicionales, con el uso, aplicado sobre la *Forma Prenex*, de la llamada *Forma de Skolem*.

- Reduciremos la consistencia de un conjunto de fórmulas en LPO a la de un conjunto de fórmulas en LP.
Este paso no es, generalmente, realizable de forma completa, ya que la simplificación de las fórmulas puede conllevar (y normalemente lo hace) un incremento en el número de variables a considerar, incluso llegando a necesitar un conjunto infinito de fórmulas proposicionales, pero aún así podremos trabajar con dichos conjuntos, haciendo uso de los trabajos de *Herdbrand*.

### 4.3.1. Forma de *Skolem*

La fórmula de *Skolem* trata de restringir la complejidad de las fórmulas en LPO, sin perder la expresividad de las mismas.

Para poder aplicar las transformaciones de *Skolem*, es necesario trabajar sobre fórmulas cerradas, que se encuentren en Forma *Prenex*. De forma que si no lo estuviere bastaría calcular la forma Prenex de la clausura universal de la fórmula.

La fórmula obtenida mediante la aplicación del método de *Skolem*, que veremos a continuación, no proporciona, generalmente fórmulas equivalentes a las originales pero sí se mantiene la consistencia.

***$\longrightarrow$ Método de Skolem: Funciones y Constantes***

Recordemos que *Skolem* recibe una fórmula cerrada en *Forma Prenex*, esto es: $$ F \equiv Q_1 x_1 \ldots Q_n x_n G\left( x_1, \ldots x_n\right) \quad (G \, abierta) $$, y el objetivo es lograr una fórmula equiconsistente, $F_{sk}$, en *Forma Prenex* y que no contenga cuantificadores existenciales, o , equivalentemente, una fórmula abierta equiconsistente con $F$: $$ F_{sk} \equiv \forall x'_1 \ldots \forall x'_m \, H(x'_1, \ldots, x'_m) \equiv  H(x'_1, \ldots, x'_m)$$

Para ello se tiene la operación de *Introducción de Funciones de Skolem*, tal que cada variable cuantificada existencialmente es sustituida por una función dependiente de las variables cuantificadas universalmente, que aparezcan delante. De forma que :

- Si no se tienen cuantificadores universales de orden superior entonces el cuantificado es eliminado y la variable es sustituida por una constante, esto es: $$ Skolem(\exists x_i F(x_i)) \equiv Skolem(F\{x_i/c_j\}) \, (c_j \textit{ nueva constante})$$.

- Si se tienen cuantificadores universales de orden superior, $\forall x'_i, \ldots, \forall x'_j$ entonces el cuantificador es eliminado y la variable es sustituida por una función dependiente de $x'_i, \ldots, x'_j$, esto es:

$$ Skolem(\forall x'_i, \ldots, \forall x'_j \exists x_i F(x_i)) \equiv Skolem(\forall x'_i, \ldots, \forall x'_j F\{x_i/g(x'_i, \ldots, x'_j)\})$$.

De forma que realizando dicha operación se llegaría a una fórmula:

$$\forall x'_1 \ldots \forall x'_m \, H(x'_1, \ldots, x'_m)$$

que corresponde precisamente al objetivo que se presentaba al comienzo del apartado. Ahora bien, nos queda demostrar que esta expresión es equiconsistente con la fórmula original. Para ello, se tienen los resultados:

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic; text-align:justify;">
<u>Teorema 4.2.</u>

La introducción de funciones de Skolem conserva la consistencia. Esto es, dada una fórmula $F$ en LPO si $F'$ se obtiene por medio de la aplicación del método de Skolem, entonces se tiene que: $$ F \textit{ posee un modelo } \Leftrightarrow F' \textit{ posee un modelo } $$
</div><br>

Y además se tiene:

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic;">
<u>Teorema 4.3.</u>

La introducción de funciones de Skolem no aumenta el conocimiento deducible del lenguaje original. Es decir,  dada una fórmula $F$ ceradda de un lenguaje de primer orden $L$, si $F'$ se obtiene por medio de la aplicación del método de Skolem, entonces se tiene que para toda fórmula $H$ del lenguaje de $L$ se tiene: $$ F \models H \Leftrightarrow F' \models H $$
</div><br>

Los conceptos y resultados anteriores nos permiten dar una definición formal de lo que se conoce como *Forma de Skolem* y su aplicabilidad sobre conjuntos de fórmulas LPO. De forma que se tiene:

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic; align:justify;">
<u>Definición.</u> <b>Forma de Skolem</b>

Sea $L$ un lenguaje de primer orden y $F$ una fórmula cerrada de $L$. Una **forma de Skolem** de $F$ es una fórmula universal $G$ que se obtiene a partir de una forma prenexa de $F$ mediante la introducción sucesiva de funciones de Skolem.
</div><br>

De forma que se tiene además que:

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic;">
<u>Teorema 4.4.</u>

Dado un conjunto $\Sigma=\{F_1, \ldots, F_n\}$ de fórmulas ceraddas de un lenguaje de primer orden $L$ y un  conjunto, $\Sigma'$, formado por las formas de Skolem de las fórmulas de $\Sigma$, entonces:

- $\Sigma$ posee un modelo si y sólo si $\Sigma'$ posee un modelo. (Se mantiene la consistencia).
- Para toda fórmula $H$ del lenguaje $L$, se cumple: $$ \Sigma \models H \Leftrightarrow \Sigma' \models H $$
</div><br>

Veámos, a continuación, algunos ejemplos de aplicación de *Skolem*:

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.3.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \forall x \exists y  \exists z \, (P(y,x) \wedge P(z,y)) $, <i>calcular la forma Skolem para $F$:</i><br>
</div><br>

Teniendo en cuenta que $F$ se encuentra en forma *Prenex* y es cerrada, entonces basta aplicar el método de *Skolem*, de forma que:

1. Introducimos una función de Skolem $f_1$, dependiente de la variable $x$ sustituyendo a la variable $y$ y eliminamos el cuantificador $\exists y$, tal que obtenemos: $$\forall x \exists z \, (P(f_1(x),x) \wedge P(z,f_1(x)))$$

2. Introducimos una función de Skolem $f_2$, dependiente de la variable $x$ sustituyendo a la variable $z$ y eliminamos el cuantificador $\exists z$, tal que obtenemos: $$\forall x \, (P(f_1(x),x) \wedge P(f_2(x),f_1(x)))$$

De forma que la fórmula obtenida es una fórmula de *Skolem* de la fórmula inicial.
</div><br>

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.4.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \exists x_1 \forall y_1  \exists z_2 \forall x_2 \exists y_2 \, ((x_1 + b = y_1) \rightarrow (x_2 · y_2 = 0 + z_2)) $, <i>calcular la forma Skolem para $F$:</i><br>
</div><br>

Teniendo en cuenta que $F$ se encuentra en forma *Prenex* y es cerrada, entonces basta aplicar el método de *Skolem*, de forma que:

1. Introducimos una constante de Skolem $c$, y eliminamos el cuantificador $\exists x_1$, tal que obtenemos: $$ \forall y_1  \exists z_2 \forall x_2 \exists y_2 \, ((c + b = y_1) \rightarrow (x_2 · y_2 = 0 + z_2)) $$

2. Introducimos una función de Skolem $f_1$, dependiente de la variable $y_1$ sustituyendo a la variable $z_2$ y eliminamos el cuantificador $\exists z_2$, tal que obtenemos: $$ \forall y_1 \forall x_2 \exists y_2 \, ((c + b = y_1) \rightarrow (x_2 · y_2 = 0 + f_1(y_1))) $$

3. Introducimos una función de Skolem $f_2$, dependiente de las variables $y_1$ y $x_2$ sustituyendo a la variable $y_2$ y eliminamos el cuantificador $\exists y_2$, tal que obtenemos: $$ \forall y_1 \forall x_2 \, ((c + b = y_1) \rightarrow (x_2 · f_2(y_1,x_2) = 0 + f_1(y_1))) $$

De forma que la fórmula obtenida es una fórmula de *Skolem* de la fórmula inicial.
</div><br>

<div style="text-align:justify; padding:0.3cm; background-color:lightgray">
<i><b><u>Forma de Skolem en Logicus</i></b></u><br><br>

El cálculo de la <i>Forma de Skolem</i> está implementado en el módulo *NormalFormsLPO*, a través de la función *toSkolemForm*. Esta función recibe una *FormulaLPO* y calcula una forma de Skolem, siguiendo el método de Skolem y devolviendo una fórmula abierta (sin cuantificadores).

<u>IMPORTANTE:</u> El método es únicamete aplicable sobre fórmulas cerradas en forma Prenex, por lo que es conveniente dar al método una fórmula que cumpla dichas condiciones. En caso de que no lo haga la función calculará una forma Prenex cerrada y posteriormente aplicará el método de Skolem.

<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.3.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \forall x \exists y  \exists z \, (P(y,x) \wedge P(z,y)) $, <i>calcular la forma Skolem para $F$:</i><br>
</div><br>

```elm {l context="1"}
import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO)
import Logicus.NormalFormsLPO exposing (toSkolemForm)
import Logicus.SintaxSemanticsLPO exposing (FormulaLPO)
```

(1) Definición de la fórmula.

```elm {l context="1"}
f : FormulaLPO
f = fromStringToFLPO "FORALL{x} EXISTS{y} EXISTS{z} (P[y;x;] AND P[z;y;])"|> extractReadFLPO
```
^^^elm {m=("$F$: " ++ toLatexFLPO f) context="1"}^^^

(2) Calculamos la forma de *Skolem*.

```elm {l context="1"}
f_sk : FormulaLPO
f_sk = toSkolemForm f
```

```elm {m context="1"}
fskShow = "$F_{sk}$: " ++ toLatexFLPO f_sk
```
^^^elm {m=(fskShow) context="1"}^^^

<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.4.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \exists x_1 \forall y_1  \exists z_2 \forall x_2 \exists y_2 \, ((x_1 + b = y_1) \rightarrow (x_2 · y_2 = 0 + z_2)) $, <i>calcular la forma Skolem para $F$:</i><br>
</div><br>

```elm {l context="2"}
import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO)
import Logicus.NormalFormsLPO exposing (toSkolemForm)
import Logicus.SintaxSemanticsLPO exposing (FormulaLPO)
```

(1) Definición de la fórmula.

```elm {l context="2"}
f : FormulaLPO
f = fromStringToFLPO "EXISTS{x_1} FORALL{y_1} EXISTS{z_2} FORALL{x_2} EXISTS{y_2} ( (_+[x_1;_b;] = y_1) IMPLIES (_·[x_2; y_2;] = _+[_0;z_2;]) )" |> extractReadFLPO
```
^^^elm {m=("$F$: " ++ toLatexFLPO f) context="2"}^^^

(2) Calculamos la forma de *Skolem*.

```elm {l context="2"}
f_sk : FormulaLPO
f_sk = toSkolemForm f
```

```elm {m context="2"}
fskShow = "$F_{sk}$: " ++ toLatexFLPO f_sk
```
^^^elm {m=(fskShow) context="2"}^^^

</div>

### 4.3.2. FNC, FND y Forma Clausal

De igual manera que definíamos la *Forma Normal Conjuntiva* y la *Forma Normal Disyuntiva* para fórmulas LP podemos definirlas para fórmulas de LPO, de forma que se tienen las siguientes definiciones:

<div style="padding:0.3cm; background-color:#ECECEC; font-style:italic;">
<u> Definiciones</u><br> <br>

- Una fórmula, $F$, se dice **Literal** si se trata de una fórmula atómica o de la negación de una, esto es, si se trata de un predicado (o igualdad) o la negación de uno.<br><br>

- Una fórmula abierta está en **Forma Normal Conjuntiva** si está expresada como conjunción de disyunciones de literales $$F = \bigwedge \limits_{i=1}^{n}\left(\bigvee \limits_{j=1}^{m_i} L_{i,j}\right)$$.

- Una fórmula abierta está en **Forma Normal Disyuntiva** si está expresada como disyunción de conjunciones de literales. $$F = \bigvee \limits_{i=1}^{n}\left(\bigwedge \limits_{j=1}^{m_i} L_{i,j}\right)$$

- Una **cláusula** corresponde a una disyunción de literales, por tanto es una fórmula abierta. Al igual que en el caso proposicional identificaremos una cláusula con el conjunto de literales que aparecen en ella y denotaremos la cláusula vacía con el símbolo □.<br><br>

- Dada una fórmula, $F$ de $L$, una fórma clausal de $F$ corresponde a un conjunto de cláusulas $S$ (no necesariamente del lenguaje $L$) tal que: $$ F \textrm{ tiene un modelo } \Leftrightarrow S \textrm{ tiene un modelo}$$
De forma que  la obtención de una forma clausal se reduce a la aplicación del siguiente procedimiento: <br><br>
  1. Obtener el cierre universal de $F$ (si es que no es cerrada). Denotémoslo por $G$.
  2. Obtener una Forma de Skolem para $G$ (sea $G_{sk}$) y expresarla en su forma abierta (obviando los cuantificadores).
  3. Obtener una FNC pars $G_{sk}$, siguiendo el procedimiento visto en el caso LP, de forma que cada una de las disyunciones corresponde a una cláusula y la conjunción de las disyunciones corresponde a la unión de las cláusulas. $$FNC(G_{sk}) = \bigwedge \limits_{i=1}^{n} C_i \Longrightarrow Forma \, Clausal (F) = \{C_i \, (i=1, \ldots, n)\}$$

- De forma análoga, una Forma Clausal para un conjunto de $\Gamma$ corresponde a un conjunto de fórmulas $S$ tal que $$ \Gamma \textrm{ tiene un modelo } \Leftrightarrow S \textrm{ tiene un modelo}$$ y podemos obtenerla sin más que realizar la unión de las formas clausales de cada una de las fórmulas de $\Gamma$
</div><br>

Veamos un ejemplo del cálculo de las formas clausáles:

<div style="padding:0.3cm; background-color:#FDE992">
<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.5.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \forall x \, \left( \forall y \, H(x,y) \rightarrow \exists z \forall u \, (u \neq z \rightarrow P(z,u))\right) $, <i>calcular una Forma Clausal para $F$:</i><br>
</div><br>

Teniendo en cuenta que $F$ es una fórmula cerrada, y siguiendo el procedimiento descrito:
1. Obtenemos una forma *Prenex*, $F_p$, para $F$: $$ \forall x \exists y \exists z \forall u \, \left( H(x,y) \rightarrow (u \neq z \rightarrow P(z,u))\right) $$

2. Obtenemos una forma de *Skolem*, $F_{s}$:
$$ \begin{array}{c}
\forall x \forall u \, \left(H(x, f_1(x)) \rightarrow (u \neq f_2(x) \rightarrow P(f_2(x),u)) \right) \\ \Downarrow  \\ H(x, f_1(x)) \rightarrow (u \neq f_2(x) \rightarrow P(f_2(x),u)) \end{array}$$

3. Obtenemos una FNC para $F_s$:
$$ \neg  H(x, f_1(x)) \vee u = f_2(x) \vee P(f_2(x), u)$$

4. Obtenemos una Forma Clausal, directamente de la expresión anterior:
$$ \left\lbrace \{ \neg  H(x, f_1(x)) , u = f_2(x) , P(f_2(x), u) \} \right\rbrace$$
</div><br>


<div style="text-align:justify; padding:0.3cm; background-color:lightgray">
<i><b><u>FNC, FND y Forma Clausal en Logicus</i></b></u><br><br>

-El cálculo de la <i>Forma Normal Conjuntiva </i> está implementado en el módulo *NormalFormsLPO*, a través de la función *toCNF*. Esta función recibe una *FormulaLPO* y calcula una FNC, siguiendo el procedimiento estudiado.

-El cálculo de la <i>Forma Normal Disyunyiva </i> está implementado en el módulo *NormalFormsLPO*, a través de la función *toDNF*. Esta función recibe una *FormulaLPO* y calcula una FNC, siguiendo el procedimiento estudiado.

-El cálculo de la <i>Forma de Clausal</i> está implementado en el módulo *NormalFormsLPO*, a través de la función *toClause*. Esta función recibe una *FormulaLPO* y calcula una Forma Clausal, devolviendo un conjunto de cláusulas. Para el cálculo de la Forma Clausal de un conjunto se tiene también la función *toClauseSet*.

Veámos la aplicación de los métodos presentados en el ejemplo expuesto anteriormente

<div style="padding:0.3cm; border: 0.02cm solid black;">
<b><i><u>Ejemplo 4.5.</u></i></b><br><br>

<i>Dada la fórmula</i> $F \equiv \forall x \, \left( \forall y \, H(x,y) \rightarrow \exists z \forall u \, (u \neq z \rightarrow P(z,u))\right) $, <i>calcular la forma Skolem para $F$:</i><br>
</div><br>

```elm {l context="3"}
import Logicus.IO_LPO exposing (fromStringToFLPO, extractReadFLPO, toLatexFLPO)
import Logicus.NormalFormsLPO exposing (toSkolemForm, toCNF, toClause, toLatexClauseSet, Clause)
import Logicus.SintaxSemanticsLPO exposing (FormulaLPO)
```

Primero sigamos el método dado para la resolución

(1) Definición de la fórmula.

```elm {l context="3"}
f : FormulaLPO
f = fromStringToFLPO "FORALL{x} (FORALL{y} H[x;y;] IMPLIES EXISTS{z} FORALL{u} (NOT (u=z) IMPLIES P[z;u;]) )" |> extractReadFLPO
```
^^^elm {m=("$F$: " ++ toLatexFLPO f) context="3"}^^^

(2) Calculamos una Forma de Skolem para $F$:

```elm {l context="3"}
f_s : FormulaLPO
f_s = toSkolemForm f
```
^^^elm {m=("$F_s$: " ++ toLatexFLPO f_s) context="3"}^^^

(3) Calculamos una *FNC* para $F_s$:

```elm {l context="3"}
f_cnf : FormulaLPO
f_cnf = toCNF f_s
```
```elm {m context="3"}
f_cnfshow = "$F_{CNF}$: " ++ toLatexFLPO f_cnf
```
^^^elm {m=(f_cnfshow) context="3"}^^^

De forma que la forma clausal correspondería a: $$ \left\lbrace \{ \neg  H(x, f_1(x)) , u = f_2(x) , P(f_2(x), u) \} \right\rbrace$$ Que coincide con la que calculamos en el ejemplo.

Para calcularla podríamos utilizado directamente la función $toClause$ y habríamos obtenido el mismo resultado:

```elm {l context="3"}
f_clause : List Clause
f_clause = toClause f_s
```
^^^elm {m=(toLatexClauseSet f_clause) context="3"}^^^
</div>
