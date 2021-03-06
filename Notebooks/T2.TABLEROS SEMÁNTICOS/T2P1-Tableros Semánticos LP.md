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


## **TEMA 2 - PARTE 1**
Tableros Semánticos I
====================

Descripción general del capítulo
--------------------------------

En el tema anterior fijamos las bases, sintácticas y semánticas, para poder trabajar con la Lógica Proposicional y la Lógica de Primer Orden. En este capítulo, comenzaremos a ahondar en los algoritmos de decisión más relevantes, presentando el primero de los que vamos a tratar a lo largo del temario, los Tableros Semánticos, del que vamos a tratar los conceptos teóricos y el desarrollo del algoritmo, así como el trabajo con el mismo a través de la librería Logicus.

### <u> Estructura del capítulo </u>

El capítulo se encuentra estructurado en distintas secciones, a través de las cuales se abordan los conceptos fundamentales para el trabajo con Tableros Semánticos, destinados al estudio del problema fundamental (satisfactibilidad de conjuntos) para la Lógica Proposicional y la Lógica de Primer Orden.

Para el estudio práctico se hará uso de los módulos:

-   **Módulo SemanticBoardsLP**. Recoge la implementación del algoritmo de Tableros Semánticos para la Lógica Proposicional.

- **Módulo SemanticBoardsLP**. Recoge la implementación del algoritmo de Tableros Semánticos para la Lógica de Primer Orden.

-   **Módulo IO\_LP**. Recoge las implementaciones de los métodos relacionados con la lectura y representación de las fórmulas LP.

-   **Módulo IO\_LPO**. Recoge las implementaciones de los métodos relacionados con la lectura y representación de las fórmulas LPO.


2.1. Introducción. Visión general de Tableros Semánticos
--------------------------------------------

El método de los Tableros Semánticos establece un algoritmo para la decisión de la satisfactibilidad de un conjunto de fórmulas, trabajando con las fórmulas del conjunto desde un marco sintáctico y sin necesidad de preprocesamiento de las fórmulas.

Para reducir la satisfactibilidad del conjunto, se basa en la obtención de conjuntos de literales, en las que la decisión se da de manera trivial , de manera que o bien todos los conjuntos de literales son inconsistentes (entonces el original también lo es) o bien proporcionan modelos (en el caso de la lógica proposicional más claramente) para el conjunto (que resulta, por tanto, consistente).

Además este método es gráficamente representable de manera sencilla a través de árboles binarios, que recogen todo el proceso del algoritmo.

Aunque nosotros vamos a estudiar su desarrollo para las lógicas LP y LPO, la versatilidad y sencillez del método, lo hace fácilmente adaptable a otras lógicas (descriptivas, modales, ...).

2.2. Tableros Semánticos en Lógica Proposicional
--------------------------------------------

El método de los tableros semánticos (en adelante tableros) en la lógica proposicional basa su desarrollo en dos pasos fundamentales:

1. **Clasificación de las fórmulas**. De forma que clasifica las fórmulas en dos clases.
    - Las fórmulas de tipo $\alpha$, que adoptan el comportamiento de conjunciones.
    - Las fórmulas de tipo $\beta$, que adoptan el comportamiento de disyunciones.<br><br>

    Aunque podrían verse como fórmulas $\alpha$ se distingue una tercera clase, $dN$, que engloa a aquellas fórmulas que resultan ser la doble negación de otras.

2. Reduce la satisfactibilidad de cada una de las fórmulas del conjunto a la satisfactibilidad de dos fórmulas más sencillas (**componenetes**).

### Clasificación de las fórmulas

#### Fórmulas $dN$

Distinguimos esta clase aparte, ya que de este tipo de fórmulas se obtiene una única componente, en vez de 2 como ocurre para los casos $\alpha$ y $\beta$.

Esta categoría engloba únicamente a fórmulas tal que $F \equiv \neg \neg G$, obteniéndose como única componente $G$ (ya que se tiene $\neg \neg G \equiv G$).

#### Fórmulas $\alpha$

Esta categoría engloba a todas aquellas fórmulas que se comportan como conjunciones ($F \equiv \alpha_1 \wedge \alpha_2$), y de las cuales se obtienen dos componentes ($\alpha_1$, $\alpha_2$), de forma que la satisfactibilidad de la fórmula original se reduce a la satisfactibilidad simultánea de las componentes. Son fórmulas $\alpha$:

<table style="margin:auto; width:30%; border-collapse:collapse;border-color:#ccc;border-spacing:0" class="tg"><thead><tr><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">𝛼</th><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">𝛼₁</th><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">𝛼₂</th></tr></thead><tbody><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁ ∧ F₂</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬(F₁ ∨ F₂)</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬(F₁ → F₂)</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁ ↔ F₂</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁ → F₂</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₂ → F₁</td></tr></tbody></table>

#### Fórmulas $\beta$

Esta categoría engloba a todas aquellas fórmulas que se comportan como disyunciones ($F \equiv \alpha_1 \vee \alpha_2$), y de las cuales se obtienen dos componentes ($\beta_1$, $\beta_2$), de forma que la satisfactibilidad de la fórmula original se reduce a la satisfactibilidad de alguna de las componentes. Son fórmulas $\beta$:

<table style="margin:auto; width:35%; border-collapse:collapse;border-color:#ccc;border-spacing:0" class="tg"><thead><tr><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">β</th><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">β₁</th><th style="background-color:#f0f0f0;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;font-weight:normal;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">β₂</th></tr></thead><tbody><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁ v F₂</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬(F₁ ∧ F₂)</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₁ → F₂</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ F₁</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">F₂</td></tr><tr><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬ (F₁ ↔ F₂)</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬(F₁ → F₂)</td><td style="background-color:#fff;border-color:inherit;border-style:solid;border-width:1px;color:#333;font-family:Arial, sans-serif;font-size:14px;overflow:hidden;padding:10px 5px;text-align:center;vertical-align:top;word-break:normal">¬(F₂ → F₁)</td></tr></tbody></table>


### Reglas de reducción de la satisfactibilidad.

Reducen la consistencia de un conjunto de fórmulas a la de otro conjunto formado por fórmulas más sencillas.

#### Regla $dN$

> Si $(F \equiv \neg \neg G) \in U$, entonces $U$ es satisfactible si y sólo si lo es $(U - \{F\}) \cup \{G\}$. Denotaremos la aplicación de esta regla sobre un conjunto $U$, dada una fórmula $F$ como $dN(U,F)$.

#### Regla $\alpha$

> Si $F \in U$ es de tipo $\alpha$, entonces $U$ es satisfactible si y sólo si lo es $(U - \{F\}) \cup \{\alpha_1, \alpha_2\}$. Denotaremos la aplicación de esta regla sobre un conjunto $U$, dada una fórmula $F$ como $\alpha(U,F)$.

#### Regla $\beta$

> Si $F \in U$ es de tipo $\beta$, entonces $U$ es satisfactible si y sólo si lo es $(U - \{F\}) \cup \{\beta_1\}$ o lo es $(U - \{F\}) \cup \{\beta_2\}$. Denotaremos la aplicación de esta regla sobre un conjunto $U$, dada una fórmula $F$ como $\beta(U,F)$, siendo $\beta(U,F)_1 = (U - \{F\}) \cup \{\beta_1\}$ y $\beta(U,F)_2 = (U - \{F\}) \cup \{\beta_2\}$

### Algoritmo de Tableros en LP (Tableros completos)

Un **tablero** para un conjunto de fórmulas $U = \{F_1, \ldots, F_n\}$ es un árbol $T$, con nodos etiquetados con conjuntos de fórmulas, construido siguiendo el siguiente esquema:

1. La raíz ($r$) de ($T$) se etiqueta con $U_r = \{F_1, \ldots, F_n\}$.
2. Si quedan hojas en $T$ no marcadas, tomar la hoja $n$ con etiqueta $U_n$ y:
  2.1. Si $U_n$ contiene una contradicción, esto es, dos fórmulas tal que una sea la negación de la otra, entonces marcar la **hoja** como **cerrada** ("╳").
  2.2. Si no, si $U_r$ corresponde a un conjunto de literales (átomos o negaciones de átomos), entonces marcar la **hoja** como **abierta** ("◯").
  2.3. Si no, si $U_n$ contiene alguna fórmula de tipo $dN$, sea $F$, entonces asignar como hijo de $U_n$ el nodo etiquetado con $U_m = dN(U_n, F)$.
  2.4. Si no, si $U_n$ contiene alguna fórmula de tipo $\alpha$, sea $F$, entonces asignar como hijo de $U_n$ el nodo etiquetado con $U_m = \alpha(U_n, F)$.
  2.5. Si no, $U_n$ contiene alguna fórmula de tipo $\beta$, sea $F$, entonces asignar como hijo de $U_n$ los nodos $n_1$ y $n_2$ nodo etiquetándolos con $U_{n_1}= \beta(U_n, F)_1$ y $U_{n_2}= \beta(U_n, F)_2$.

Nótese que el algoritmo se presenta en forma recursiva, pues es fácilmente probable (por inducción) que en todos los casos el algoritmo termina (en el peor de los casos en un tiempo exponencial). El tablero finalmente obtenido se denomina **tablero completo** y se dice **cerrado** si todas las hojas son cerradas y **abierto** en otro caso.

#### Teorema de Corrección y Completitud

> Dado un conjunto de fórmulas $S$ y un tablero $T$ (completo) para $S$:
> 1. **Corrección**. Si $T$ es cerrado, entonces $S$ es insatisfactible.
> 2. **Completitud**. Si $S$ es insatisfactible, entonces $T$ es cerrado.

#### Búsqueda de modelos.

Dado el *Teorema de Corrección y Completitud*, un conjunto de fórmulas $S$ admite un tablero completo abierto si y sólo si $S$ es consistente. De forma que cada una de las hojas abiertas $H$ (conjuntos de literales no contradictorios) proporciona, al menos, un modelo (no necesariamente distinto) de $S$ tal que para cada uno de las varibles proposicionales $p$ de $S$: $$ v_H(p) = \left\lbrace \begin{array}{ll} v_H(p) = 1 & si \, p \in U_H \\ v_H(p) = 0 & si \, \neg p \in U_H \\ v_H(p) = 0 \, ó \, v_H(p) = 1  & e.o.c. \\\end{array} \right.  $$

#### Consecuencia lógica.

Dada la relación (vista en el tema 1) entre conseciencia lógica e inconsistencia de conjuntos: $$ U \models F \Leftrightarrow (U \cup \{ \neg F\}) \textrm{ es inconsistente}$$

Es sencillo probar la consecuencia lógica sin más reducirlo a la obtención de un tablero cerrado del conjunto, debidamente formado. De forma se tendrá dicha consecuencia si y sólo si el tablero (completo) es cerrado.

### Tableros Semánticos en Logicus.

El módulo *Logicus.SemanticBoardsLP* se ha implementado el método de los tableros semánticos, tanto su desarrollo como su representación gráfica en forma de árbol. De forma que se han seguido los conceptos presentados en los apartados anteriores de forma que:

#### Fórmulas $dN, \alpha$ y $\beta$

    isDobleNeg : FormulaLP -> Bool
    isDobleNeg x =
        case x of
            Neg ( Neg ( Atom _ ) ) -> True

            _ -> False

    isAlpha : FormulaLP -> Bool
    isAlpha x =
        case x of
            Neg (Neg _) -> True
            Conj _ _ -> True
            Neg (Disj _ _) -> True
            Neg (Impl _ _) -> True
            Equi _ _ -> True
            _ -> False

    isBeta : FormulaLP -> Bool
    isBeta x =
        case x of
            Disj _ _ -> True
            Impl _ _ -> True
            Neg (Conj _ _) -> True
            Neg (Equi _ _) -> True
            _ -> False

    -- Componentes

    formulaComponents : FormulaLP -> List FormulaLP
    formulaComponents x =
        case x of
            Atom p -> [Atom p]                  
            Neg (Atom p) -> [Neg (Atom p)]      
            Neg (Neg f) -> [f]                  
            Conj f g -> [f, g]
            Neg (Impl f g) -> [f, Neg g]
            Neg (Disj f g) -> [Neg f, Neg g]
            Disj f g -> [f, g]
            Impl f g -> [Neg f , g]
            Neg (Conj f g) -> [Neg f, Neg g]
            Equi f g -> [Conj f g, Conj (Neg f) (Neg g)]
            Neg (Equi f g) -> [Conj f (Neg g), Conj (Neg f) g]
            _ -> [Insat]

#### Reglas de reducción

    dnExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
    dnExpansion fs f = [uniqueConcatList (remove f fs) (formulaComponents f)]

    alphaExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
    alphaExpansion fs f = [uniqueConcatList (remove f fs) (formulaComponents f)]

    betaExpansion : List FormulaLP -> FormulaLP -> List (List FormulaLP)
    betaExpansion fs f =
        let fs2 = remove f fs in
            List.map (\x -> uniqueConcatList fs2 [x]) (formulaComponents f)

#### Algoritmo de Tableros Semánticos

$-$ Construcción gráfica de Tableros

    makeSBoard : List FormulaLP -> String
    makeSBoard fs =
        let myStyles =
                { defaultStyles | node = "shape=plaintext, color=black", edge = "dir=none, color=blue, fontcolor=blue"}
        in
            String.replace "\n\n" "\n" <| outputWithStyles myStyles (\x -> Just x) (\x-> Just x) <|  uncurry fromNodesAndEdges <| makeSBoardAux fs 0


    makeSBoardAux : List FormulaLP -> NodeId -> (List (Node String), List (Edge String))
    makeSBoardAux fs nid =
        let
            actualNode = Node nid (toStringFLPSet fs)
        in
            if  hasContradiction fs then
                    ([actualNode, Node (nid + 1) "╳"], [Edge  nid  (nid + 1) ""])

            else if isLiteralSet fs then
                        ([actualNode, Node (nid + 1) "◯"], [Edge  nid  (nid + 1) ""])
            else
                case List.head (List.filter isDobleNeg fs) of
                    Just f ->
                        let
                            edgeLabel = "dN : " ++  toStringFLP f ++ " ⟼ " ++ (toStringFLPSet <| formulaComponents f)
                            (nodes, edges) = makeSBoardAux (List.concat <| dnExpansion fs f) (nid + 1)
                        in
                            (actualNode::nodes,  Edge nid (nid + 1) edgeLabel::edges)
                    Nothing ->
                        case List.head (List.filter isAlpha fs) of
                            Just f ->
                                let
                                    edgeLabel = "α : " ++  toStringFLP f ++ " ⟼ " ++ (toStringFLPSet <| formulaComponents f)
                                    (nodes, edges) = makeSBoardAux (List.concat <| alphaExpansion fs f) (nid + 1)
                                in
                                    (actualNode::nodes,  Edge nid (nid + 1) edgeLabel::edges)
                            Nothing ->
                                case List.head (List.filter  isBeta fs) of
                                    Just f ->
                                        case formulaComponents f of
                                            [f1, f2] ->
                                                let
                                                    edgeLabel1 = " β: " ++  toStringFLP f ++ " ⟼ " ++  toStringFLP f1
                                                    edgeLabel2 = " β: " ++  toStringFLP f ++ " ⟼ " ++  toStringFLP f2
                                                    nfs =  remove f fs
                                                in
                                                    let (nodes1, edges1) = makeSBoardAux (uniqueConcatList nfs [f1]) (nid + 1) in
                                                        let nextid = nid + List.length nodes1 + 1 in
                                                            let (nodes2, edges2) = makeSBoardAux (uniqueConcatList nfs [f2]) nextid in
                                                                (actualNode::(nodes1 ++ nodes2), [Edge nid (nid + 1) edgeLabel1, Edge nid nextid edgeLabel2] ++ edges1 ++ edges2)
                                            _ -> ([],[])
                                    _ -> ([],[])


Funciones auxiliares:

    isInsat : FormulaLP -> Bool
    isInsat x =
    case x of
        Insat -> True
        Neg (Insat) -> True
        _ -> False

    isLiteral : FormulaLP -> Bool
    isLiteral x =
        case x of
            Atom _ -> True
            Neg (Atom _) -> True
            _ -> False




    isLiteralSet : List FormulaLP -> Bool
    isLiteralSet fs = List.all (\x -> isLiteral x) fs



    hasContradiction : List FormulaLP -> Bool
    hasContradiction fs = List.any (\x -> member (Neg x) fs || isInsat x) fs

$-$ Búsqueda de modelos

    modelsTab : List FormulaLP -> List (List FormulaLP)     
    modelsTab fs = if hasContradiction fs then
                        []
                    else if isLiteralSet fs then
                        [fs]
                    else
                        List.concat <| List.map (\gs -> modelsTab gs) (sucessors fs)

    generalModels : List (List FormulaLP) -> List (List FormulaLP)
    generalModels ps =  let generalModelsAux ms ls = case List.head ms of
                                                        Nothing -> ls

                                                        Just m -> if List.any (\x -> isSubSetList x m) ls then
                                                                        generalModelsAux (deleteFirstLs ms) ls
                                                                else
                                                                        generalModelsAux (deleteFirstLs ms) (m:: List.filter (\x -> not (isSubSetList m x)) ls )
                        in
                            generalModelsAux ps []

Funciones auxiliares:

    sucessors : List FormulaLP -> List (List FormulaLP)
    sucessors fs = case List.head (List.filter isDobleNeg fs) of
        Just f -> dnExpansion fs f

        Nothing -> case List.head (List.filter  isAlpha fs) of
            Just f -> alphaExpansion fs f

            Nothing -> case  List.head (List.filter  isBeta fs) of
                Just f -> betaExpansion fs f

                Nothing -> [fs]


$-$ Consecuencia lógica

    isConsecuenceBoard : List FormulaLP -> FormulaLP -> Bool
    isConsecuenceBoard fs f = List.isEmpty <| modelsTab <|  Neg f::fs



#### Ejemplos de aplicación de Tableros Semánticos en LP

Los conceptos (teóricos y prácticos) que hemos desarrollado previamente nos permiten trabajar completamente con el Método de Tableros Semánticos para su aplicación a la satisfactibilidad de conjuntos. A continuación, se muestran algunos ejemplos (en forma de ejercicios resueltos) con el fin de afianzar los conceptos vistos durante esta primera parte del tema:


##### Ejemplo 1
> Sea $F: p \wedge q \leftrightarrow \neg p \vee r$:
> 1. Construir un tablero completo para $F$ y otro para $\neg F$
> 2. Decribir todos los modelos y contramodelos de la  fórmula $F$

***Solución (usando Logicus)***

<u>Apartado 1</u>

```elm {l context="1"}
import Logicus.IO_LP exposing (..)
import Logicus.SintaxSemanticsLP exposing (..)
import Logicus.SemanticBoardsLP exposing (..)
import Logicus.AuxForLitvis exposing (..)

a : FormulaLP
a = fromStringToFLP "p AND q EQUIV NOT q OR r" |> extractReadFLP
```
De forma que el tablero para $F$:

```elm {l context="1"}

tablero_a : String
tablero_a  = makeSBoard [a]

-- Ejecutamos: showGraphViz "T1" tablero_a
```

^^^elm {m=(showGraphViz "T1"  tablero_a) context="1"}^^^

Para $\neg F$:

```elm {l context="1"}

tablero_na : String
tablero_na  = makeSBoard [Neg a]

-- Ejecutamos: showGraphViz "T2"  tablero_na
```

^^^elm {m=(showGraphViz "T2" tablero_na) context="1"}^^^

<u>Apartado 2</u>

De forma que los modelos de $F$ corresponden a la unión de los modelos dados por cada una de las hojas abiertas del tablero que hemos construido para $F$ y los contramodelos corresponderían a la unión de los modelos dados para $\neg F$.

¡Ojo! Las hojas abiertas proporcionan modelos, pero las cerradas **no** proporcionan contramodelos, de hecho son inconsistentes.

Los modelos que obtenemos de las hojas corresponden a:

```elm {l context="1"}

modelosTab_a : List (List FormulaLP)
modelosTab_a = modelsTab [a]

-- Ejecutamos: String.join ", " <| List.map (toLatexLPSet) modelosTab_a
```

^^^elm {m=(String.join ", " <| List.map (toLatexLPSet) modelosTab_a) context="1"}^^^


De igual modo, los contramodelos de $F$ corresponden a los modelos de $F$, de forma que:  

```elm {l context="1"}

modelosTab_na : List (List FormulaLP)
modelosTab_na = modelsTab [Neg a]

-- Ejecutamos: String.join ", " <| List.map (toLatexLPSet) modelosTab_na
```

^^^elm {m=(String.join ", " <| List.map (toLatexLPSet) modelosTab_na) context="1"}^^^

De forma que, generalizando:

```elm {l context="1"}

genModelosTab_a : List (List FormulaLP)
genModelosTab_a = generalModels <| modelosTab_a

-- Ejecutamos: String.join ", " <| List.map (toStringFLPSet) genModelosTab_a
genModelosTab_na : List (List FormulaLP)
genModelosTab_na = generalModels <| modelsTab [Neg a]

-- Ejecutamos: String.join ", " <| List.map (toLatexLPSet) genModelosTab_na
```

^^^elm {m=(String.join ", " <| List.map (toLatexLPSet) genModelosTab_na) context="1"}^^^

Nótese que en los conjuntos de literales, descriptores de modelos, el número de modelos que contienen es $2^{i}$ tal que $i$ es el número de símbolos proposicionales que no aparecen en el conjunto.

De forma que hemos obtenido 2 modelos para $F$ y ¿$(2^{1} + 2^{2} + 1 = 7)$ contramodelos? No, existen $2^{3} = 8$ valoraciones distintas, luego si hay 2 modelos necesariamente ha de haber 6 contramodelos.

Nótese que al hacer la suma hemos contado algún modelo más de una vez, ya que está incluido en varios de esos conjuntos. En concreto el modelo : $\{\neg p, \neg q, r\}$ pertenece al primer y segundo conjunto de modelos, por tanto al hacer la suma, éste lo hemos contado 2 veces.

##### Ejemplo 2
> Responde, de forma razonada, a las siguientes cuestiones usando tableros semánticos:
> 1. Es $(p \vee q \rightarrow r) \wedge (t \rightarrow \neg r) \wedge \neg(\neg t \rightarrow q) $ satisfactible?
> 2. ¿Es $(p \vee q \rightarrow r) \rightarrow (\neg r \rightarrow \neg p)$ una tautología?
> 3. ¿$\{p \rightarrow q, q \rightarrow r\} \models p \rightarrow r$?
> 4. ¿$p \wedge q \rightarrow r \equiv q \rightarrow \neg p \vee r$?

***Solución (usando Logicus)***

<u>Apartado 1</u>

Según el Teorema de Compacidad y Completitud, un conjunto de fórmulas es consistente si y sólo si posee un tablero abierto. De manera que si tomamos $U_1 = \{(p \vee q \rightarrow r) , (t \rightarrow \neg r) , \neg(\neg t \rightarrow q) \}$, (nótese que si $U_1$ es consistente entonces las trés fórmulas lo son simultáneamenete, luego $F$ lo es también). Un tablero correspondería a:

```elm {l context="2"}

import Logicus.IO_LP exposing (..)
import Logicus.SintaxSemanticsLP exposing (..)
import Logicus.SemanticBoardsLP exposing (..)
import Logicus.AuxForLitvis exposing (..)
import Bool.Extra exposing (toString)

u1 : LPSet
u1 =  List.map (\x -> fromStringToFLP x |> extractReadFLP)["(p OR q IMPLIES r)","(t IMPLIES NOT r)", "NOT (NOT t IMPLIES q)"]

tablero_u1 : String
tablero_u1  = makeSBoard u1

-- Ejecutamos: showGraphViz "T3" tablero_u1
```

^^^elm {m=(showGraphViz "T3" tablero_u1) context="2"}^^^

O equivalentemente, calculamos los modelos mediante tableros

```elm {l context="2"}

modelosTab_u1 : List (List FormulaLP)
modelosTab_u1 = modelsTab u1

-- Ejecutamos: String.join ", " <| List.map (toLatexLPSet) modelosTab_u1
```

^^^elm {m=(String.join ", " <| List.map (toLatexLPSet) modelosTab_u1) context="2"}^^^

De forma que existe al menos un modelo para $U_1$ y por tanto para $F : (p \vee q \rightarrow r) \wedge (t \rightarrow \neg r) \wedge \neg(\neg t \rightarrow q)$, luego es satisfactible.

<u>Apartado 2</u>

Teniendo en cuenta que se tiene que una fórmula es tautología si y sólo si la negación de la misma es insatisfactible, entonces para probar que $F = (p \vee q \rightarrow r) \rightarrow (\neg r \rightarrow \neg p)$ es tautología, basta comprobar que el tablero para $\neg F$ es cerrado:


```elm {l context="2"}
u2 : LPSet
u2 =  List.map (\x -> fromStringToFLP x |> extractReadFLP)["NOT ((p OR q IMPLIES r) IMPLIES (NOT r IMPLIES NOT p))"]

tablero_u2 : String
tablero_u2  = makeSBoard u2

-- Ejecutamos: showGraphViz "T4" tablero_u2
```

^^^elm {m=(showGraphViz "T4" tablero_u2) context="2"}^^^

O equivalentemente, calculamos los modelos mediante tableros, y comprobamos que no existe ninguno:

```elm {l context="2"}

modelosTab_u2 : List (List FormulaLP)
modelosTab_u2 = modelsTab u2

-- Ejecutamos: toString <| List.isEmpty <| modelosTab_u2
```

^^^elm {m=(toString <| List.isEmpty <| modelosTab_u2) context="2"}^^^

De forma que, en efecto, $\neg F$ es insatisfactible, luego $F$ es tautología

<u>Apartado 3</u>

Dado que se tiene que: $M \models F \Leftrightarrow M \cup {\neg F} \textrm{ insatisfactible}$ , entonces para comprobar si se da consecuencia lógica basta comporbar que el tablero correspondiente a $\{p \rightarrow q, q \rightarrow r, \neg (p \rightarrow r)\}$ es cerrado. De forma que:

```elm {l context="2"}
u3 : LPSet
u3 =  List.map (\x -> fromStringToFLP x |> extractReadFLP)
  ["p IMPLIES q", "q IMPLIES r", "NOT (p IMPLIES r)"]

tablero_u3 : String
tablero_u3  = makeSBoard u3

-- Ejecutamos: showGraphViz "T5" tablero_u3
```

^^^elm {m=(showGraphViz "T5" tablero_u3) context="2"}^^^

O equivalentemente, no posee modelos

```elm {l context="2"}

modelosTab_u3 : List (List FormulaLP)
modelosTab_u3 = modelsTab u3

-- Ejecutamos: toString <| List.isEmpty <| modelosTab_u3
```

^^^elm {m=(toString <| List.isEmpty <| modelosTab_u3) context="2"}^^^

Luego el conjunto es insatisfactible y por tanto sí se da la consecuencia lógica.

<u>Apartado 4</u>

Dado que se tiene que $F_1 \equiv F_2$ si y solo si los modelos de $F_1$ son exactamente los de $F_2$, o equivalentemente si $F_1 \leftrightarrow F_2$ es tautología, basta comprobar si el conjunto $\{\neg (p \wedge q \rightarrow r \leftrightarrow q \rightarrow \neg p \vee r)\}$ es inconsistente, esto es su tablero es cerrado:

```elm {l context="2"}
u4 : LPSet
u4 =  List.map (\x -> fromStringToFLP x |> extractReadFLP)
  ["NOT (p AND q IMPLIES r EQUIV q IMPLIES NOT p OR r)"]

tablero_u4 : String
tablero_u4 = makeSBoard u4

-- Ejecutamos: showGraphViz "T6" tablero_u4
```

^^^elm {m=(showGraphViz "T6" tablero_u4) context="2"}^^^

O equivalentemente, no posee modelos

```elm {l context="2"}

modelosTab_u4 : List (List FormulaLP)
modelosTab_u4 = modelsTab u4

-- Ejecutamos: toString <| List.isEmpty <| modelosTab_u4
```

^^^elm {m=(toString <| List.isEmpty <| modelosTab_u4) context="2"}^^^

Por tanto, el conjunto es, en efecto, inconsistente y por tanto sí se da la condición planteada en el problema.
