function parseClauses(indexes, cs) {

    function parseVars(v) {
        
        return v.split(",").map(function(x){ return x.charAt(0) == "¬" ? - indexes[x.substring(1)]: indexes[x];});
    }

    return cs.split(';').map((x) => parseVars(x));
    
}

function deepcopy(o) {
    return JSON.parse(JSON.stringify(o));
}

function getVariables(clauses) {
    let res = {};
    for (let c of clauses){
        for (let a of c){
            if ((Math.abs(a)) in res){
                if (a < 0){
                    res[-a][1] += 1;
                }else{
                    res[a][0] += 1;
                }
            }else{
                if (a < 0){
                    res[-a]= [0,1];
                }else{
                    res[a]= [1,0];
                }
            }
            
        }
    }
    return res;
    
}

function length(o) {
    return Object.keys(o).length;
}

function isEmpty(o){
    return Object.keys(o).length == 0;
}




function satSolver(clauses_in){

    function selectBestVar(vars){
        function entropia (a, b){
            if (a*b==0){
                return 0;
            }
            var n = a + b;
            var a_ = a*Math.log2(a);
            var b_ = b*Math.log2(b);

            return (-1/n)*(a_ + b_) + Math.log2(n);
        }

        function entropia (a, b){
            if (a*b==0){
                return 0;
            }
            var n = a + b;
            var a_ = a*Math.log2(a);
            var b_ = b*Math.log2(b);

            return (-1/n)*(a_ + b_) + Math.log2(n);
        }

        var best = null;
        var best_v = Number.POSITIVE_INFINITY;
        for (v in vars){
            var v_v = vars[v];
            var h_v = -(v_v[0] + v_v[1])
            //var h_v = entropia(v_v[0], v_v[1]);
            if (h_v < best_v){
                best = v;
                best_v = h_v;
            }else if (h_v == best_v && Math.random()>0.5){
                best = v;
                best_v = h_v;
            }
        }
        return best;
    }

    function cleanClauses (asig, clauses, av_vars, best_var, value){
        let res = [];
        let ok = parseInt(best_var);
        let contrary = - ok;
        av_vars_c = deepcopy(av_vars);
        
        if(value==0){
            contrary = parseInt(best_var);
            ok = - contrary;
           
        }

        for (let c of clauses){

            if (!c.includes(ok)){
                res.push(c.filter((x)=> x != contrary));
            }else{
                for (let v of c){
                    if (v != ok){
                        if(v < 0){
                            av_vars_c[-v][1] -= 1;
                        }else{
                            av_vars_c[v][0] -= 1;
                        }
                    }
                }
            }
        }
        return [res, av_vars_c];
    }

    function indexe(var_str){
        let ret0 = var_str.split(",");
        let ret1 = {}
        let cont = 1;
        for (var variable of var_str.split(",")){
            ret1[variable] = cont;
            cont ++;
        }
        return [ret0, ret1];
    }

    function satSolverAux(asig, av_vars, av_clauses){
        if (isEmpty(av_clauses)){
            return asig;
        }else{
            let best_var = null;
            let best_var_v= null;

            for (let c of av_clauses){
                if(length(c)==0){
                    return null;
                }else if (length(c)==1){
                    best_var = Math.abs(c[0]);
                    best_var_v = c[0] < 0 ? [0] : [1];
                }
            }

            if (best_var== null){
                best_var = selectBestVar(av_vars);
                best_var_v = [1,0];

                if (av_vars[best_var][1] > av_vars[best_var][0]){
                    best_var_v = [0,1];
                }
            }

            let av_vars_c = deepcopy(av_vars);
            delete av_vars_c[best_var];
           
            
            for (let value of best_var_v){
                asig[best_var]=value;
                var aux = cleanClauses (asig, av_clauses, av_vars_c, best_var, value);
                var solution = satSolverAux(asig, deepcopy(aux[1]), deepcopy(aux[0]));
                if (solution != null){
                    return solution;
                }
            }
            delete asig[best_var];
        }
        return null;
    }
    
    var clauses_in_aux = clauses_in.split("#");
    var indexes= indexe(clauses_in_aux[0]);
    var indexes_var_num = indexes[1];
    var indexes_num_var = indexes[0];
    var clauses = parseClauses(indexes_var_num, clauses_in_aux[1]);

    var vars = getVariables(clauses);
    console.log("Empiezo a resolverlo...");
    var solved = satSolverAux([], vars, clauses);
    if (solved != null){
        var result = "Satisfactible: true variables are [  ";
        for (v in solved){
            if (solved[v] == 1){
                result += indexes_num_var[v-1] + ", ";
            }

        }
        
        result = result.substring(0, result.length-2);
        result += "  ]";

        return result;
    } else {
        return "This problem isn't satisfactible";
    }
}